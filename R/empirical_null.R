#' @title Gene statistics and their empirical null
#' @description Internal function used by \code{\link{find_gene_zscore_hit}}
#'     (reference = "empirical") and \code{\link{plot_empirical_null}}. It
#'     computes the score of each gene (mean or median of the scores of its
#'     barcodes), standardizes it into a statistic, fits the empirical null
#'     of the statistics and computes the p-values.
#' @param table A table with one row for each barcode and the columns Gene,
#'     Log2FC and score.
#' @param score The column with the barcode scores.
#' @param direction "Down" or "Up".
#' @param statistic "mean" or "median".
#' @param min_barcodes Genes with fewer barcodes are not tested.
#' @param empirical_fit "central" or "one-sided".
#' @return A tibble with the columns Gene, NBarcodes, Score, Log2FC,
#'     Statistic and PValue, with the attribute "null" (mean and sd of the
#'     fitted null).
#' @importFrom stats median mad density lm pnorm qnorm coef
#' @keywords internal
empirical_null_statistics <- function(table, score = "Zscore",
    direction = "Down", statistic = "mean", min_barcodes = 3,
    empirical_fit = "central") {
    missing_columns <- setdiff(c("Gene", "Log2FC", score), colnames(table))
    if (length(missing_columns) > 0) {
        stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
    }
    table <- dplyr::ungroup(table)
    table <- dplyr::filter(table, !is.na(.data$Gene), !is.na(.data[[score]]))
    table$Gene <- as.character(table$Gene)

    summarise_score <- if (statistic == "mean") mean else stats::median
    gene_table <- table %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(
            NBarcodes = dplyr::n(),
            Score = summarise_score(.data[[score]]),
            Log2FC = stats::median(.data$Log2FC),
            .groups = "drop"
        )
    check_barcode_threshold(gene_table$NBarcodes, min_barcodes,
        strict = FALSE, argument = "min_barcodes")
    gene_table <- dplyr::filter(gene_table, .data$NBarcodes >= min_barcodes)
    if (nrow(gene_table) < 100) {
        warning("Only ", nrow(gene_table), " genes are tested: the ",
            "empirical null needs hundreds of genes to be estimated well.")
    }

    # Standardize: a gene of k barcodes taken at random would have a
    # statistic close to N(0, 1); the empirical null corrects the centre and
    # the spread of the genes without effect
    centre <- stats::median(table[[score]])
    spread <- stats::mad(table[[score]])
    gene_table$Statistic <- (gene_table$Score - centre) /
        (spread / sqrt(gene_table$NBarcodes))

    null <- if (empirical_fit == "central") {
        fit_null_central(gene_table$Statistic)
    } else {
        fit_null_one_sided(gene_table$Statistic,
            side = if (direction == "Down") "right" else "left")
    }
    z <- (gene_table$Statistic - null[["mean"]]) / null[["sd"]]
    gene_table$PValue <- stats::pnorm(z, lower.tail = direction == "Down")
    attr(gene_table, "null") <- null
    gene_table
}

#' @title Fit the empirical null by central matching
#' @description Internal function: near its mode the log density of a normal
#'     distribution is a parabola. The parabola is fitted to the log density
#'     of x within 1.5 median absolute deviations of the mode, where the hits
#'     are rare, and gives the mean and the standard deviation of the null
#'     (central matching, Efron 2004). When the fit fails (the parabola does
#'     not open downwards), the median and the median absolute deviation are
#'     used.
#' @param x The gene statistics.
#' @param width The half-width of the fitted region, in median absolute
#'     deviations.
#' @return A named vector with mean and sd.
#' @keywords internal
fit_null_central <- function(x, width = 1.5) {
    d <- stats::density(x, n = 1024)
    mode <- d$x[which.max(d$y)]
    keep <- abs(d$x - mode) <= width * stats::mad(x) & d$y > 0
    centre_x <- d$x[keep]
    fit <- stats::lm(log(d$y[keep]) ~ centre_x + I(centre_x^2))
    b <- stats::coef(fit)[[2]]
    a2 <- stats::coef(fit)[[3]]
    if (!is.finite(a2) || a2 >= 0) {
        return(c(mean = stats::median(x), sd = stats::mad(x)))
    }
    c(mean = -b / (2 * a2), sd = sqrt(-1 / (2 * a2)))
}

#' @title Fit the empirical null on the side without hits
#' @description Internal function: the centre of the null is the mode of x
#'     and its spread is estimated from the values on the side without hits
#'     only (for a normal distribution the median distance from the mean is
#'     0.674 standard deviations).
#' @param x The gene statistics.
#' @param side "right" (the hits are depleted, on the left) or "left".
#' @return A named vector with mean and sd.
#' @keywords internal
fit_null_one_sided <- function(x, side = "right") {
    d <- stats::density(x, n = 2048)
    mode <- d$x[which.max(d$y)]
    distance <- if (side == "right") x[x > mode] - mode else mode - x[x < mode]
    c(mean = mode, sd = stats::median(distance) / stats::qnorm(0.75))
}

#' @title Plot the empirical null of the gene statistics
#' @description Plots the distribution of the gene statistics used by
#'     \code{\link{find_gene_zscore_hit}} with reference = "empirical",
#'     together with the fitted null (scaled to the share of genes it
#'     describes). The null should follow the centre of the distribution; the
#'     hits are in the tail on the side of direction.
#' @param table_treate_vs_control A table with one row for each barcode and
#'     at least the columns Gene, Log2FC and the score column, as returned by
#'     \code{\link{compute_metrics}} or
#'     \code{\link{compute_barcode_statistics}}.
#' @param score The column to use as score: "Zscore" (default) or
#'     "ZscoreRobust".
#' @param direction "Down" (default) or "Up".
#' @param statistic The gene score: "mean" (default) or "median".
#' @param min_barcodes Genes with fewer barcodes than this are not used
#'     (default 3).
#' @param empirical_fit "central" (default) or "one-sided".
#' @param bins The number of bins of the histogram. Default 60.
#' @return A ggplot object.
#' @seealso \code{\link{find_gene_zscore_hit}}
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "Time3", treatment = "TRT",
#'     day = "Time3"
#' )
#' plot_empirical_null(table)
plot_empirical_null <- function(table_treate_vs_control,
    score = c("Zscore", "ZscoreRobust"), direction = c("Down", "Up"),
    statistic = c("mean", "median"), min_barcodes = 3,
    empirical_fit = c("central", "one-sided"), bins = 60) {
    score <- match.arg(score)
    direction <- match.arg(direction)
    statistic <- match.arg(statistic)
    empirical_fit <- match.arg(empirical_fit)
    genes <- empirical_null_statistics(table_treate_vs_control, score = score,
        direction = direction, statistic = statistic,
        min_barcodes = min_barcodes, empirical_fit = empirical_fit)
    null <- attr(genes, "null")
    # Share of genes described by the null: the density of the statistics at
    # the centre of the null, relative to that of the null
    density_at_mode <- stats::approx(
        stats::density(genes$Statistic, n = 1024)[c("x", "y")],
        xout = null[["mean"]])$y
    share <- min(1, density_at_mode / stats::dnorm(0, sd = null[["sd"]]))
    grid <- seq(min(genes$Statistic), max(genes$Statistic), length.out = 512)
    curve <- data.frame(x = grid,
        y = share * stats::dnorm(grid, null[["mean"]], null[["sd"]]))

    ggplot2::ggplot(genes, aes(x = .data$Statistic)) +
        ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)),
            bins = bins, fill = "grey70", colour = "grey40") +
        ggplot2::geom_line(data = curve, aes(x = .data$x, y = .data$y),
            colour = "#b2182b", linewidth = 0.8) +
        ggplot2::labs(x = "Gene statistic", y = "Density",
            subtitle = sprintf(
                "Empirical null: mean %.2f, sd %.2f (%.0f%% of the genes)",
                null[["mean"]], null[["sd"]], 100 * share))
}
