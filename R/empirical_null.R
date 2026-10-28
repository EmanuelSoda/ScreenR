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
#' @param empirical_fit "central", "one-sided" or "mixture".
#' @param seed Optional seed for mixture cross-fitting.
#' @param n_folds Number of mixture cross-fitting folds (default 5).
#' @return A tibble with the columns Gene, NBarcodes, Score, Log2FC,
#'     Statistic and PValue, with the attribute "null" (mean and sd of the
#'     fitted null), or per-fold mixture fits and diagnostics.
#' @importFrom stats median mad density lm pnorm qnorm coef
#' @keywords internal
empirical_null_statistics <- function(table, score = "Zscore",
    direction = "Down", statistic = "mean", min_barcodes = 3,
    empirical_fit = "central", seed = NULL, n_folds = 5) {
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

    if (empirical_fit == "mixture") {
        x <- log(gene_table$NBarcodes)
        x <- x - mean(x)
        null <- cross_fit_mixture_null(gene_table$Statistic, x,
            n_folds = n_folds, seed = seed)
        null$genes <- gene_table$Gene
        null$x <- x
        gene_table$PValue <- if (direction == "Down") {
            null$p_down
        } else {
            null$p_up
        }
        null$p_down <- null$p_up <- NULL
        attr(gene_table, "null") <- null
        return(gene_table)
    }

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
#' @param empirical_fit "central" (default), "one-sided" or "mixture".
#' @param seed Optional seed for reproducible mixture folds. The session
#'     random state is restored afterwards.
#' @param n_folds Number of mixture cross-fitting folds (default 5). Using
#'     1 is an in-sample diagnostic and risks anti-conservative p-values.
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
    empirical_fit = c("central", "one-sided", "mixture"), bins = 60,
    seed = NULL, n_folds = 5) {
    score <- match.arg(score)
    direction <- match.arg(direction)
    statistic <- match.arg(statistic)
    empirical_fit <- match.arg(empirical_fit)
    genes <- empirical_null_statistics(table_treate_vs_control, score = score,
        direction = direction, statistic = statistic,
        min_barcodes = min_barcodes, empirical_fit = empirical_fit,
        seed = seed, n_folds = n_folds)
    null <- attr(genes, "null")
    if (empirical_fit == "mixture") {
        return(plot_mixture_null(genes, null, bins))
    }
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

#' @title Cross-fit the mixture empirical null
#' @description Fits each null on the genes outside its fold and evaluates
#'     both neutral tails on the held-out genes. The random state is restored
#'     even when seed is NULL, as the legacy empirical test uses no randomness.
#' @param statistic Numeric vector of gene statistics.
#' @param x Numeric vector of centred log barcode counts.
#' @param n_folds Number of folds; 1 means an in-sample diagnostic fit.
#' @param seed Optional random seed.
#' @return A list with fits, fold assignment, both p-value tails and diagnostics.
#' @keywords internal
cross_fit_mixture_null <- function(statistic, x, n_folds = 5, seed = NULL) {
    n <- length(statistic)
    if (!is.numeric(statistic) || !is.numeric(x) || length(x) != n ||
        n < 3 || any(!is.finite(statistic)) || any(!is.finite(x))) {
        stop("statistic and x must be finite numeric vectors of equal ",
            "length, with at least 3 genes.")
    }
    if (!is.numeric(n_folds) || length(n_folds) != 1 ||
        !is.finite(n_folds) || n_folds != floor(n_folds) ||
        n_folds < 1 || n_folds > n) {
        stop("n_folds must be a single integer between 1 and the ",
            "number of tested genes.")
    }
    restore_random_state()
    if (!is.null(seed)) {
        set.seed(seed)
    }
    fold <- if (n_folds == 1) rep(1L, n) else {
        sample(rep(seq_len(n_folds), length.out = n))
    }
    fits <- vector("list", n_folds)
    p_down <- p_up <- numeric(n)
    for (k in seq_len(n_folds)) {
        held_out <- fold == k
        training <- if (n_folds == 1) rep(TRUE, n) else !held_out
        fit <- mixture_null_fit(statistic[training], x[training])
        if (fit$convergence != 0 || isTRUE(fit$fallback)) {
            warning("Mixture null fold ", k, ": convergence = ",
                fit$convergence, ", fallback = ", fit$fallback, ". ",
                fit$message)
        }
        fits[[k]] <- fit
        p_down[held_out] <- mixture_null_cdf(fit, statistic[held_out],
            x[held_out], lower.tail = TRUE)
        p_up[held_out] <- mixture_null_cdf(fit, statistic[held_out],
            x[held_out], lower.tail = FALSE)
    }
    per_fold <- do.call(rbind, lapply(seq_len(n_folds), function(k) {
        fit <- fits[[k]]
        data.frame(fold = k, n_train = fit$n, n_test = sum(fold == k),
            convergence = fit$convergence, fallback = fit$fallback,
            starts_converged = fit$starts_converged,
            mu0 = unname(fit$theta["mu0"]),
            s = unname(fit$theta["s"]),
            pi0 = unname(fit$weights["null"]),
            pi_down = unname(fit$weights["down"]),
            pi_up = unname(fit$weights["up"]),
            at_bound = paste(fit$at_bound, collapse = ", "),
            message = paste(fit$message, collapse = "; "))
    }))
    spread <- if (n_folds == 1) c(mu0 = 0, s = 0) else {
        c(mu0 = stats::sd(per_fold$mu0), s = stats::sd(per_fold$s))
    }
    list(fits = fits, fold = fold, p_down = p_down, p_up = p_up,
        diagnostics = list(per_fold = per_fold, spread = spread))
}

#' @title Plot the cross-fitted mixture null
#' @description Averages the neutral density over held-out covariates and
#'     folds, weighted by each fitted null proportion.
#' @param genes Gene statistics returned by empirical_null_statistics.
#' @param null The mixture null attribute of genes.
#' @param bins Number of histogram bins.
#' @return A ggplot object.
#' @keywords internal
plot_mixture_null <- function(genes, null, bins) {
    grid <- seq(min(genes$Statistic), max(genes$Statistic), length.out = 512)
    y <- numeric(length(grid))
    for (k in seq_along(null$fits)) {
        fit <- null$fits[[k]]
        x <- null$x[null$fold == k]
        mu <- fit$theta[["mu0"]] + fit$theta[["beta"]] * x
        sigma <- exp(fit$theta[["s"]] + fit$theta[["gamma"]] * x)
        y <- y + fit$weights[["null"]] * vapply(grid, function(t) {
            sum(stats::dnorm(t, mean = mu, sd = sigma))
        }, numeric(1)) / nrow(genes)
    }
    curve <- data.frame(x = grid, y = y)
    diagnostics <- null$diagnostics$per_fold
    ggplot2::ggplot(genes, aes(x = .data$Statistic)) +
        ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)),
            bins = bins, fill = "grey70", colour = "grey40") +
        ggplot2::geom_line(data = curve, aes(x = .data$x, y = .data$y),
            colour = "#b2182b", linewidth = 0.8) +
        ggplot2::labs(x = "Gene statistic", y = "Density",
            subtitle = sprintf(
                "Mixture empirical null: %d folds, mean null share %.0f%%",
                length(null$fits), 100 * stats::weighted.mean(diagnostics$pi0,
                    diagnostics$n_test)))
}
