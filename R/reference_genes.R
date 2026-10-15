#' @title Compute the quality of a screen from reference genes
#' @description Measures how well a screen separates genes known to be
#'     essential from genes known to be non-essential, the standard quality
#'     check of proliferation (dropout) screens, both CRISPR and shRNA. In a
#'     good screen the essential genes are clearly more depleted than the
#'     non-essential ones. The reference genes can be the core essential and
#'     non-essential genes of Hart et al. (\code{\link{hart_reference_genes}})
#'     or any other lists.
#' @details The scores (by default the log2 fold changes) are averaged over
#'     the barcodes of each gene (\code{level = "gene"}) or used barcode by
#'     barcode (\code{level = "barcode"}). With \eqn{x_E} the scores of the
#'     essential genes and \eqn{x_N} those of the non-essential genes:
#'     \itemize{
#'       \item NNMD, the null-normalized mean difference (Hart et al. 2015):
#'             \eqn{(mean(x_E) - mean(x_N)) / sd(x_N)}. More negative is
#'             better.
#'       \item Cohen's d: the difference of the means divided by the pooled
#'             standard deviation. More negative is better.
#'       \item ROC AUC: the probability that an essential gene has a lower
#'             score than a non-essential gene (0.5: no separation, 1:
#'             perfect separation).
#'     }
#'     The reference genes not present in the table are ignored; the number
#'     of genes used is reported.
#' @param table A table with one row for each barcode and the columns Gene
#'     and score, for example the output of \code{\link{compute_metrics}} or
#'     \code{\link{compute_barcode_statistics}}.
#' @param essential The names of the essential genes (as in the Gene
#'     column).
#' @param nonessential The names of the non-essential genes.
#' @param score The column with the score (default "Log2FC").
#' @param level "gene" (default) to average the scores of the barcodes of
#'     each gene, or "barcode" to use each barcode.
#' @return A one-row tibble with the columns NEssential and NNonessential
#'     (the number of genes or barcodes used), MeanEssential,
#'     MeanNonessential, NNMD, CohenD and AUC.
#' @seealso \code{\link{scale_by_reference}},
#'     \code{\link{plot_reference_separation}},
#'     \code{\link{hart_reference_genes}}
#' @references Hart T, Chandrashekhar M, Aregger M, et al. (2015).
#'     High-resolution CRISPR screens reveal fitness genes and genotype-specific
#'     cancer liabilities. Cell 163:1515-1526. \doi{10.1016/j.cell.2015.11.015}
#' @concept compute
#' @export
#' @examples
#' data(k562_count_table)
#' data(k562_annotation_table)
#' data(hart_reference_genes)
#' samples <- data.frame(
#'     Sample = c("Plasmid", "T14_A", "T14_B"), Day = "Screen",
#'     Treatment = c("Plasmid", "T14", "T14")
#' )
#' object <- create_screenr_object(
#'     table = k562_count_table, annotation = k562_annotation_table,
#'     groups = factor(c("Plasmid", "T14", "T14")), sample_table = samples
#' )
#' object <- compute_data_table(normalize_data(object))
#' metrics <- compute_metrics(object,
#'     control = "Plasmid", treatment = "T14", day = "Screen"
#' )
#' essential <- hart_reference_genes$Gene[
#'     hart_reference_genes$Class == "core essential"]
#' nonessential <- hart_reference_genes$Gene[
#'     hart_reference_genes$Class == "non-essential"]
#' compute_screen_quality(metrics, essential, nonessential)
compute_screen_quality <- function(table, essential, nonessential,
    score = "Log2FC", level = c("gene", "barcode")) {
    level <- match.arg(level)
    values <- reference_scores(table, essential, nonessential, score, level)
    ess <- values$essential
    non <- values$nonessential
    pooled_sd <- sqrt(((length(ess) - 1) * stats::var(ess) +
        (length(non) - 1) * stats::var(non)) /
        (length(ess) + length(non) - 2))
    # Mann-Whitney estimate of P(essential score < non-essential score)
    ranks <- rank(c(ess, non))
    auc <- 1 - (sum(ranks[seq_along(ess)]) - length(ess) *
        (length(ess) + 1) / 2) / (length(ess) * length(non))
    tibble::tibble(
        NEssential = length(ess), NNonessential = length(non),
        MeanEssential = mean(ess), MeanNonessential = mean(non),
        NNMD = (mean(ess) - mean(non)) / stats::sd(non),
        CohenD = (mean(ess) - mean(non)) / pooled_sd,
        AUC = auc
    )
}

#' @title Scale scores to reference genes
#' @description Rescales the scores of a screen so that the median of the
#'     non-essential genes is 0 and the median of the essential genes is -1,
#'     as done for example by BAGEL2 and Chronos. Scaled scores can be
#'     compared between screens with different strength (for example
#'     different cell lines or time points): a scaled log2 fold change of -1
#'     means "as depleted as a typical essential gene".
#' @details With \eqn{m_N} and \eqn{m_E} the medians of the scores of the
#'     non-essential and essential genes (gene-level means of the barcode
#'     scores), the scaled score of each barcode is
#'     \eqn{(x - m_N) / (m_N - m_E)}.
#' @inheritParams compute_screen_quality
#' @return The table with a new column named "Scaled" followed by the name
#'     of the score column (for example ScaledLog2FC).
#' @seealso \code{\link{compute_screen_quality}},
#'     \code{\link{hart_reference_genes}}
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' metrics <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3", day = "Time3"
#' )
#' # Example only: two arbitrary sets of genes used as references
#' genes <- unique(as.character(metrics$Gene))
#' scaled <- scale_by_reference(metrics,
#'     essential = genes[1:20], nonessential = genes[21:60]
#' )
#' head(scaled$ScaledLog2FC)
scale_by_reference <- function(table, essential, nonessential,
    score = "Log2FC") {
    values <- reference_scores(table, essential, nonessential, score,
        level = "gene")
    median_ess <- stats::median(values$essential)
    median_non <- stats::median(values$nonessential)
    if (median_non - median_ess <= 0) {
        warning("The essential genes are not more depleted than the ",
            "non-essential genes: the scaled scores are not meaningful.",
            call. = FALSE)
    }
    table[[paste0("Scaled", score)]] <- (table[[score]] - median_non) /
        (median_non - median_ess)
    table
}

#' @title Plot the separation of reference genes
#' @description Plots the distribution of the scores (by default the log2
#'     fold changes) of the essential and non-essential reference genes, with
#'     the quality metrics of \code{\link{compute_screen_quality}} in the
#'     subtitle. In a good dropout screen the two distributions are well
#'     separated and the essential genes are on the left.
#' @inheritParams compute_screen_quality
#' @param alpha The transparency of the densities.
#' @return A ggplot object.
#' @seealso \code{\link{compute_screen_quality}}
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' metrics <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3", day = "Time3"
#' )
#' # Example only: two arbitrary sets of genes used as references
#' genes <- unique(as.character(metrics$Gene))
#' plot_reference_separation(metrics,
#'     essential = genes[1:20], nonessential = genes[21:60]
#' )
plot_reference_separation <- function(table, essential, nonessential,
    score = "Log2FC", level = c("gene", "barcode"), alpha = 0.5) {
    level <- match.arg(level)
    values <- reference_scores(table, essential, nonessential, score, level)
    quality <- compute_screen_quality(table, essential, nonessential, score,
        level)
    data <- data.frame(
        Score = c(values$essential, values$nonessential),
        Class = factor(rep(c("Essential", "Non-essential"),
            c(length(values$essential), length(values$nonessential))),
            levels = c("Essential", "Non-essential"))
    )
    ggplot2::ggplot(data, ggplot2::aes(.data$Score, fill = .data$Class)) +
        ggplot2::geom_density(alpha = alpha) +
        ggplot2::scale_fill_manual(
            values = c(Essential = "#C9836A", "Non-essential" = "#93B5A3")
        ) +
        ggplot2::labs(
            x = paste0(score, if (level == "gene") " (gene mean)" else ""),
            y = "Density", fill = NULL,
            subtitle = sprintf("NNMD %.2f, Cohen's d %.2f, ROC AUC %.3f",
                quality$NNMD, quality$CohenD, quality$AUC)
        )
}

#' @title Scores of the Reference Genes
#' @description Returns the scores of the essential and non-essential genes,
#'     averaged by gene or barcode by barcode.
#' @inheritParams compute_screen_quality
#' @return A list with the numeric vectors essential and nonessential.
#' @keywords internal
reference_scores <- function(table, essential, nonessential, score, level) {
    for (column in c("Gene", score)) {
        if (!column %in% colnames(table)) {
            stop("The table has no '", column, "' column.", call. = FALSE)
        }
    }
    genes <- as.character(table$Gene)
    values <- table[[score]]
    keep <- is.finite(values)
    genes <- genes[keep]
    values <- values[keep]
    if (level == "gene") {
        means <- tapply(values, genes, mean)
        genes <- names(means)
        values <- as.numeric(means)
    }
    ess <- values[genes %in% essential]
    non <- values[genes %in% nonessential]
    if (length(ess) < 2 || length(non) < 2) {
        stop("At least two essential and two non-essential genes must be ",
            "in the table (found ", length(ess), " and ", length(non), ").",
            call. = FALSE)
    }
    list(essential = ess, nonessential = non)
}
