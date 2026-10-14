#' @title Compute Gini Index
#' @description Computes the Gini index of the barcode counts of each
#'              sample, a measure of how evenly the reads are distributed
#'              over the barcodes of the library. It is 0 when all the
#'              barcodes have the same number of reads and close to 1 when
#'              a few barcodes take most of the reads. A Gini index that
#'              increases strongly over time or differs between replicates
#'              can indicate a bottleneck or a technical problem.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return A tibble with the columns Sample and Gini.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' compute_gini(object)
compute_gini <- function(screenR_Object) {
    count_table <- screenR_Object@count_table
    numeric_columns <- vapply(count_table, is.numeric, logical(1))
    counts <- as.matrix(count_table[, numeric_columns])
    gini <- apply(counts, 2, function(x) {
        # as.numeric() avoids integer overflow with large counts
        x <- sort(as.numeric(x))
        n <- length(x)
        if (n == 0 || sum(x) == 0) {
            return(NA_real_)
        }
        sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
    })
    tibble::tibble(
        Sample = factor(colnames(counts), levels = colnames(counts)),
        Gini = as.numeric(gini)
    )
}

#' @title Plot Gini Index
#' @description Plots the Gini index of each sample computed with
#'              \code{\link{compute_gini}}: 0 means that all the barcodes
#'              have the same number of reads, values close to 1 that a few
#'              barcodes take most of the reads. In a dropout screen it
#'              usually increases over time; a strong increase or a large
#'              difference between replicates can signal a bottleneck.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param palette An optional vector of colors, one for each sample.
#' @param alpha The opacity of the bars.
#' @param legende_position The position of the legend ("none" by default).
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_fill_manual
#' @importFrom ggplot2 theme element_text labs scale_y_continuous expansion
#' @return A ggplot object.
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' plot_gini(object)
plot_gini <- function(screenR_Object, palette = NULL, alpha = 1,
    legende_position = "none") {
    gini <- compute_gini(screenR_Object)
    plot <- ggplot2::ggplot(gini, ggplot2::aes(
        x = .data$Sample, y = .data$Gini, fill = .data$Sample
    )) +
        ggplot2::geom_col(alpha = alpha) +
        ggplot2::geom_text(ggplot2::aes(label = round(.data$Gini, 2)),
            vjust = -0.4, size = 3
        ) +
        ggplot2::scale_y_continuous(
            limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
        ) +
        ggplot2::labs(x = NULL, y = "Gini index") +
        ggplot2::theme(
            legend.position = legende_position,
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    if (!is.null(palette)) {
        plot <- plot + ggplot2::scale_fill_manual(values = palette)
    }
    return(plot)
}

#' @title Plot Replicate Correlation
#' @description Plots the correlation between all the pairs of samples,
#'              computed on the log2 of the counts per million (plus 1).
#'              Replicates of the same condition are expected to be highly
#'              correlated; a replicate with a low correlation to the others
#'              may be an outlier.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param method The correlation method: "pearson" (default) or "spearman".
#' @param label Whether to write the correlation value in each tile.
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient
#' @importFrom ggplot2 theme element_text labs
#' @importFrom stats cor
#' @return A ggplot object.
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' plot_replicate_correlation(object)
plot_replicate_correlation <- function(screenR_Object,
    method = c("pearson", "spearman"), label = TRUE) {
    method <- match.arg(method)
    count_table <- screenR_Object@count_table
    numeric_columns <- vapply(count_table, is.numeric, logical(1))
    counts <- as.matrix(count_table[, numeric_columns])
    log_cpm <- log2(sweep(counts, 2, colSums(counts), "/") * 1e6 + 1)
    correlation <- stats::cor(log_cpm, method = method)

    samples <- colnames(correlation)
    data <- tibble::tibble(
        Sample1 = factor(rep(samples, times = length(samples)),
            levels = samples
        ),
        Sample2 = factor(rep(samples, each = length(samples)),
            levels = rev(samples)
        ),
        Correlation = as.numeric(correlation)
    )

    plot <- ggplot2::ggplot(data, ggplot2::aes(
        x = .data$Sample1, y = .data$Sample2, fill = .data$Correlation
    )) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient(low = "white", high = "#2166AC") +
        ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 45, hjust = 1
        ))
    if (label) {
        plot <- plot + ggplot2::geom_text(ggplot2::aes(
            label = format(round(.data$Correlation, 2), nsmall = 2)
        ), size = 2.5)
    }
    return(plot)
}

#' @title Find Discordant Barcodes
#' @description Finds the barcodes whose log2 fold change is far from the
#'              other barcodes of the same gene. For each gene it computes
#'              the median and the median absolute deviation (MAD) of the
#'              log2 fold changes of its barcodes; a barcode is discordant
#'              when its distance from the median is larger than threshold
#'              MADs. Discordant barcodes may have off-target effects or a
#'              poor knockdown, and can drive a gene to be (or not to be) a
#'              hit.
#' @param table_treate_vs_control A table with one row for each barcode and
#'                                at least the columns Barcode, Gene and
#'                                Log2FC, as returned by
#'                                \code{\link{compute_metrics}} or
#'                                \code{\link{compute_barcode_statistics}}.
#' @param threshold The number of MADs from the gene median beyond which a
#'                  barcode is discordant.
#' @importFrom stats median mad
#' @return A tibble with the discordant barcodes and the columns Barcode,
#'         Gene, Log2FC, GeneLog2FC (median of the gene) and Deviation
#'         (distance from the gene median in MADs), ordered by the absolute
#'         deviation.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3",
#'     day = "Time3"
#' )
#' find_discordant_barcodes(table)
find_discordant_barcodes <- function(table_treate_vs_control, threshold = 3) {
    table <- dplyr::ungroup(table_treate_vs_control)
    missing_columns <- setdiff(c("Barcode", "Gene", "Log2FC"), colnames(table))
    if (length(missing_columns) > 0) {
        stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
    }
    discordant <- table %>%
        dplyr::filter(!is.na(.data$Gene)) %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::mutate(
            GeneLog2FC = stats::median(.data$Log2FC),
            Deviation = (.data$Log2FC - .data$GeneLog2FC) /
                stats::mad(.data$Log2FC)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(is.finite(.data$Deviation),
            abs(.data$Deviation) > threshold) %>%
        dplyr::select("Barcode", "Gene", "Log2FC", "GeneLog2FC",
            "Deviation") %>%
        dplyr::arrange(dplyr::desc(abs(.data$Deviation)))
    return(discordant)
}
