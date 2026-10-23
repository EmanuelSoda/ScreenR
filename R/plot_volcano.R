#' @title Volcano plot of the genes
#' @description Shows every tested gene with its effect (the mean log2 fold
#'     change of its barcodes, x axis) and its significance (-log10 FDR, y
#'     axis), so that the hits stand out from the other genes. It works with
#'     the table of a single method (\code{\link{find_roast_hit}},
#'     \code{\link{find_camera_hit}}, \code{\link{find_gene_zscore_hit}}) or
#'     with the combined table of \code{\link{summarise_hits}}.
#' @details The FDR is taken from \code{fdr_column}: by default MaxFDR when
#'     the table has it (\code{\link{summarise_hits}}: the largest FDR of the
#'     methods, so a gene is above the dashed line only when every method
#'     finds it) and FDR otherwise. The effect is the Log2FC column of the
#'     table (the median log2 fold change of the barcodes for FRY, ROAST and
#'     CAMERA, the mean for the Z-score test). The table of
#'     \code{\link{summarise_hits}} has none: give then the barcode
#'     statistics with \code{statistics}, and the mean log2 fold change of the
#'     barcodes of each gene is used (also with the other tables, when
#'     \code{statistics} is given).
#'
#'     Genes without an FDR (for example genes not tested by every method)
#'     are not shown. FDRs of 0 are drawn at the smallest positive FDR of the
#'     table. No theme is applied; choose the appearance with
#'     \code{ggplot2::theme_set()}.
#' @param table A table with one row for each gene and the columns Gene, the
#'     FDR (see \code{fdr_column}) and Log2FC (unless \code{statistics} is
#'     given). Computed with \code{fdr = 1}, so that it contains all the
#'     tested genes.
#' @param statistics Optional. The barcode statistics of the comparison
#'     (\code{\link{compute_barcode_statistics}}), with the columns Gene and
#'     Log2FC: the mean log2 fold change of the barcodes of each gene is used
#'     as its effect. Needed when \code{table} has no Log2FC column.
#' @param fdr The FDR threshold that defines the hits (default 0.05), drawn
#'     as a dashed line.
#' @param fdr_column The column with the FDR. By default "MaxFDR" when the
#'     table has it, otherwise "FDR".
#' @param labels The number of hits (the most significant) labelled with
#'     their name (default 10, 0 for none).
#' @param genes Optional names of genes to highlight with a circle and a
#'     label, whether they are hits or not.
#' @param colors The colours of the hits and of the other genes.
#' @param label_color The colour of the labels and of the circles of the
#'     highlighted genes (default "black").
#' @param alpha The transparency of the points (default 0.7).
#' @param point_size The size of the points (default 1.8).
#' @param label_size The size of the labels (default 3.5).
#' @return A ggplot object.
#' @seealso \code{\link{summarise_hits}}, \code{\link{plot_common_hit}},
#'     \code{\link{plot_barcode_hit}}
#' @concept plot
#' @export
#' @examples
#' data(count_table)
#' data(annotation_table)
#' # The unmapped reads "*" appear several times: remove them
#' counts <- count_table[count_table$Barcode != "*", ]
#' groups <- factor(c(
#'     "T1/T2", "T1/T2", "Time3_TRT", "Time3_TRT", "Time3_TRT",
#'     "Time3_CTRL", "Time3_CTRL", "Time3_CTRL", "Time4_TRT", "Time4_TRT",
#'     "Time4_TRT", "Time4_CTRL", "Time4_CTRL", "Time4_CTRL"
#' ))
#' object <- create_screenr_object(
#'     table = counts, annotation = annotation_table, groups = groups
#' )
#' object <- normalize_data(object)
#' design <- create_design_matrix(object)
#' contrast <- create_contrast(design,
#'     treatment = "Time4_TRT", control = "Time4_CTRL"
#' )
#'
#' # One method
#' camera <- find_camera_hit(object, design, contrast, fdr = 1)
#' plot_volcano(camera, fdr = 0.25)
#'
#' # Several methods: a gene is a hit when every method finds it
#' statistics <- compute_barcode_statistics(object, design, contrast)
#' zscore <- find_gene_zscore_hit(statistics, reference = "empirical",
#'     fdr = 1
#' )
#' hits <- summarise_hits(list(camera = camera, zscore = zscore), fdr = 0.25)
#' plot_volcano(hits, statistics = statistics, fdr = 0.25)
plot_volcano <- function(table, statistics = NULL, fdr = 0.05,
    fdr_column = NULL, labels = 10, genes = NULL,
    colors = c(Hit = "#C9836A", "Not a hit" = "#93B5A3"),
    label_color = "black", alpha = 0.7, point_size = 1.8, label_size = 3.5) {
    if (!is.data.frame(table) || !"Gene" %in% colnames(table)) {
        stop("table must be a table with a Gene column.", call. = FALSE)
    }
    if (is.null(fdr_column)) {
        fdr_column <- if ("MaxFDR" %in% colnames(table)) "MaxFDR" else "FDR"
    }
    if (!fdr_column %in% colnames(table)) {
        stop("The table has no '", fdr_column, "' column.", call. = FALSE)
    }
    if (!is.numeric(fdr) || length(fdr) != 1L || is.na(fdr) || fdr <= 0 ||
        fdr > 1) {
        stop("fdr must be a number between 0 and 1.", call. = FALSE)
    }
    if (!is.numeric(labels) || length(labels) != 1L || is.na(labels) ||
        labels < 0) {
        stop("labels must be a number of genes (0 or more).", call. = FALSE)
    }
    if (length(colors) != 2L) {
        stop("colors must have two colours: the hits and the other genes.",
            call. = FALSE)
    }
    data <- data.frame(Gene = as.character(table$Gene),
        FDR = table[[fdr_column]])
    if (!is.null(statistics)) {
        if (!all(c("Gene", "Log2FC") %in% colnames(statistics))) {
            stop("statistics must have the columns Gene and Log2FC.",
                call. = FALSE)
        }
        effect <- tapply(statistics$Log2FC, as.character(statistics$Gene),
            mean, na.rm = TRUE)
        data$Log2FC <- unname(effect[data$Gene])
    } else if ("Log2FC" %in% colnames(table)) {
        data$Log2FC <- table$Log2FC
    } else {
        stop("The table has no Log2FC column: give the barcode statistics ",
            "with statistics (see compute_barcode_statistics()).",
            call. = FALSE)
    }
    data <- data[!is.na(data$FDR) & is.finite(data$Log2FC), ]
    if (nrow(data) == 0) {
        stop("No gene has both an FDR and a log2 fold change.", call. = FALSE)
    }
    positive <- data$FDR[data$FDR > 0]
    smallest <- if (length(positive) > 0) min(positive) else 1e-300
    data$Significance <- -log10(pmax(data$FDR, smallest))
    data$Status <- factor(ifelse(data$FDR <= fdr, "Hit", "Not a hit"),
        levels = c("Hit", "Not a hit"))
    data <- data[order(data$Status, decreasing = TRUE), ]
    names(colors) <- levels(data$Status)

    hits <- data[data$Status == "Hit", ]
    labelled <- utils::head(hits[order(hits$FDR, -abs(hits$Log2FC)), ],
        labels)
    highlighted <- data[data$Gene %in% genes, ]
    labelled <- rbind(labelled, highlighted[!highlighted$Gene %in%
        labelled$Gene, ])

    ggplot2::ggplot(data, ggplot2::aes(x = .data$Log2FC,
        y = .data$Significance)) +
        ggplot2::geom_hline(yintercept = -log10(fdr), linetype = "dashed",
            colour = colors[[1]]) +
        ggplot2::geom_point(ggplot2::aes(colour = .data$Status),
            alpha = alpha, size = point_size) +
        ggplot2::geom_point(data = highlighted, shape = 21,
            size = point_size * 2.8, stroke = 1.2, colour = label_color) +
        ggplot2::geom_text(data = labelled,
            ggplot2::aes(label = .data$Gene), vjust = -0.9,
            size = label_size, fontface = "bold", colour = label_color,
            check_overlap = TRUE) +
        ggplot2::scale_colour_manual(values = colors) +
        # Room for the labels at the top and at the sides
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(
            mult = 0.08)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(
            mult = c(0.02, 0.12))) +
        ggplot2::labs(
            x = if (is.null(statistics)) {
                "Log2 fold change of the gene"
            } else {
                "Mean log2 fold change of the barcodes"
            },
            y = if (fdr_column == "MaxFDR") {
                "-log10 FDR (largest of the methods)"
            } else {
                paste("-log10", fdr_column)
            },
            colour = NULL,
            subtitle = sprintf("%d hits at %s%% FDR", nrow(hits),
                format(fdr * 100))
        )
}
