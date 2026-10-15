#' @title Plot the barcodes of a gene (barcode plot)
#' @description Draws a barcode plot for a gene, as
#'     \code{\link[limma]{barcodeplot}} but with ggplot2: all the barcodes
#'     are ranked by their log2 fold change (computed with edgeR for the
#'     given contrast) and the barcodes of the gene are shown as vertical
#'     bars. If most of the bars are on the left (negative log2 fold change)
#'     the gene is depleted, if they are on the right it is enriched. It is
#'     useful to check the hits of \code{\link{find_camera_hit}} and
#'     \code{\link{find_roast_hit}}.
#' @details The plot has up to three panels, which share the ranking of the
#'     barcodes on the x axis:
#'     \itemize{
#'       \item the enrichment line: the density of the barcodes of the gene
#'             along the ranking divided by their average density (computed
#'             as in \code{\link[limma]{barcodeplot}}, with a tricube moving
#'             average). Values above 1 (dashed line) mean that the barcodes
#'             of the gene are concentrated there;
#'       \item the barcodes of the gene as vertical bars, colored by their
#'             log2 fold change, on three regions: below, between and above
#'             the two values of quantile;
#'       \item (when show_log2fc = TRUE) the log2 fold change of all the
#'             barcodes, in order, with the barcodes of the gene as points.
#'             It shows how strong the effect is, which the ranks alone do
#'             not.
#'     }
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @param contrast The comparison to plot: the name or index of a column of
#'     matrix_model, a numeric contrast vector (for example created with
#'     \code{\link{create_contrast}}) or a one-column matrix created with
#'     \code{\link[limma]{makeContrasts}}.
#' @param number_barcode The gene must have more than number_barcode barcodes
#'     (default 3), as in \code{\link{find_camera_hit}}.
#' @param quantile The two log2 fold change values that delimit the central
#'     (grey) region of the plot. Default c(-0.5, 0.5).
#' @param labels The labels of the left and right ends of the plot.
#' @param gene The name of the gene to plot.
#' @param show_log2fc Whether to add the panel with the log2 fold change of
#'     all the barcodes (default TRUE).
#' @concept  plot
#' @return A ggplot object (a patchwork of the panels), which can be printed,
#'     saved with \code{\link[ggplot2]{ggsave}} or combined with other
#'     plots. No theme is applied: add one to all the panels with \code{&},
#'     for example \code{plot_barcode_hit(...) & ggplot2::theme_classic()}.
#' @seealso \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{plot_barcode_trend}}
#' @importFrom edgeR estimateDisp
#' @importFrom edgeR glmFit
#' @importFrom edgeR glmLRT
#' @importFrom limma tricubeMovingAverage
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- create_design_matrix(object)
#' contrast <- create_contrast(matrix_model,
#'     treatment = "Treated", control = "Control"
#' )
#'
#' plot_barcode_hit(object, matrix_model,
#'     contrast = contrast,
#'     gene = "Gene_300"
#' )
#'
#' # Without the panel of the log2 fold changes
#' plot_barcode_hit(object, matrix_model,
#'     contrast = contrast,
#'     gene = "Gene_300", show_log2fc = FALSE
#' )
plot_barcode_hit <- function(screenR_Object, matrix_model,
    contrast, number_barcode = 3, gene,
    quantile = c(-0.5, 0.5), labels = c("Negative logFC", "Positive logFC"),
    show_log2fc = TRUE) {
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, design = matrix_model)
    log2fc <- barcode_log2fc(xglm, matrix_model, contrast)

    genesymbollist <- unique_gene_symbols(DGEList$genes$Gene, number_barcode)
    if (is.null(genesymbollist[[gene]])) {
        stop("Gene ", gene, " is not in the annotation or does not have ",
            "more than number_barcode = ", number_barcode, " barcodes.")
    }
    in_gene <- seq_along(log2fc) %in% genesymbollist[[gene]]
    barcode_plot(log2fc, in_gene, title = gene, quantile = quantile,
        labels = labels, show_log2fc = show_log2fc)
}

#' @title Draw a barcode plot
#' @description Internal function that draws the barcode plot of
#'     \code{\link{plot_barcode_hit}} from the log2 fold changes of all the
#'     barcodes and the barcodes of the gene.
#' @param log2fc The log2 fold change of each barcode.
#' @param in_gene A logical vector, TRUE for the barcodes of the gene.
#' @param title The title of the plot.
#' @param quantile The two log2 fold change values that delimit the central
#'     region.
#' @param labels The labels of the left and right ends of the plot.
#' @param show_log2fc Whether to add the panel with the log2 fold changes.
#' @param span The span of the tricube moving average of the enrichment line
#'     (0.45, as in \code{\link[limma]{barcodeplot}}).
#' @return A patchwork object.
#' @keywords internal
barcode_plot <- function(log2fc, in_gene, title, quantile, labels,
    show_log2fc, span = 0.45) {
    ord <- order(log2fc)
    data <- data.frame(Rank = seq_along(ord), Log2FC = log2fc[ord],
        InGene = in_gene[ord])
    n <- nrow(data)
    # Density of the barcodes of the gene along the ranking, relative to
    # their average density, as in limma::barcodeplot
    data$Enrichment <- limma::tricubeMovingAverage(
        as.numeric(data$InGene), span = span) / mean(data$InGene)
    cut <- findInterval(sort(quantile), data$Log2FC)
    regions <- data.frame(
        xmin = c(0, cut) + 0.5, xmax = c(cut, n) + 0.5,
        Region = factor(c("low", "mid", "high"), c("low", "mid", "high"))
    )
    gene_barcodes <- data[data$InGene, ]
    region_fill <- ggplot2::scale_fill_manual(
        values = c(low = "#cfe0f3", mid = "grey92", high = "#f6d5d5"),
        guide = "none")
    barcode_colour <- ggplot2::scale_colour_gradient2(low = "#2166ac",
        mid = "grey40", high = "#b2182b", midpoint = 0, guide = "none")
    # The panels share the x axis: only the bottom one has its title. The
    # rank numbers are not shown. The axes are set with the scales, so that
    # the theme is left to the user
    x_scale <- function(show) {
        ggplot2::scale_x_continuous(limits = c(0.5, n + 0.5),
            expand = c(0, 0), breaks = NULL,
            name = if (show) "Barcodes ranked by log2 fold change" else NULL)
    }

    worm <- ggplot2::ggplot(data, aes(x = .data$Rank,
        y = .data$Enrichment)) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
            colour = "grey50") +
        ggplot2::geom_line(colour = "#b2182b", linewidth = 0.8) +
        x_scale(FALSE) + ggplot2::labs(y = "Enrichment")

    strip <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = regions, aes(xmin = .data$xmin,
            xmax = .data$xmax, ymin = 0, ymax = 1, fill = .data$Region)) +
        ggplot2::geom_segment(data = gene_barcodes, aes(x = .data$Rank,
            xend = .data$Rank, y = 0, yend = 1, colour = .data$Log2FC),
            linewidth = 0.9) +
        ggplot2::annotate("label", x = n * 0.01, y = 0.5, label = labels[1],
            hjust = 0, size = 3.2, fontface = "bold", fill = "white",
            alpha = 0.85) +
        ggplot2::annotate("label", x = n * 0.99, y = 0.5, label = labels[2],
            hjust = 1, size = 3.2, fontface = "bold", fill = "white",
            alpha = 0.85) +
        region_fill + barcode_colour + x_scale(!show_log2fc) +
        ggplot2::scale_y_continuous(expand = c(0, 0), breaks = NULL,
            name = NULL)

    subtitle <- sprintf("%d barcodes, median log2FC %.2f",
        nrow(gene_barcodes), stats::median(gene_barcodes$Log2FC))
    if (!show_log2fc) {
        return(patchwork::wrap_plots(worm, strip, ncol = 1,
            heights = c(1.2, 0.5)) +
            patchwork::plot_annotation(title = title, subtitle = subtitle))
    }

    curve <- ggplot2::ggplot(data, aes(x = .data$Rank, y = .data$Log2FC)) +
        ggplot2::geom_rect(data = regions, aes(xmin = .data$xmin,
            xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$Region),
            inherit.aes = FALSE) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
        ggplot2::geom_line(colour = "grey30") +
        ggplot2::geom_point(data = gene_barcodes,
            aes(colour = .data$Log2FC), size = 2.4) +
        region_fill + barcode_colour + x_scale(TRUE) +
        ggplot2::labs(y = "Log2FC")

    patchwork::wrap_plots(worm, strip, curve, ncol = 1,
        heights = c(1.2, 0.5, 1.6)) +
        patchwork::plot_annotation(title = title, subtitle = subtitle)
}
