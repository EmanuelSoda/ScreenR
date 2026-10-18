#' Plot the separation between two screening conditions
#'
#' Shows the log2 ratio of treatment to control mean normalized abundance for
#' selected genes. Each biological sample is first summarized over its barcodes,
#' then samples are averaged within condition and time point.
#' @param screenR_Object A normalized ScreenR object.
#' @param genes Gene identifiers to show, in panel order.
#' @param treatment,control The two distinct values of condition_column to compare.
#' @param condition_column The colData variable identifying conditions (default
#'     "Treatment"). Names such as CTRL or TRT are not required.
#' @param time_column The colData variable identifying stages (default "Day").
#' @param time Optional named numeric vector mapping stages to actual times.
#'     NULL spaces stages equally in selected sample order. All selected stages,
#'     including shared references, remain on the axis. Use this mapping to
#'     specify chronological order when samples are stored in another order.
#' @param samples Optional unique sample identifiers. NULL uses all samples.
#'     Use a selection to restrict the plot to one cell line, dose or other
#'     experimental stratum; such strata are not automatically modelled.
#' @param pseudocount Non-negative finite value added to each condition mean
#'     before taking the ratio (default 0). Zero means require a positive value;
#'     the choice can strongly affect low-abundance genes.
#' @param labels Whether to label percentage differences (default TRUE).
#'     With a positive pseudocount these labels also use the adjusted ratio.
#' @param palette Optional treatment color, or named palette overriding the
#'     stored condition palette. Otherwise the object palette is used when
#'     available, falling back to ggplot2 colors.
#' @param point_size,line_size Size of points and width of connecting lines.
#' @param nrow,ncol Optional numbers of facet rows and columns.
#' @param scales Facet scales: "fixed" (default), "free", "free_x" or "free_y".
#' @details Only stages with both conditions have a comparison. Other stages
#'     have missing values, never fabricated zero effects; lines break at missing
#'     comparisons. A single comparable stage can also be plotted.
#'     This is a descriptive ratio of arithmetic means, not the median barcode
#'     log2 fold change reported by hit-finding methods, a confidence interval,
#'     or a test of temporal change. Samples are averaged without pairing or
#'     adjustment for covariates. The plot is independent of the hit-calling
#'     method and accepts normalized barcode screens supported by ScreenR.
#'     No theme is applied; choose appearance with theme_set() in user code.
#' @return A ggplot object. Its data contain Gene, Time, TreatmentMean,
#'     ControlMean, NTreatment, NControl, Ratio, Log2Ratio, PercentDifference,
#'     Label and Condition. Missing comparisons are retained as NA rows.
#' @seealso \code{\link{plot_trend}}, \code{\link{plot_barcode_heatmap}}
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' plot_treatment_separation(object, "Gene_42", treatment = "TRT",
#'     control = "Time3", time_column = "Day")
plot_treatment_separation <- function(screenR_Object, genes, treatment, control,
    condition_column = "Treatment", time_column = "Day", time = NULL,
    samples = NULL, pseudocount = 0, labels = TRUE, palette = NULL,
    point_size = 2, line_size = 0.8, nrow = NULL, ncol = NULL,
    scales = "fixed") {
    object <- update_screenr_object(screenR_Object)
    valid_ids <- function(ids, available, what) {
        if (!is.character(ids) || !length(ids) || anyNA(ids) ||
            any(!nzchar(ids)) || anyDuplicated(ids) || any(!ids %in% available)) {
            stop(what, " must be unique, non-missing identifiers present in ",
                "the object.", call. = FALSE)
        }
    }
    valid_ids(genes, genes_of(object), "genes")
    if (is.null(samples)) samples <- colnames(object)
    valid_ids(samples, colnames(object), "samples")
    info <- as.data.frame(SummarizedExperiment::colData(object))[samples, , drop = FALSE]
    for (column in list(condition_column, time_column)) {
        if (!is.character(column) || length(column) != 1L || is.na(column) ||
            !column %in% names(info)) {
            stop("condition_column and time_column must name colData variables.",
                call. = FALSE)
        }
        if (anyNA(info[[column]]) || any(!nzchar(as.character(info[[column]])))) {
            stop("Selected condition and time metadata must not be missing or empty.",
                call. = FALSE)
        }
    }
    for (value in list(treatment, control)) {
        if (!is.character(value) || length(value) != 1L || is.na(value) ||
            !value %in% as.character(info[[condition_column]])) {
            stop("treatment and control must each identify a selected condition.",
                call. = FALSE)
        }
    }
    if (identical(treatment, control)) {
        stop("treatment and control must be distinct.", call. = FALSE)
    }
    if (!is.numeric(pseudocount) || length(pseudocount) != 1L ||
        !is.finite(pseudocount) || pseudocount < 0) {
        stop("pseudocount must be a non-negative finite number.", call. = FALSE)
    }
    if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
        stop("labels must be TRUE or FALSE.", call. = FALSE)
    }
    scales <- match.arg(scales, c("fixed", "free", "free_x", "free_y"))
    if (!is.null(time) && (is.null(names(time)) || anyNA(names(time)) ||
        any(!nzchar(names(time))) || anyDuplicated(names(time)) || anyDuplicated(time))) {
        stop("time must map unique stage names to distinct finite numeric times.",
            call. = FALSE)
    }
    info$Sample <- samples
    positions <- encode_time(info, time_column, time)
    stage_labels <- attr(positions, "labels")
    values <- normalized_counts(object, "plot_treatment_separation()")
    gene_ids <- genes_of(object)
    means <- vapply(genes, function(gene) {
        colMeans(values[which(gene_ids == gene), samples, drop = FALSE])
    }, numeric(length(samples)))
    observations <- tibble::tibble(
        Gene = rep(genes, each = length(samples)),
        Time = rep(as.numeric(positions), times = length(genes)),
        Condition = rep(as.character(info[[condition_column]]), times = length(genes)),
        Frequency = as.vector(means))
    summarize_condition <- function(condition, mean_name, n_name) {
        subset <- observations[observations$Condition == condition, ]
        summary <- subset %>%
            dplyr::group_by(.data$Gene, .data$Time) %>%
            dplyr::summarise(Mean = mean(.data$Frequency), N = dplyr::n(),
                .groups = "drop")
        names(summary)[3:4] <- c(mean_name, n_name)
        summary
    }
    data <- tidyr::expand_grid(Gene = genes, Time = sort(unique(as.numeric(positions))))
    data <- dplyr::left_join(data,
        summarize_condition(treatment, "TreatmentMean", "NTreatment"),
        by = c("Gene", "Time"))
    data <- dplyr::left_join(data,
        summarize_condition(control, "ControlMean", "NControl"),
        by = c("Gene", "Time"))
    paired <- !is.na(data$TreatmentMean) & !is.na(data$ControlMean)
    if (!any(paired)) {
        stop("The selected conditions have no shared time point.", call. = FALSE)
    }
    if (any(data$TreatmentMean[paired] + pseudocount <= 0) ||
        any(data$ControlMean[paired] + pseudocount <= 0)) {
        stop("Zero condition means require a positive pseudocount.", call. = FALSE)
    }
    data$Ratio <- (data$TreatmentMean + pseudocount) /
        (data$ControlMean + pseudocount)
    data$Log2Ratio <- log2(data$Ratio)
    data$PercentDifference <- 100 * (data$Ratio - 1)
    data$Label <- ifelse(paired, sprintf("%+.0f%%", data$PercentDifference), NA_character_)
    data$Gene <- factor(data$Gene, levels = genes)
    data$Condition <- factor(treatment, levels = treatment)
    # Keep missing comparisons inside trajectories so lines break at those stages.
    line_data <- data[data$Gene %in% names(which(tapply(paired, data$Gene, sum) > 1)), ]
    plot <- ggplot2::ggplot(data,
        aes(.data$Time, .data$Log2Ratio, group = .data$Gene, colour = .data$Condition)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_line(data = line_data, linewidth = line_size, na.rm = TRUE) +
        ggplot2::geom_point(size = point_size, na.rm = TRUE) +
        palette_scale(object, condition_column, treatment, palette, aesthetic = "colour") +
        ggplot2::guides(colour = "none") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.15, .3))) +
        ggplot2::labs(x = if (is.null(time)) time_column else "Time",
            y = paste0("Log2 ", treatment, " / ", control))
    if (labels) {
        plot <- plot + ggplot2::geom_text(aes(label = .data$Label),
            vjust = -1, size = 3, na.rm = TRUE)
    }
    if (is.null(time)) {
        plot <- plot + ggplot2::scale_x_continuous(
            breaks = seq_along(stage_labels), labels = stage_labels,
            limits = range(seq_along(stage_labels)) + c(-.2, .2))
    } else {
        plot <- plot + ggplot2::scale_x_continuous(breaks = sort(unique(positions)))
    }
    if (length(genes) > 1L) {
        plot <- plot + ggplot2::facet_wrap("Gene", nrow = nrow, ncol = ncol, scales = scales)
    }
    plot
}
