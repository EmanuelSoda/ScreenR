#' @title Plot the trend of genes over time
#' @description Plots, for the given genes, the mean normalized count of their
#'     barcodes in each sample (taken from the data table) against the time
#'     point of the sample, with a mean trajectory or regression line. It shows
#'     whether a hit decreases (or increases) over time in the selected samples. The slope
#'     of an ungrouped regression line is computed by
#'     \code{\link{compute_slope}} and
#'     used by \code{\link{filter_by_slope}}.
#' @details Replicates of the same time point are drawn at the same time. By
#'     default the time points are numbered 1, 2, ... in the order of the
#'     samples (equally spaced); give time to place them at their real times.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param group_var The values of the Treatment column of the data table (see
#'     \code{\link{get_sample_table}}) of the samples to plot, for example the
#'     early time points and the treated samples.
#' @param alpha The opacity of the confidence band, between 0 and 1. Default
#'     0.5.
#' @param se Whether to draw the confidence band of the regression line.
#'     Default FALSE.
#' @param point_size The size of the points. Default 1.
#' @param line_size The width of the regression line. Default 1.
#' @param nrow The number of rows of panels, when several genes are plotted.
#' @param ncol The number of columns of panels, when several genes are
#'     plotted. By default (NULL) ggplot2 chooses nrow and ncol.
#' @param genes The names of the genes to plot.
#' @param scales The scales of the panels: "free" (default), "fixed",
#'     "free_x" or "free_y".
#' @param time_column The column of the data table with the time point of
#'     each sample: "Day" (default) or, for example, "Treatment" when the
#'     time points are described there.
#' @param time An optional named numeric vector with the time of each time
#'     point (the names are the values of time_column), for example
#'     c(T0 = 0, T3 = 3, T18 = 18). By default the time points are equally
#'     spaced, in the order of the samples.
#' @param color_by An optional categorical colData variable, for example
#'     "Treatment", to overlay a separate trend for each condition. NULL
#'     preserves the original single trend per gene. Color and shape both
#'     identify conditions; object palettes are used unless palette is given.
#' @param trend NULL (default), "mean", or "lm". NULL selects a line joining
#'     time-point means when color_by is supplied, and the original linear
#'     regression otherwise. Mean lines connect condition means across
#'     biological samples; they do not connect individual cultures.
#' @param reference_groups Optional Treatment values to show as neutral
#'     reference points, excluded from trend summaries and regressions. They
#'     must also be selected in group_var. Shared baseline observations are
#'     drawn once, not duplicated into both conditions.
#' @param connect_reference With mean trajectories and color_by, connect the
#'     shared reference sequence to each condition using its latest reference
#'     before that condition's first observation (default FALSE). Reference
#'     observations are still displayed once; these connecting segments are
#'     descriptive and do not add independent samples or enter regressions.
#' @param highlight_difference Shade the gap between two condition means at
#'     their shared time points (default FALSE, requires mean trajectories).
#'     This band shows the observed separation, not a confidence interval.
#' @param palette Optional colors for the levels of color_by, overriding the
#'     stored palette.
#' @details Each small point is one biological sample, summarized across its
#'     barcodes. Larger points on mean trajectories summarize those samples
#'     at each time point. The se argument is supported only for trend = "lm";
#'     its band is the ordinary regression confidence band, not an interval
#'     for the edgeR/limma gene-level treatment effect. With color_by, each
#'     condition is fitted separately. Actual times are recommended when known.
#'     No theme is applied; choose appearance in user code with theme_set().
#' @return A ggplot object.
#' @seealso \code{\link{compute_slope}}, \code{\link{filter_by_slope}},
#'     \code{\link{plot_boxplot}}
#' @importFrom ggplot2 geom_smooth
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_trend(object, genes = "Gene_42", group_var = c("T1", "T2", "TRT"))
#'
#' plot_trend(object,
#'     genes = c("Gene_42", "Gene_100"),
#'     group_var = c("T1", "T2", "TRT"),
#'     nrow = 2
#' )
#'
#' # With the real times of the time points
#' plot_trend(object,
#'     genes = "Gene_42", group_var = c("T1", "T2", "TRT"),
#'     time = c(T1 = 0, T2 = 2, Time3 = 7, Time4 = 14)
#' )
plot_trend <- function(screenR_Object, genes, group_var,
    alpha = 0.5, se = FALSE, point_size = 1, line_size = 1,
    nrow = NULL, ncol = NULL, scales = "free", time_column = "Day",
    time = NULL, color_by = NULL, trend = NULL, reference_groups = NULL,
    palette = NULL, connect_reference = FALSE, highlight_difference = FALSE) {
    object <- update_screenr_object(screenR_Object)
    if (is.null(trend)) trend <- if (is.null(color_by)) "lm" else "mean"
    trend <- match.arg(trend, c("mean", "lm"))
    if (se && trend == "mean") {
        stop("se is a regression confidence band: use trend = 'lm', or ",
            "se = FALSE for mean trajectories.", call. = FALSE)
    }
    for (flag in list(connect_reference, highlight_difference)) {
        if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
            stop("connect_reference and highlight_difference must be TRUE or FALSE.",
                call. = FALSE)
        }
    }
    if ((connect_reference || highlight_difference) &&
        (trend != "mean" || is.null(color_by))) {
        stop("Reference connections and gap shading require color_by and ",
            "trend = 'mean'.", call. = FALSE)
    }
    data <- compute_trend(object, genes, group_var, time_column, time,
        color_by, reference_groups)
    if (!is.null(color_by)) data$Gene <- factor(data$Gene, levels = unique(genes))
    plotted <- data[!data$Reference, ]
    mapping <- if (is.null(color_by)) {
        aes(.data$Time, .data$Frequency, group = .data$Gene)
    } else {
        aes(.data$Time, .data$Frequency,
            group = interaction(.data$Gene, .data$Condition),
            colour = .data$Condition, shape = .data$Condition)
    }
    plot <- ggplot2::ggplot(data, mapping)
    if (highlight_difference) {
        conditions <- unique(as.character(plotted$Condition))
        if (length(conditions) != 2L) {
            stop("Gap shading requires exactly two selected conditions.", call. = FALSE)
        }
        gap <- plotted %>%
            dplyr::group_by(.data$Gene, .data$Condition, .data$Time) %>%
            dplyr::summarise(Mean = mean(.data$Frequency), .groups = "drop")
        first <- gap[as.character(gap$Condition) == conditions[1],
            c("Gene", "Time", "Mean")]
        second <- gap[as.character(gap$Condition) == conditions[2],
            c("Gene", "Time", "Mean")]
        gap <- dplyr::inner_join(first, second, by = c("Gene", "Time"),
            suffix = c("_first", "_second"))
        if (nrow(gap) == 0L) {
            stop("Gap shading needs at least one shared time point.", call. = FALSE)
        }
        # Insert crossings so the shaded gap closes exactly where the
        # condition lines meet, rather than interpolating an outer envelope.
        gap <- dplyr::bind_rows(lapply(split(gap, gap$Gene, drop = TRUE),
            function(panel) {
                panel <- panel[order(panel$Time), ]
                crossings <- list()
                if (nrow(panel) > 1L) {
                    delta <- panel$Mean_first - panel$Mean_second
                    for (i in seq_len(nrow(panel) - 1L)) {
                        if (delta[i] * delta[i + 1L] < 0) {
                            fraction <- delta[i] / (delta[i] - delta[i + 1L])
                            point <- panel[i, ]
                            point$Time <- panel$Time[i] + fraction *
                                (panel$Time[i + 1L] - panel$Time[i])
                            value <- panel$Mean_first[i] + fraction *
                                (panel$Mean_first[i + 1L] - panel$Mean_first[i])
                            point$Mean_first <- point$Mean_second <- value
                            crossings[[length(crossings) + 1L]] <- point
                        }
                    }
                }
                panel <- dplyr::bind_rows(panel, dplyr::bind_rows(crossings))
                panel[order(panel$Time), ]
            }))
        gap$Lower <- pmin(gap$Mean_first, gap$Mean_second)
        gap$Upper <- pmax(gap$Mean_first, gap$Mean_second)
        plot <- plot + ggplot2::geom_ribbon(data = gap,
            aes(x = .data$Time, ymin = .data$Lower, ymax = .data$Upper,
                group = .data$Gene), inherit.aes = FALSE,
            fill = "grey55", alpha = 0.18)
    }
    plot <- plot + ggplot2::geom_point(data = plotted, size = point_size)
    if (trend == "lm") {
        plot <- plot + ggplot2::geom_smooth(data = plotted,
            method = "lm", formula = y ~ x, alpha = alpha,
            se = se, linewidth = line_size)
    } else {
        means <- plotted %>%
            dplyr::group_by(.data$Gene, .data$Condition, .data$Time) %>%
            dplyr::summarise(Frequency = mean(.data$Frequency), .groups = "drop")
        trajectory <- means
        if (connect_reference) {
            references <- data[data$Reference, ]
            if (nrow(references) == 0L) {
                stop("connect_reference needs selected reference_groups.", call. = FALSE)
            }
            references <- references %>%
                dplyr::group_by(.data$Gene, .data$Time) %>%
                dplyr::summarise(Frequency = mean(.data$Frequency), .groups = "drop")
            anchors <- lapply(split(means, interaction(means$Gene,
                means$Condition, drop = TRUE)), function(condition) {
                available <- references[references$Gene == condition$Gene[1] &
                    references$Time < min(condition$Time), ]
                if (nrow(available) == 0L) return(NULL)
                anchor <- available[which.max(available$Time), ]
                anchor$Condition <- condition$Condition[1]
                anchor
            })
            trajectory <- dplyr::bind_rows(means, dplyr::bind_rows(anchors))
            reference_lines <- references[duplicated(references$Gene) |
                duplicated(references$Gene, fromLast = TRUE), ]
            plot <- plot + ggplot2::geom_line(data = reference_lines,
                aes(.data$Time, .data$Frequency, group = .data$Gene),
                inherit.aes = FALSE, colour = "grey50", linewidth = line_size)
        }
        plot <- plot + ggplot2::geom_line(data = trajectory, linewidth = line_size) +
            ggplot2::geom_point(data = means, size = point_size + 1)
    }
    if (any(data$Reference)) {
        plot <- plot + ggplot2::geom_point(data = data[data$Reference, ],
            aes(.data$Time, .data$Frequency), inherit.aes = FALSE,
            colour = "grey45", shape = 4, size = point_size + 0.5)
    }
    plot <- plot + ggplot2::labs(y = "Mean normalized counts")
    if (!is.null(color_by)) {
        plot <- plot + palette_scale(object, color_by, plotted$Condition,
            palette, aesthetic = "colour") +
            ggplot2::labs(colour = color_by, shape = color_by)
    }
    if (is.null(time)) {
        labels <- attr(data$Time, "labels")
        plot <- plot + ggplot2::scale_x_continuous(
            breaks = seq_along(labels), labels = labels) +
            ggplot2::labs(x = time_column)
    } else {
        plot <- plot + ggplot2::scale_x_continuous(
            breaks = sort(unique(data$Time))) + ggplot2::labs(x = "Time")
    }
    if (length(genes) > 1) {
        plot <- plot + ggplot2::facet_wrap("Gene",
            nrow = nrow, ncol = ncol, scales = scales)
    }
    plot
}

#' @title Compute the trend of genes
#' @description Internal function that computes, for each gene and sample, the
#'     mean normalized count of the barcodes of the gene, and the time of the
#'     sample (see \code{\link{encode_time}}).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param genes The names of the genes.
#' @param group_var The values of the Treatment column of the samples to use.
#' @param time_column The column of the data table with the time point of
#'     each sample.
#' @param time NULL or a named numeric vector with the time of each time
#'     point.
#' @param color_by An optional colData column for condition trends.
#' @param reference_groups Treatment values excluded from trend fitting.
#' @return A tibble with Sample, Gene, Frequency, Time, Condition and Reference.
#' @keywords internal
compute_trend <- function(screenR_Object, genes, group_var,
    time_column = "Day", time = NULL, color_by = NULL,
    reference_groups = NULL) {
    if (any(!reference_groups %in% group_var)) {
        stop("reference_groups must also be selected in group_var.", call. = FALSE)
    }
    object <- update_screenr_object(screenR_Object)
    data <- data_table_of(object,
        "plot_trend()")

    # Select only the hit gene
    data <- dplyr::filter(data, .data$Gene %in% genes)

    # Select only the sample of interest
    data <- dplyr::filter(data, .data$Treatment %in% group_var)
    data$Time <- encode_time(data, time_column, time)
    labels <- attr(data$Time, "labels")

    # Consider only the gene (which are the mean of the different shRNAs)
    data <- dplyr::summarise(
        dplyr::group_by(data, .data$Sample, .data$Gene),
        Time = unique(.data$Time),
        Frequency = mean(.data$Frequency),
        .groups = "drop"
    )
    if (nrow(data) == 0) {
        stop("No samples and genes match the selection.", call. = FALSE)
    }
    info <- SummarizedExperiment::colData(object)
    sample_rows <- match(as.character(data$Sample), colnames(object))
    data$Reference <- as.character(info$Treatment[sample_rows]) %in%
        reference_groups
    if (is.null(color_by)) {
        data$Condition <- factor("All")
    } else {
        if (!is.character(color_by) || length(color_by) != 1L ||
            is.na(color_by) || !color_by %in% colnames(info)) {
            stop("color_by must name a colData variable.", call. = FALSE)
        }
        values <- as.character(info[[color_by]][sample_rows])
        if (anyNA(values[!data$Reference])) {
            stop("color_by has missing values in the selected samples.",
                call. = FALSE)
        }
        data$Condition <- factor(values,
            levels = variable_values(info[[color_by]]))
    }
    if (all(data$Reference)) {
        stop("Select at least one non-reference condition.", call. = FALSE)
    }
    attr(data$Time, "labels") <- labels
    return(data)
}
