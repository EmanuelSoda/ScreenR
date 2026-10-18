longitudinal_plot_fixture <- function() {
    samples <- c("Baseline", "T1_CTRL_1", "T1_CTRL_2", "T1_TRT_1",
        "T1_TRT_2", "T2_CTRL_1", "T2_TRT_1")
    counts <- rbind(c(10, 20, 24, 14, 18, 30, 8),
        c(20, 30, 34, 24, 28, 40, 18), rep(100, 7), rep(100, 7))
    colnames(counts) <- samples
    object <- create_screenr_object(
        data.frame(Barcode = paste0("b", 1:4), counts),
        data.frame(Barcode = paste0("b", 1:4), Gene = rep(c("G1", "G2"), each = 2)),
        sample_table = data.frame(Sample = samples,
            Day = c("T0", rep("T1", 4), rep("T2", 2)),
            Treatment = c("Baseline", "CTRL", "CTRL", "TRT", "TRT", "CTRL", "TRT")))
    object <- normalize_data(object, "control", control_barcodes = "b4")
    set_palettes(object, list(Treatment = c(Baseline = "grey50",
        CTRL = "#1B9E77", TRT = "#D95F02")))
}

test_that("overlaid trends summarize biological samples separately by condition", {
    object <- longitudinal_plot_fixture()
    p <- plot_trend(object, "G1", c("CTRL", "TRT"), color_by = "Treatment")
    expect_equal(nrow(p$data), 6L)
    expect_equal(nrow(p$layers[[1]]$data), 6L)
    means <- p$layers[[2]]$data
    expect_equal(nrow(means), 4L)
    expect_equal(means$Frequency[means$Condition == "CTRL" & means$Time == 1],
        mean(c(25, 29)) * 1e4)
    expect_equal(means$Frequency[means$Condition == "TRT" & means$Time == 1],
        mean(c(19, 23)) * 1e4)
    built <- ggplot2::ggplot_build(p)
    expect_equal(length(unique(built$data[[2]]$group)), 2L)
    expect_setequal(unique(built$data[[1]]$colour), c("#1B9E77", "#D95F02"))
    expect_equal(length(unique(built$data[[1]]$shape)), 2L)
    override <- plot_trend(object, "G1", c("CTRL", "TRT"),
        color_by = "Treatment", palette = c(CTRL = "blue", TRT = "red"))
    expect_setequal(unique(ggplot2::layer_data(override)$colour), c("blue", "red"))
})

test_that("shared reference points are shown once and never enter the means", {
    object <- longitudinal_plot_fixture()
    p <- plot_trend(object, c("G1", "G2"), c("Baseline", "CTRL", "TRT"),
        color_by = "Treatment", reference_groups = "Baseline")
    expect_equal(sum(p$data$Reference), 2L)
    expect_equal(nrow(p$layers[[4]]$data), 2L)
    expect_false(any(p$layers[[2]]$data$Condition == "Baseline"))
    expect_equal(nrow(p$layers[[2]]$data), 8L)
    expect_equal(sort(unique(ggplot2::layer_data(p, 4)$x)), 1)
    expect_error(plot_trend(object, "G1", "Baseline", color_by = "Treatment",
        reference_groups = "Baseline"), "non-reference")
})

test_that("condition regressions and real-time coordinates remain separate", {
    object <- longitudinal_plot_fixture()
    p <- plot_trend(object, "G1", c("CTRL", "TRT"), color_by = "Treatment",
        trend = "lm", time = c(T1 = 7, T2 = 14))
    expect_equal(sort(unique(p$data$Time)), c(7, 14))
    lines <- ggplot2::layer_data(p, 2)
    expect_equal(length(unique(lines$group)), 2L)
    slopes <- vapply(split(lines, lines$group), function(x) {
        (tail(x$y, 1) - x$y[1]) / (tail(x$x, 1) - x$x[1])
    }, numeric(1))
    expected <- vapply(split(p$data, p$data$Condition, drop = TRUE), function(x) {
        unname(coef(lm(Frequency ~ Time, data = x))[2])
    }, numeric(1))
    expect_equal(unname(slopes), unname(expected))
    expect_error(plot_trend(object, "G1", "CTRL", color_by = "missing"), "colData")
    expect_error(plot_trend(object, "G1", "CTRL", trend = "mean", se = TRUE),
        "regression confidence")
})

test_that("heatmap values follow the normalization and explicit centering", {
    object <- longitudinal_plot_fixture()
    p <- plot_barcode_heatmap(object, c("G2", "G1"))
    expect_identical(levels(p$data$Gene), c("G2", "G1"))
    expect_equal(as.numeric(tapply(p$data$Value, p$data$Barcode, mean)), rep(0, 4),
        tolerance = 1e-10)
    expect_equal(nrow(ggplot2::layer_data(p)), 28L)
    expected <- log2(ScreenR:::normalized_counts(object) + 1)
    raw <- plot_barcode_heatmap(object, "G1", center = "none")
    expect_equal(raw$data$Value, as.vector(t(expected[1:2, ])))
    samples <- rev(colnames(object)[-1])
    reference <- plot_barcode_heatmap(object, "G1", samples = samples,
        center = "reference", reference_samples = "Baseline")
    values <- sweep(expected[1:2, samples, drop = FALSE], 1, expected[1:2, 1], "-")
    expect_equal(reference$data$Value, as.vector(t(values)))
    expect_identical(levels(reference$data$Sample), samples)
    expect_true(all(is.finite(ggplot2::layer_data(reference)$x)))
    ungrouped <- plot_barcode_heatmap(object, "G1", column_group = NULL)
    expect_equal(nlevels(ungrouped$data$ColumnGroup), 1L)
})

test_that("heatmaps reject ambiguous selections and invalid references", {
    object <- longitudinal_plot_fixture()
    expect_error(plot_barcode_heatmap(object, "unknown"), "genes")
    expect_error(plot_barcode_heatmap(object, "G1", samples = c("Baseline", "Baseline")),
        "samples")
    expect_error(plot_barcode_heatmap(object, "G1", center = "reference"),
        "reference_samples")
    expect_error(plot_barcode_heatmap(object, "G1", reference_samples = "Baseline"),
        "requires")
    expect_error(plot_barcode_heatmap(object, "G1", pseudocount = 0), "positive")
    expect_error(plot_barcode_heatmap(object, "G1", column_group = "unknown"), "colData")
})

test_that("all stages remain visible and references anchor both mean paths once", {
    object <- longitudinal_plot_fixture()
    p <- plot_trend(object, "G1", c("Baseline", "CTRL", "TRT"),
        color_by = "Treatment", reference_groups = "Baseline",
        connect_reference = TRUE, highlight_difference = TRUE)
    expect_equal(nrow(p$data), 7L)
    expect_equal(sum(p$data$Reference), 1L)
    expect_equal(sort(unique(p$data$Time)), 1:3)
    gap <- p$layers[[1]]$data
    expect_equal(as.numeric(gap$Time), c(2L, 3L))
    expect_equal(gap$Upper - gap$Lower, c(6, 22) * 1e4)
    paths <- p$layers[[4]]$data
    anchors <- paths[paths$Time == 1, ]
    expect_equal(nrow(anchors), 2L)
    expect_equal(anchors$Frequency, rep(15e4, 2))
    expect_setequal(as.character(anchors$Condition), c("CTRL", "TRT"))
    # Anchors are geometry only; the displayed reference remains one cross.
    expect_equal(nrow(p$layers[[6]]$data), 1L)
    expect_equal(nrow(ggplot2::layer_data(p, 6)), 1L)
    expect_error(plot_trend(object, "G1", c("CTRL", "TRT"),
        color_by = "Treatment", connect_reference = TRUE), "reference_groups")
    expect_error(plot_trend(object, "G1", "CTRL", color_by = "Treatment",
        highlight_difference = TRUE), "exactly two")
    expect_error(plot_trend(object, "G1", "CTRL", color_by = "Treatment",
        trend = "lm", highlight_difference = TRUE), "mean")
})

test_that("the new plots inherit the user theme without imposing theme elements", {
    object <- longitudinal_plot_fixture()
    previous <- ggplot2::theme_set(ggplot2::theme_void())
    p <- plot_trend(object, "G1", c("CTRL", "TRT"), color_by = "Treatment")
    h <- plot_barcode_heatmap(object, "G1")
    hidden <- plot_barcode_heatmap(object, "G1", barcode_labels = FALSE)
    expect_length(p$theme, 0L)
    expect_length(h$theme, 0L)
    expect_length(hidden$theme, 0L)
    expect_identical(ggplot2::theme_get(), ggplot2::theme_void())
    ggplot2::theme_set(previous)
})

test_that("gap shading closes at crossings without adding observations", {
    object <- longitudinal_plot_fixture()
    counts <- get_count_table(object)
    counts[1:2, grepl("T1_TRT", names(counts))] <- 100
    object <- normalize_data(set_count_table(object, counts), "control",
        control_barcodes = "b4")
    p <- plot_trend(object, "G1", c("CTRL", "TRT"), color_by = "Treatment",
        highlight_difference = TRUE)
    gap <- p$layers[[1]]$data
    expect_equal(nrow(p$data), 6L)
    expect_equal(nrow(gap), 3L)
    crossing <- gap[2, ]
    fraction <- 73 / (73 + 22)
    expect_equal(as.numeric(crossing$Time), 1 + fraction)
    expect_equal(crossing$Lower, crossing$Upper)
    expect_equal(crossing$Lower, (27 + fraction * 8) * 1e4)
})

test_that("gap shading rejects conditions with no shared time points", {
    object <- longitudinal_plot_fixture()
    info <- SummarizedExperiment::colData(object)
    info$Day[info$Treatment == "TRT"] <- c("T3", "T3", "T4")
    SummarizedExperiment::colData(object) <- info
    expect_error(plot_trend(object, "G1", c("CTRL", "TRT"),
        color_by = "Treatment", highlight_difference = TRUE), "shared time point")
})
