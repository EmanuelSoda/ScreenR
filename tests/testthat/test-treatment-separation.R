separation_fixture <- function() {
    counts <- rbind(c(10, 20, 24, 14, 18, 30, 8),
        c(20, 30, 34, 24, 28, 40, 18), rep(100, 7), rep(100, 7))
    samples <- paste0("sample", seq_len(ncol(counts)))
    colnames(counts) <- samples
    object <- create_screenr_object(
        data.frame(Barcode = paste0("b", 1:4), counts),
        data.frame(Barcode = paste0("b", 1:4), Gene = rep(c("G1", "G2"), each = 2)),
        sample_table = data.frame(Sample = samples,
            Day = c("baseline", rep("early", 4), rep("late", 2)),
            Treatment = c("reference", "vehicle", "vehicle", "drug", "drug",
                "vehicle", "drug"),
            Hours = c("zero", rep("eight", 4), rep("forty", 2))),
        palettes = list(Treatment = c(reference = "grey", vehicle = "blue", drug = "red")))
    object <- normalize_data(object, "control", control_barcodes = "b4")
    info <- SummarizedExperiment::colData(object)
    info$Arm <- info$Treatment
    SummarizedExperiment::colData(object) <- info
    object
}

test_that("separation uses biological sample means and retains reference stages", {
    object <- separation_fixture()
    original <- object
    p <- plot_treatment_separation(object, c("G2", "G1"), "drug", "vehicle")
    expect_identical(levels(p$data$Gene), c("G2", "G1"))
    g1 <- p$data[p$data$Gene == "G1", ]
    expect_equal(g1$Ratio, c(NA, 21 / 27, 13 / 35))
    expect_equal(g1$Log2Ratio, log2(c(NA, 21 / 27, 13 / 35)))
    expect_equal(g1$NTreatment, c(NA_integer_, 2L, 1L))
    expect_equal(g1$NControl, c(NA_integer_, 2L, 1L))
    expect_equal(g1$Label, c(NA, "-22%", "-63%"))
    expect_equal(p$scales$get_scales("x")$labels, c("baseline", "early", "late"))
    expect_setequal(ggplot2::layer_data(p, 3)$colour, "red")
    expect_identical(object, original)
})

test_that("custom metadata, real times, subsets and reversed conditions work", {
    object <- separation_fixture()
    p <- plot_treatment_separation(object, "G1", "drug", "vehicle",
        condition_column = "Arm", time_column = "Hours",
        time = c(zero = 0, eight = 8, forty = 40),
        samples = rev(colnames(object)), labels = FALSE, palette = c(drug = "purple"))
    expect_equal(p$data$Time, c(0, 8, 40))
    expect_equal(p$data$Ratio, c(NA, 21 / 27, 13 / 35))
    expect_equal(p$scales$get_scales("x")$breaks, c(0, 8, 40))
    expect_setequal(ggplot2::layer_data(p, 3)$colour, "purple")
    single <- plot_treatment_separation(object, "G1", "vehicle", "drug",
        samples = colnames(object)[2:5])
    expect_equal(single$data$Ratio, 27 / 21)
    expect_equal(nrow(single$layers[[2]]$data), 0L)
    expect_no_warning(ggplot2::ggplot_build(single))
    # Sample selection changes the biological replicate mean, not the guide count.
    subset <- plot_treatment_separation(object, "G1", "drug", "vehicle",
        samples = colnames(object)[c(2, 4, 5)])
    expect_equal(subset$data$Ratio, 21 / 25)
    expect_equal(subset$data$NControl, 1L)
})

test_that("zero means need explicit pseudocounts and missing stages stay missing", {
    object <- separation_fixture()
    counts <- get_count_table(object)
    counts[1:2, 5:6] <- 0
    object <- normalize_data(set_count_table(object, counts), "control", control_barcodes = "b4")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle"), "pseudocount")
    p <- plot_treatment_separation(object, "G1", "drug", "vehicle", pseudocount = 1)
    expect_equal(p$data$Ratio[2], 1 / (27e4 + 1))
    expect_true(is.na(p$data$Ratio[1]))
    expect_true(all(is.finite(p$data$Log2Ratio[-1])))
    # An absent control at an intermediate stage must not become an effect of zero.
    object <- separation_fixture()
    info <- SummarizedExperiment::colData(object)
    info$Day <- c("early", "middle", "middle", "early", "middle", "late", "late")
    info$Treatment[1] <- "vehicle"
    info$Treatment[2:3] <- "reference"
    SummarizedExperiment::colData(object) <- info
    p <- plot_treatment_separation(object, "G1", "drug", "vehicle")
    expect_true(is.na(p$data$Log2Ratio[2]))
    expect_true(is.na(p$layers[[2]]$data$Log2Ratio[2]))
})

test_that("separation rejects invalid inputs and nonoverlapping comparisons", {
    object <- separation_fixture()
    expect_error(plot_treatment_separation(object, "missing", "drug", "vehicle"), "genes")
    expect_error(plot_treatment_separation(object, "G1", "drug", "unknown"), "condition")
    expect_error(plot_treatment_separation(object, "G1", "drug", "drug"), "distinct")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        samples = c("sample2", "sample2")), "samples")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        condition_column = "missing"), "colData")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        time = c(baseline = 0, early = 8)), "no value")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        time = c(baseline = 0, early = 8, late = 8)), "distinct")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        pseudocount = -1), "non-negative")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        labels = NA), "TRUE or FALSE")
    expect_error(plot_treatment_separation(object, "G1", "drug", "vehicle",
        samples = c("sample2", "sample7")), "shared time")
})

test_that("separation leaves themes to the caller", {
    previous <- ggplot2::theme_set(ggplot2::theme_void())
    on.exit(ggplot2::theme_set(previous))
    p <- plot_treatment_separation(separation_fixture(), "G1", "drug", "vehicle")
    expect_length(p$theme, 0L)
    expect_identical(ggplot2::theme_get(), ggplot2::theme_void())
    expect_true("plot_treatment_separation" %in% getNamespaceExports("ScreenR"))
})

test_that("a time course compares every time point with the control", {
    counts <- rbind(c(100, 60, 50, 20, 10), c(80, 40, 50, 10, 30),
        rep(100, 5), rep(100, 5))
    samples <- c("T0", "T3_A", "T3_B", "T18_A", "T18_B")
    colnames(counts) <- samples
    object <- create_screenr_object(
        data.frame(Barcode = paste0("b", 1:4), counts),
        data.frame(Barcode = paste0("b", 1:4), Gene = rep(c("G1", "G2"), each = 2)),
        sample_table = data.frame(Sample = samples,
            Day = c("T0", "T3", "T3", "T18", "T18"), Treatment = "Untreated"),
        palettes = list(Day = c(T0 = "grey", T3 = "purple", T18 = "orange")))
    object <- normalize_data(object, "control", control_barcodes = "b4")
    values <- get_normalized_count_table(object)
    g1 <- colMeans(as.matrix(values[1:2, samples]))
    p <- plot_treatment_separation(object, "G1", treatment = c("T3", "T18"),
        control = "T0", condition_column = "Day", time_column = "Day")
    expect_equal(p$scales$get_scales("x")$labels, c("T0", "T3", "T18"))
    expect_equal(p$data$Ratio, c(1, mean(g1[2:3]) / g1[[1]],
        mean(g1[4:5]) / g1[[1]]), ignore_attr = TRUE)
    expect_equal(p$data$NTreatment, c(1L, 2L, 2L))
    expect_equal(p$data$NControl, c(1L, 1L, 1L))
    expect_true(is.na(p$data$Label[1]))
    expect_identical(as.character(p$data$Condition), c("T0", "T3", "T18"))
    expect_setequal(ggplot2::layer_data(p, 3)$colour, c("grey", "purple", "orange"))
    expect_setequal(ggplot2::layer_data(p, 2)$colour, "grey60")
    expect_equal(p$labels$y, "Log2 ratio to T0")
    # Only the selected time points, at their real times
    late <- plot_treatment_separation(object, "G1", treatment = "T18",
        control = "T0", condition_column = "Day", time_column = "Day",
        time = c(T0 = 0, T3 = 3, T18 = 18))
    expect_equal(late$data$Time, c(0, 18))
    expect_error(plot_treatment_separation(object, "G1", c("T3", "T0"), "T0",
        condition_column = "Day", time_column = "Day"), "distinct")
    # Several treatments are only accepted in a time course
    expect_error(plot_treatment_separation(object, "G1", c("T3", "T18"), "T0",
        condition_column = "Day", time_column = "Treatment"), "condition")
})
