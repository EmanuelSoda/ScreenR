data("count_table", package = "ScreenR")
data("annotation_table", package = "ScreenR")
# The unmapped reads "*" appear several times: remove them
data <- count_table[count_table$Barcode != "*", ]
annotaion <- annotation_table

groups <- factor(c(
    "T1/T2", "T1/T2", "Treated", "Treated", "Treated",
    "Control", "Control", "Control", "Treated", "Treated", "Treated",
    "Control", "Control", "Control"
))


palette <- c(
    "#1B9E75", "#1B9E75", "#D95F02", "#D95F02", "#D95F02",
    "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
    "#66A61E", "#66A61E", "#66A61E"
)

create_test_object <- function() {
    data <- data %>%
        dplyr::filter(Barcode != "*")

    colnames(data) <- c(
        "Barcode", "T1", "T2", "Time3_TRT_A", "Time3_TRT_B", "Time3_TRT_C",
        "Time3_A", "Time3_B", "Time3_C", "Time4_TRT_A", "Time4_TRT_B",
        "Time4_TRT_C", "Time4_A", "Time4_B", "Time4_c"
    )
    obj <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    obj <- normalize_data(obj)

    # Keep only the barcodes of the first 10 genes; the effective library
    # sizes computed on all the barcodes are kept
    obj <- obj[SummarizedExperiment::rowData(obj)$Gene %in%
        paste0("Gene_", seq(1, 10)), ]

    return(obj)
}
test_that("plot trend hit", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_trend(
        screenR_Object = object, genes = c("Gene_1", "Gene_300"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2
    )
    expect_true(ggplot2::is_ggplot(plot))
})


test_that("Plot Boxplot violinplot", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "violinplot"
    )

    expect_true(ggplot2::is_ggplot(plot))
})


test_that("Plot Boxplot boxplot", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "boxplot"
    )

    expect_true(ggplot2::is_ggplot(plot))
})

test_that("Plot Boxplot error", {
    library(tibble)
    object <- create_test_object()

    expect_error(plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = ""
    ))
})

test_that("plot_barcode_lost_for_gene", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    plot <- plot_barcode_lost_for_gene(object, samples = c("Time3_A", "Time3_B"))
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_barcode_lost_for_gene", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    plot <- plot_barcode_lost_for_gene(object, samples = c("Time3_A", "Time3_B"))
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_barcode_trend", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_5")
    object <- object[SummarizedExperiment::rowData(object)$Gene %in% genes, ]

    metrics <- dplyr::bind_rows(
        compute_metrics(object,
            control = "Time3", treatment = "TRT",
            day = "Time3"
        ),
        compute_metrics(object,
            control = "Time4", treatment = "TRT",
            day = "Time4"
        )
    )
    p <- plot_barcode_trend(metrics, genes = c("Gene_1", "Gene_50"), n_col = 2)

    expect_equal(class(p)[1], "patchwork")
})

test_that("plot_barcode_trend color", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_5")

    metrics <- dplyr::bind_rows(
        compute_metrics(object,
            control = "Time3", treatment = "TRT",
            day = "Time3"
        ),
        compute_metrics(object,
            control = "Time4", treatment = "TRT",
            day = "Time4"
        )
    )
    p <- plot_barcode_trend(metrics,
        genes = c("Gene_1", "Gene_10"),
        n_col = 2, color = c(
            "red", "green", "orange",
            "black", "gray", "pink",
            "yellow", "brown", "purple",
            "aquamarine"
        )
    )

    expect_equal(class(p)[1], "patchwork")
})



test_that("plot_zscore_distribution", {
    object <- create_test_object()
    genes <- c("Gene_1", "Gene_5")
    object <- object[SummarizedExperiment::rowData(object)$Gene %in% genes, ]

    tables <- list(
        compute_metrics(object,
            control = "Time3",
            treatment = "TRT", day = "Time3"
        ),
        compute_metrics(object,
            control = "Time4",
            treatment = "TRT", day = "Time4"
        )
    )
    plot <- plot_zscore_distribution(tables, alpha = 0.5)

    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_barcode_hit", {
    object <- get0("object", envir = asNamespace("ScreenR"))

    object <- object[SummarizedExperiment::rowData(object)$Gene %in%
        paste0("Gene_", seq(1, 20)), ]

    matrix_model <- create_design_matrix(object)
    contrast <- create_contrast(matrix_model,
        treatment = "Treated", control = "Control"
    )
    plot <- plot_barcode_hit(object, matrix_model,
        contrast = contrast,
        gene = "Gene_10"
    )
    expect_true(ggplot2::is_ggplot(plot))
    expect_s3_class(plot, "patchwork")
    expect_length(plot$patches$plots, 2)

    # Without the panel of the log2 fold changes
    plot <- plot_barcode_hit(object, matrix_model,
        contrast = contrast,
        gene = "Gene_10", show_log2fc = FALSE
    )
    expect_length(plot$patches$plots, 1)
})

test_that("barcode_plot marks the barcodes of the gene", {
    log2fc <- c(-3, 2, -2, 0, 1, 0.2)
    in_gene <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
    plot <- barcode_plot(log2fc, in_gene, title = "G",
        quantile = c(-0.5, 0.5), labels = c("Down", "Up"),
        show_log2fc = TRUE)
    # The gene's barcodes are the two most depleted, ranks 1 and 2
    points <- ggplot2::layer_data(plot, 4)
    expect_equal(sort(points$x), c(1, 2))
    expect_equal(sort(points$y), c(-3, -2))
    # The enrichment line is above 1 on the left, where the barcodes are
    worm <- ggplot2::layer_data(plot$patches$plots[[1]], 2)
    expect_gt(worm$y[1], 1)
    expect_lt(worm$y[6], 1)
})


test_that("Plot number mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, palette)
    expect_true(ggplot2::is_ggplot(plot))
})


test_that("Plot number mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, NULL)
    expect_true(inherits(plot, "ggplot"))
})

test_that("Boxplot mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads_distribution(object, palette,
        alpha = 0.8,
        type = "boxplot"
    )

    expect_true(ggplot2::is_ggplot(plot))
})

test_that("Density mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads_distribution(object, palette,
        alpha = 0.8,
        type = "density"
    )
    expect_true(ggplot2::is_ggplot(plot))
})


test_that("Plot number of Barcode Lost", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )

    plot <- plot_barcode_lost(screenR_Object = object, palette = palette)
    expect_true(ggplot2::is_ggplot(plot))
})


test_that("Plot common Hit", {
    hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E", "F", "J", "L"))
    hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G", "L"))
    hit_roast <- data.frame(Gene = c("A", "L", "N", "F", "J"))
    plot <- suppressWarnings(plot_common_hit(hit_zscore, hit_camera, hit_zscore,
        show_percentage = FALSE
    ))
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_boxplot orders the fill legend as the samples", {
    data(hap1_count_table, package = "ScreenR", envir = environment())
    data(hap1_annotation_table, package = "ScreenR", envir = environment())
    object <- create_screenr_object(
        table = hap1_count_table, annotation = hap1_annotation_table,
        groups = factor(c("T0", "T3", "T3", "T3", "T18", "T18", "T18")),
        sample_table = data.frame(
            Sample = colnames(hap1_count_table)[-1], Day = "Screen",
            Treatment = c("T0", "T3", "T3", "T3", "T18", "T18", "T18")
        )
    )
    object <- compute_data_table(normalize_data(object))
    plot <- plot_boxplot(object, genes = "RPL14",
        group_var = c("T0", "T3", "T18"), fill_var = "Treatment")
    expect_equal(levels(plot$data$Treatment), c("T0", "T3", "T18"))
})

test_that("plot_barcode_lost draws the bars once, with alpha", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    plot <- plot_barcode_lost(object, alpha = 0.3)
    bars <- vapply(plot$layers, function(l) inherits(l$geom, "GeomBar"),
        logical(1))
    expect_equal(sum(bars), 1)
    expect_equal(plot$layers[[which(bars)]]$aes_params$alpha, 0.3)
})

test_that("plot_boxplot and plot_trend draw several genes by default", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    object <- compute_data_table(normalize_data(object))
    genes <- c("Gene_1", "Gene_2", "Gene_3")
    groups <- c("T1", "T2", "TRT")
    expect_silent(ggplot2::ggplot_build(
        plot_boxplot(object, genes = genes, group_var = groups)))
    expect_silent(ggplot2::ggplot_build(
        plot_trend(object, genes = genes, group_var = groups)))
})
