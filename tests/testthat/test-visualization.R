data("count_table", package = "ScreenR")
data("annotation_table", package = "ScreenR")
data <- count_table
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
    obj <- compute_data_table(obj)

    obj@data_table <- obj@data_table %>%
        dplyr::filter(Gene %in% paste0("Gene_", seq(1, 10)))

    obj@normalized_count_table <- obj@normalized_count_table %>%
        dplyr::filter(Barcode %in% obj@data_table$Barcode)

    obj@count_table <- obj@count_table %>%
        dplyr::filter(Barcode %in% obj@data_table$Barcode)

    obj@annotation_table <- obj@annotation_table %>%
        dplyr::filter(Barcode %in% obj@data_table$Barcode)

    return(obj)
}
test_that("plot trend hit", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_trend(
        screenR_Object = object, genes = c("Gene_1", "Gene_300"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2
    )
    expect_equal(class(plot)[[1]], "gg")
})


test_that("Plot Boxplot violinplot", {
    library(tibble)
    object <- create_test_object()

    p <- plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "violinplot"
    )

    expect_equal(class(p)[1], "gg")
})


test_that("Plot Boxplot boxplot", {
    library(tibble)
    object <- create_test_object()

    p <- plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "boxplot"
    )

    expect_equal(class(p)[1], "gg")
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
    p <- plot_barcode_lost_for_gene(object, samples = c("Time3_A", "Time3_B"))
    expect_equal(class(p)[1], "gg")
})

test_that("plot_barcode_lost_for_gene", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    p <- plot_barcode_lost_for_gene(object, samples = c("Time3_A", "Time3_B"))
    expect_equal(class(p)[1], "gg")
})

test_that("plot_barcode_trend", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_5")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    metrics <- dplyr::bind_rows(
        compute_metrics(object,
            control = "TRT", treatment = "Time3",
            day = "Time3"
        ),
        compute_metrics(object,
            control = "TRT", treatment = "Time4",
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
            control = "TRT", treatment = "Time3",
            day = "Time3"
        ),
        compute_metrics(object,
            control = "TRT", treatment = "Time4",
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
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    tables <- list(
        compute_metrics(object,
            control = "TRT",
            treatment = "Time3", day = "Time3"
        ),
        compute_metrics(object,
            control = "TRT",
            treatment = "Time4", day = "Time4"
        )
    )
    p <- plot_zscore_distribution(tables, alpha = 0.5)

    expect_equal(class(p)[1], "gg")
})

test_that("plot_barcode_hit", {
    object <- get0("object", envir = asNamespace("ScreenR"))

    object@data_table <-
        object@data_table %>%
        dplyr::filter(Gene %in% paste0("Gene_", seq(1, 20)))

    matrix_model <- model.matrix(~ slot(object, "groups"))
    colnames(matrix_model) <- c("Control", "T1_T2", "Treated")
    contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)
    expect_null(plot_barcode_hit(object, matrix_model,
        contrast = contrast,
        gene = "Gene_10"
    ))
})


test_that("Plot number mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, palette)
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Plot number mapped reads", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, NULL)
    expect_equal(class(plot)[2], "ggplot")
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

    expect_equal(class(plot)[2], "ggplot")
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
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Plot number of Barcode Lost", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )

    plot <- plot_barcode_lost(screenR_Object = object, palette = palette)
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Plot common Hit", {
    hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E", "F", "J", "L"))
    hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G", "L"))
    hit_roast <- data.frame(Gene = c("A", "L", "N", "F", "J"))
    plot <- suppressWarnings(plot_common_hit(hit_zscore, hit_camera, hit_zscore,
        show_percentage = FALSE
    ))

    expect_equal("gg", class(plot)[1])
})


