library(testthat)
library(ScreenR)
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
    obj <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    obj <- normalize_data(obj)
    obj <- compute_data_table(obj)

    return(obj)
}



test_that("Creation of the screenR object", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    expect_s4_class(object = object, class = "screenR_object")
})



test_that("Normalize Data", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )

    object <- normalize_data(object)
    expect_equal(dim(object@count_table), dim(object@normalized_count_table))
})

test_that("Number mapped reads", {
    library(tibble)
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    mapped <- mapped_reads(object)

    expect_equal(is_tibble(mapped), TRUE)
})


test_that("Plot number mapped reads", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, palette)
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Plot number mapped reads", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- plot_mapped_reads(object, NULL)
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Boxplot mapped reads", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- distribution_mapped_reads(object, palette,
        alpha = 0.8,
        type = "boxplot"
    )
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Density mapped reads", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    plot <- distribution_mapped_reads(object, palette,
        alpha = 0.8,
        type = "density"
    )
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Number of Barcode Lost", {
    library(tibble)

    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    barcode_lost <- barcode_lost(object)
    expect_equal(is_tibble(barcode_lost), TRUE)
})

test_that("Plot number of Barcode Lost", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )

    plot <- plot_barcode_lost(screenR_Object = object, palette = palette)
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Create data_table", {
    object <- create_screenR_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    object <- normalize_data(object)


    object <- compute_data_table(object)
    expect_equal(class(object@data_table)[1], "tbl_df")
})



test_that("Compute Metrics ", {
    object <- create_test_object()

    # In order to speed up the test we will compute the metrics only for a
    # subset of the genes
    genes <- c("Gene_1", "Gene_300", "Gene_10", "Gene_15", "Gene_3", "Gene_30")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]
    table <- compute_metrics(object,
        treatment = "TRT", control = "Time3",
        day = "Time3"
    )
    expect_equal(class(table)[1], "tbl_df")
})


test_that("Hit Z-score per giorno", {
    object <- create_test_object()

    # In order to speed up the test we will compute the metrics only for a
    # subset of the genes
    genes <- c("Gene_1", "Gene_300", "Gene_10", "Gene_15", "Gene_3", "Gene_30")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    table <- compute_metrics(object,
        treatment = "TRT", control = "Time3",
        day = "Time3"
    )

    hit_table <- find_zscore_hit(table, number_barcode = 2)
    expect_equal(class(hit_table)[1], "tbl_df")
})



test_that("Plot MDS NULL", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_MDS(screenR_Object = object))
    expect_equal(class(plot)[1], "gg")
})

test_that("Plot MDS", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_MDS(
        screenR_Object = object,
        groups = c(rep("T1/T2", 2), rep("Time3", 6), rep("Time4", 6))
    ))
    expect_equal(class(plot)[1], "gg")
})

test_that("Camera", {
    object <- create_test_object()

    matrix <- model.matrix(~ object@groups)
    colnames(matrix) <- c("Control", "T1/T2", "Treated")
    camera_hit <- suppressWarnings(find_camera_hit(
        screenR_Object = object,
        matrix_model = matrix, contrast = "Treated"
    ))
    expect_equal(class(camera_hit)[1], "tbl_df")
})

test_that("ROAST", {
    testthat::skip_on_cran()
    testthat::skip_on_bioc()

    object <- create_test_object()

    matrix <- model.matrix(~ object@groups)
    colnames(matrix) <- c("Control", "T1/T2", "Treated")

    roast_hit <- suppressWarnings(find_roast_hit(
        screenR_Object = object,
        matrix_model = matrix, contrast = "Treated"
    ))
    expect_equal(class(roast_hit)[[1]], "tbl_df")
})


# test_that("find_common_hit 2", {
#     testthat::skip_on_bioc()
#     testthat::skip_on_cran()
#     hit_zscore <- data.frame(Gene = c('A', 'B', 'C', 'D', 'E'))
#     hit_camera <- data.frame(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
#     hit_roast <- data.frame(Gene = c('A', 'L', 'N'))
#
#     common_hit <-
#         find_common_hit(hit_zscore, hit_camera, hit_zscore, common_in = 2)
#     #expect_equal(class(find_common_hit), "character")
#     expect_equal(common_hit, c("A","B", "C"))
# })
#
# test_that("find_common_hit 3", {
#     hit_zscore <- data.frame(Gene = c('A', 'B', 'C', 'D', 'E'))
#     hit_camera <- data.frame(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
#     hit_roast <- data.frame(Gene = c('A', 'L', 'N'))
#
#     common_hit <- find_common_hit(hit_zscore, hit_camera, hit_zscore, common_in = 3)
#     #expect_equal(class(find_common_hit), "character")
#     expect_equal(common_hit, c("A"))
# })


test_that("Plot common Hit", {
    hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E", "F", "J", "L"))
    hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G", "L"))
    hit_roast <- data.frame(Gene = c("A", "L", "N", "F", "J"))
    plot <- suppressWarnings(plot_common_hit(hit_zscore, hit_camera, hit_zscore,
        show_percentage = FALSE
    ))

    expect_equal("gg", class(plot)[1])
})

test_that("Find_Score_hit mean", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_300", "Gene_10", "Gene_15", "Gene_3", "Gene_30")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    table <- compute_metrics(object,
        control = "Time3", treatment = "TRT",
        day = c("Time3")
    )

    hit_zscore <- find_zscore_hit(table, number_barcode = 2, metric = "mean")

    expect_equal(class(hit_zscore)[[1]], "tbl_df")
})

test_that("Find_Score_hit median ", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_300", "Gene_10", "Gene_15", "Gene_3", "Gene_30")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    table <- compute_metrics(object,
        control = "Time3", treatment = "TRT",
        day = c("Time3")
    )
    hit_zscore <- find_zscore_hit(table, number_barcode = 4)

    expect_equal(class(hit_zscore)[[1]], "tbl_df")
})

test_that("find_robust_zscore_hit median ", {
    object <- create_test_object()

    genes <- c("Gene_1", "Gene_300", "Gene_10", "Gene_15", "Gene_3", "Gene_30")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    table <- compute_metrics(object,
        control = "Time3", treatment = "TRT",
        day = c("Time3")
    )
    hit_zscore_R <- find_robust_zscore_hit(table, number_barcode = 2)
    expect_equal(class(hit_zscore_R)[[1]], "grouped_df")
})


# test_that('Plot barcode hit ', { library(tibble) object <-
# create_test_object() matrix <- model.matrix(~object@groups) colnames(matrix)
# <- c('Control', 'T0_T48', 'Treated') table <- compute_metrics(object, control
# = 'Met', treatment ='DMSO', day = c('Day3')) hit_zscore_R <-
# find_robust_zscore_hit(table, number_barcode = 6) hit <- c('ACACB', 'ACLY',
# 'ACOX2', 'ACSL6') plot <- plot_barcode_hit(screenR_Object = object,
# matrix_model = matrix, hit_common = hit, contrast = 'Treated') })


test_that("plot_PC_explained_variance", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_PC_explained_variance(object, cumulative = FALSE)
    expect_equal(class(plot)[[1]], "gg")
})

test_that("plot_PC_explained_variance CUM", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_PC_explained_variance(
        screenR_Object = object,
        cumulative = TRUE
    )
    expect_equal(class(plot)[[1]], "gg")
})

test_that("plot trend hit", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_trend(
        screenR_Object = object, genes = c("Gene_1", "Gene_300"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2
    )
    expect_equal(class(plot)[[1]], "gg")
})

test_that("filter_by_slope", {
    library(tibble)
    object <- create_test_object()

    data <- filter_by_slope(
        screenR_Object = object,
        genes = c("Gene_1", "Gene_300"),
        group_var_treatment = c("T1", "T2", "TRT"),
        group_var_control = c("T1", "T2", "Time3", "Time4"),
        slope_treatment = 1
    )
    expect_equal(class(data)[1], "tbl_df")
})


test_that("filter_by_variance", {
    library(tibble)
    object <- create_test_object()

    matrix_model <- model.matrix(~ object@groups)
    colnames(matrix_model) <- c("Control", "T1_T2", "Treated")


    contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)
    data <- filter_by_variance(
        genes = c("Gene_320"), screenR_Object = object,
        matrix_model = matrix_model, contrast = contrast
    )

    expect_equal(class(data)[1], "tbl_df")
})

test_that("Plot Boxplot", {
    library(tibble)
    object <- create_test_object()

    p <- plot_boxplot(
        screenR_Object = object, genes = c("Gene_320", "Gene_32"),
        group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "violinplot"
    )

    expect_equal(class(p)[1], "gg")
})
