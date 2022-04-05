library(testthat)
library(ScreenR)

create_test_object <- function() {
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E75", "#1B9E75", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    data <- CountTable_THP1_CONTROL_vs_MET %>%
        dplyr::filter(Barcode != "*")

    colnames(data) <- c("Barcode", "T0",
        "T48_postPURO", "Day3_Met_A", "Day3_Met_B", "Day3_Met_C", "Day3_DMSO_A",
        "Day3_DMSO_B", "Day3_DMSO_C", "Day6_Met_A", "Day6_Met_B", "Day6_Met_C",
        "Day6_DMSO_A", "Day6_DMSO_B", "Day6_DMSO_C")
    obj <- create_screenR_object(table = data,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    obj <- normalize_data(obj)
    obj <- compute_data_table(obj)

    return(obj)
}



test_that("Creation of the screenR object", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))
    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    expect_s4_class(object = object, class = "screenR_object")
})



test_that("Normalize Data", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))

    object <- normalize_data(object)
    expect_equal(dim(object@count_table), dim(object@normalized_count_table))
})

test_that("Number mapped reads", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    mapped <- mapped_reads(object)

    expect_equal(is_tibble(mapped), TRUE)
})
# > Test passed 🎉

test_that("Plot number mapped reads", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    plot <- plot_mapped_reads(object, palette)
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Plot number mapped reads", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    plot <- plot_mapped_reads(object, NULL)
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Boxplot mapped reads", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    plot <- distribution_mapped_reads(object, palette, alpha = 0.8,
        type = "boxplot")
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Density mapped reads", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    plot <- distribution_mapped_reads(object, palette, alpha = 0.8,
        type = "density")
    expect_equal(class(plot)[2], "ggplot")
})


test_that("Number of Barcode Lost", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    barcode_lost <- barcode_lost(object)
    expect_equal(is_tibble(barcode_lost), TRUE)
})

test_that("Plot number of Barcode Lost", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))

    plot <- plot_barcode_lost(screenR_Object = object, palette = palette)
    expect_equal(class(plot)[2], "ggplot")
})

test_that("Create data_table", {
    library(tibble)
    groups <- factor(c("T0/T48", "T0/T48", "Treated", "Treated", "Treated",
        "Control", "Control", "Control", "Treated", "Treated", "Treated",
        "Control", "Control", "Control"))


    palette <- c("#1B9E77", "#1B9E77", "#D95F02", "#D95F02", "#D95F02",
        "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#E7298A",
        "#66A61E", "#66A61E", "#66A61E")

    object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
        annotation = Table_Annotation, groups = groups, replicates = c(""))
    object <- normalize_data(object)


    object <- compute_data_table(object)
    expect_equal(class(object@data_table)[1], "tbl_df")
})



test_that("Compute Metrics ", {
    library(tibble)
    object <- create_test_object()

    # In order to speed up the test we will compute the metrics only for a
    # subset of the genes
    genes <- c("SEPT5", "SEPT9", "ACAA1", "CARS", "GPT", "HERC6")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]
    table <- compute_metrics(object, treatment = "Met", control = "DMSO",
        day = "Day3")
    expect_equal(class(table)[1], "tbl_df")
})


test_that("Hit Z-score per giorno", {
    library(tibble)
    object <- create_test_object()

    # In order to speed up the test we will compute the metrics only for a
    # subset of the genes
    genes <- c("SEPT5", "SEPT9", "ACAA1", "CARS", "GPT", "HERC6", "GLS")
    object@data_table <- object@data_table[object@data_table$Gene %in% genes, ]

    table <- compute_metrics(object, treatment = "Met", control = "DMSO",
        day = "Day3")

    hit_table <- find_zscore_hit(table, number_barcode = 2)
    expect_equal(class(hit_table)[1], "tbl_df")
})



test_that("Plot MDS NULL", {
    library(tibble)
    object <- create_test_object()
    plot <- suppressWarnings(plot_MDS(screenR_Object = object))
    expect_equal(class(plot)[1], "gg")
})

test_that("Plot MDS", {
    library(tibble)
    object <- create_test_object()
    plot <- suppressWarnings(plot_MDS(screenR_Object = object,
        groups = c(rep("T0/T48", 2), rep("Day3", 6), rep("Day",
            6))))
    expect_equal(class(plot)[1], "gg")
})

test_that("Camera", {
    library(tibble)

    object <- create_test_object()

    matrix <- model.matrix(~object@groups)
    colnames(matrix) <- c("Control", "T0/T48", "Treated")
    camera_hit <- suppressWarnings(find_camera_hit(screenR_Object = object,
        matrix_model = matrix, contrast = "Treated"))
    expect_equal(class(camera_hit)[1], "tbl_df")
})

test_that("ROAST", {
    testthat::skip_on_cran()
    testthat::skip_on_bioc()
    library(tibble)

    object <- create_test_object()

    matrix <- model.matrix(~object@groups)
    colnames(matrix) <- c("Control", "T0/T48", "Treated")

    roast_hit <- suppressWarnings(find_roast_hit(screenR_Object = object,
        matrix_model = matrix, contrast = "Treated"))
    expect_equal(class(roast_hit)[[1]], "tbl_df")
})


test_that("find_common_hit 2", {
     hit_zscore <- data.frame(Gene = c('A', 'B', 'C', 'D', 'E'))
     hit_camera <- data.frame(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
     hit_roast <- data.frame(Gene = c('A', 'L', 'N'))

     find_common_hit <-
         find_common_hit(hit_zscore, hit_camera, hit_zscore,
                         common_in = 2)
    expect_equal(class(find_common_hit), "character")
})

test_that("find_common_hit 3", {
    hit_zscore <- data.frame(Gene = c('A', 'B', 'C', 'D', 'E'))
    hit_camera <- data.frame(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
    hit_roast <- data.frame(Gene = c('A', 'L', 'N'))

    find_common_hit <- find_common_hit(hit_zscore, hit_camera, hit_zscore,
        common_in = 3)
    expect_equal(class(find_common_hit), "character")
})


test_that("Plot common Hit", {
    hit_zscore <- data.frame(Gene = c('A', 'B', 'C', 'D', 'E', 'F', 'J', 'L'))
    hit_camera <- data.frame(Gene = c('A', 'B', 'C', 'F', 'H', 'G', 'L'))
    hit_roast <- data.frame(Gene = c('A', 'L', 'N', 'F', 'J'))
    plot <- suppressWarnings(plot_common_hit(hit_zscore, hit_camera, hit_zscore,
        show_percentage = FALSE))

    expect_equal("gg", class(plot)[1])
})

test_that("Find_Score_hit mean", {
    library(tibble)
    object <- create_test_object()

    table <- compute_metrics(object, control = "Met", treatment = "DMSO",
        day = c("Day3"))

    hit_zscore <- find_zscore_hit(table, number_barcode = 6, metric = "mean")

    expect_equal(class(hit_zscore)[[1]], "tbl_df")
})

test_that("Find_Score_hit median ", {
    object <- create_test_object()

    table <- compute_metrics(object, control = "Met", treatment = "DMSO",
        day = c("Day3"))
    hit_zscore <- find_zscore_hit(table, number_barcode = 6)

    expect_equal(class(hit_zscore)[[1]], "tbl_df")
})

test_that("find_robust_zscore_hit median ", {
    library(tibble)
    object <- create_test_object()

    table <- compute_metrics(object, control = "Met", treatment = "DMSO",
        day = c("Day3"))
    hit_zscore_R <- find_robust_zscore_hit(table, number_barcode = 6)
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

    plot <- plot_PC_explained_variance(screenR_Object = object,
        cumulative = TRUE)
    expect_equal(class(plot)[[1]], "gg")
})

test_that("plot trend hit", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_trend(screenR_Object = object, genes = c("SEPT5", "GLS"),
        group_var = c("T0", "T48", "Met"), nrow = 1, ncol = 2)
    expect_equal(class(plot)[[1]], "gg")
})

test_that("filter_by_slope", {
    library(tibble)
    object <- create_test_object()

    data <- filter_by_slope(screenR_Object = object, genes = c("SEPT5", "GLS"),
        group_var_treatment = c("T0", "T48", "Met"), group_var_control = c("T0",
            "T48", "Day3", "Day6"), slope_treatment = 1)
    expect_equal(class(data)[1], "tbl_df")

})


test_that("filter_by_variance", {
    library(tibble)
    object <- create_test_object()

    matrix_model <- model.matrix(~object@groups)
    colnames(matrix_model) <- c("Control", "T0_T48", "Treated")


    contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)
    data <- filter_by_variance(genes = c("SEPT5"), screenR_Object = object,
        matrix_model = matrix_model, contrast = contrast)

    expect_equal(class(data)[1], "tbl_df")

})

test_that("Plot Boxplot", {
    library(tibble)
    object <- create_test_object()

    plot_boxplot(screenR_Object = object, genes = c("SEPT5", "GLS"),
        group_var = c("T0", "T48", "Met"), nrow = 1, ncol = 2, fill_var = "Day",
        type = "violinplot")
})



