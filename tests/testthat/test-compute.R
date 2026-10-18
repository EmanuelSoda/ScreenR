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

test_that("Compute Metrics ", {
    object <- create_test_object()

    # In order to speed up the test we will compute the metrics only for a
    # subset of the genes
    genes <- c("Gene_1", "Gene_5")
    object <- object[SummarizedExperiment::rowData(object)$Gene %in% genes, ]
    table <- compute_metrics(object,
        treatment = "TRT", control = "Time3",
        day = "Time3"
    )
    expect_equal(class(table)[1], "tbl_df")
})

test_that("Camera", {
    object <- create_test_object()

    matrix <- model.matrix(~ get_groups(object))
    rownames(matrix) <- colnames(object)
    colnames(matrix) <- c("Control", "T1/T2", "Treated")
    camera_hit <- suppressWarnings(find_camera_hit(
        screenR_Object = object,
        matrix_model = matrix, contrast = "Treated"
    ))
    expect_equal(class(camera_hit)[1], "tbl_df")
})



test_that("Number of Barcode Lost", {
    library(tibble)

    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    barcode_lost <- barcode_lost(object)
    expect_equal(is_tibble(barcode_lost), TRUE)
})


test_that("Create data_table", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    object <- normalize_data(object)


    object <- compute_data_table(object)
    expect_equal(class(suppressMessages(get_data_table(object)))[1], "tbl_df")
})


test_that("Normalize Data", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )

    object <- normalize_data(object)
    expect_equal(dim(ScreenR:::count_table_of(object)), dim(ScreenR:::normalized_table_of(object)))
})

test_that("Number mapped reads", {
    library(tibble)
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    mapped <- mapped_reads(object)

    expect_equal(is_tibble(mapped), TRUE)
})
