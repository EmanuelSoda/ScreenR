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


test_that("filter_by_slope", {
    library(tibble)
    object <- create_test_object()

    data <- filter_by_slope(
        screenR_Object = object,
        genes = c("Gene_1", "Gene_300"),
        group_var_treatment = c("T1", "T2", "TRT"),
        group_var_control = c("T1", "T2", "Time3", "Time4"),
        slope_treatment = 1,
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



test_that("remove_all_zero_row", {
    library(tibble)
    object <- get0("object", envir = asNamespace("ScreenR"))

    object <- remove_all_zero_row(object)
    n_row <- nrow(object@count_table)
    expect_equal(n_row, 5317)
})

