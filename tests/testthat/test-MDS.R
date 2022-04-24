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
test_that("plot_mds NULL", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_mds(screenR_Object = object))
    expect_equal(class(plot)[1], "gg")
})

test_that("plot_mds", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_mds(
        screenR_Object = object,
        groups = c(rep("T1/T2", 2), rep("Time3", 6), rep("Time4", 6))
    ))
    expect_equal(class(plot)[1], "gg")
})


test_that("plot_pc_explained_variance", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_pc_explained_variance(object, cumulative = FALSE)
    expect_equal(class(plot)[[1]], "gg")
})

test_that("plot_pc_explained_variance CUM", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_pc_explained_variance(
        screenR_Object = object,
        cumulative = TRUE
    )
    expect_equal(class(plot)[[1]], "gg")
})
