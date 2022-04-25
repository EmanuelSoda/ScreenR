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
test_that("Creation of the screenR object", {
    object <- create_screenr_object(
        table = data,
        annotation = annotaion, groups = groups, replicates = c("")
    )
    expect_s4_class(object = object, class = "screenr_object")
})

test_that("get_count_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts <- get_count_table(object)
    expect_equal(class(counts)[1], "tbl_df")
})

test_that("get_count_table NULL", {
    expect_error(get_count_table(NULL))
})

test_that("get_annotation_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    annotation <- get_annotation_table(object)
    expect_equal(class(annotation)[1], "tbl_df")
})

test_that("get_annotation_table NULL", {
    expect_error(get_annotation_table(NULL))
})


test_that("get_groups", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    grp <- get_groups(object)
    expect_equal(class(grp)[1], "factor")
})

test_that("get_groups NULL", {
    expect_error(get_groups(NULL))
})


test_that("get_replicates", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    reps <- get_replicates(object)
    expect_equal(class(reps)[1], "character")
})

test_that("get_replicates NULL", {
    expect_error(get_replicates(NULL))
})



test_that("get_normalized_count_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts_norm <- get_normalized_count_table(object)
    expect_equal(class(counts_norm)[1], "tbl_df")
})

test_that("get_normalized_count_table NULL", {
    expect_error(get_normalized_count_table(NULL))
})




test_that("get_data_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    data_t <- get_data_table(object)
    expect_equal(class(data_t)[1], "tbl_df")
})

test_that("get_data_table NULL", {
    expect_error(get_data_table(NULL))
})

# Set annotation table
test_that("set_annotation_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    annotation <- get_annotation_table(object)
    expect_equal(class(annotation)[1], "tbl_df")
})

test_that("set_annotation_table NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    annotation <- get_annotation_table(object)
    expect_error(set_annotation_table(object = NULL, annotation))
})

# Set Count Table
test_that("set_count_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    count <- get_count_table(object)
    expect_silent(set_count_table(object, count))
})

test_that("set_count_table NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    count <- get_count_table(object)
    expect_error(set_count_table(object = NULL, count))
})


# Set groups
test_that("set_groups", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    groups <- get_groups(object)
    expect_silent(set_groups(object, groups))
})

test_that("set_groups NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    groups <- get_groups(object)
    expect_error(set_groups(object = NULL, groups))
})



# Set replicates
test_that("set_replicates", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    replicates <- get_replicates(object)
    expect_silent(set_replicates(object, replicates))
})

test_that("set_replicates NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    replicates <- get_replicates(object)
    expect_error(set_replicates(object = NULL, replicates))
})


# Set normalized_count_table
test_that("set_normalized_count_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts_norm <- get_normalized_count_table(object)
    expect_silent(set_normalized_count_table(object, counts_norm))
})

test_that("set_normalized_count_table NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts_norm <- get_normalized_count_table(object)
    expect_error(set_normalized_count_table(object = NULL, counts_norm))
})



# Set data_table
test_that("set_normalized_count_table", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    data_table <- get_data_table(object)
    expect_silent(set_data_table(object, data_table))
})

test_that("set_normalized_count_table NULL", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    data_table <- get_data_table(object)
    expect_error(set_data_table(object = NULL, data_table))
})

