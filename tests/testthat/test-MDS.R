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

    # Keep only the barcodes of the data table (counts and normalized
    # counts are subset together)
    obj <- obj[SummarizedExperiment::rowData(obj)$Barcode %in%
        obj@data_table$Barcode, ]

    obj@annotation_table <- obj@annotation_table %>%
        dplyr::filter(Barcode %in% obj@data_table$Barcode)

    return(obj)
}
test_that("plot_mds NULL", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_mds(screenR_Object = object))
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_mds", {
    object <- create_test_object()
    plot <- suppressWarnings(plot_mds(
        screenR_Object = object,
        groups = c(rep("T1/T2", 2), rep("Time3", 6), rep("Time4", 6))
    ))
    expect_true(ggplot2::is_ggplot(plot))
})


test_that("plot_explained_variance", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_explained_variance(object, cumulative = FALSE)
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_explained_variance CUM", {
    library(tibble)
    object <- create_test_object()

    plot <- plot_explained_variance(
        screenR_Object = object,
        cumulative = TRUE
    )
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("the PCA does not depend on the sequencing depth", {
    data(hap1_count_table, package = "ScreenR", envir = environment())
    data(hap1_annotation_table, package = "ScreenR", envir = environment())
    make_object <- function(table) {
        create_screenr_object(
            table = table, annotation = hap1_annotation_table,
            groups = factor(c("T0", "T3", "T3", "T3", "T18", "T18", "T18")),
            sample_table = data.frame(
                Sample = colnames(table)[-1], Day = "Screen",
                Treatment = c("T0", "T3", "T3", "T3", "T18", "T18", "T18")
            )
        )
    }
    deeper <- hap1_count_table
    deeper$T0 <- deeper$T0 * 10
    variance <- compute_explained_variance(make_object(hap1_count_table))
    variance_deeper <- compute_explained_variance(make_object(deeper))
    # Sequencing T0 ten times more deeply barely changes the components (on
    # the raw counts PC1 would go from 71% to almost 100%)
    difference <- unlist(variance[2, -1]) - unlist(variance_deeper[2, -1])
    expect_lt(max(abs(difference)), 0.02)
})

test_that("plot_pca", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    plot <- plot_pca(object)
    expect_true(ggplot2::is_ggplot(plot))
    expect_match(plot$labels$x, "^PC1 \\([0-9]+%\\)$")
    expect_match(plot$labels$y, "^PC2 \\([0-9]+%\\)$")
    expect_equal(nrow(plot$data), 14)

    plot <- plot_pca(object, components = c(2, 3))
    expect_match(plot$labels$x, "^PC2")

    expect_error(plot_pca(object, components = c(1, 99)), "components")
    expect_error(plot_pca(object, groups = c("a", "b")), "groups")
})

test_that("the PCA and the MDS use the normalization of normalize_data()", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts <- count_table_of(object)
    controls <- counts$Barcode[seq(1, nrow(counts), by = 10)]

    cpm <- normalize_data(object, method = "cpm")
    control <- normalize_data(object, method = "control",
        control_barcodes = controls)
    # Without normalize_data() the counts per million are used
    expect_equal(log_normalized_counts(object), log_normalized_counts(cpm))
    expect_equal(compute_explained_variance(object),
        compute_explained_variance(cpm))
    # Another normalization gives other values
    expect_false(isTRUE(all.equal(log_normalized_counts(cpm),
        log_normalized_counts(control))))
    expect_false(isTRUE(all.equal(plot_mds(cpm)$data,
        plot_mds(control)$data)))
})
