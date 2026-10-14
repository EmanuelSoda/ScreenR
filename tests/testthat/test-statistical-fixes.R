test_that("create_edger_obj uses raw counts aligned to the annotation", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts <- ScreenR:::count_table_of(object)

    # Shuffle the annotation: genes must still be matched by Barcode
    set.seed(1)
    object@annotation_table <-
        object@annotation_table[sample(nrow(object@annotation_table)), ]

    dge <- create_edger_obj(object)
    raw_lib_sizes <- colSums(dplyr::select_if(counts, is.numeric))

    expect_equal(unname(dge$samples$lib.size), unname(raw_lib_sizes))
    expect_equal(
        as.character(dge$genes$Barcode),
        as.character(counts$Barcode)
    )
})

test_that("create_edger_obj fails when barcodes are missing", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    object@annotation_table <- object@annotation_table[-1, ]
    expect_error(create_edger_obj(object), "not in the annotation")
})

test_that("unique_gene_symbols keeps only genes with enough barcodes", {
    genes <- c(rep("A", 5), rep("B", 2), NA, "C")

    res <- ScreenR:::unique_gene_symbols(genes, number_barcode = 3)
    expect_equal(names(res), "A")
    expect_equal(res$A, seq_len(5))

    res <- ScreenR:::unique_gene_symbols(genes, number_barcode = 1)
    expect_equal(names(res), c("A", "B"))

    expect_length(ScreenR:::unique_gene_symbols(genes, number_barcode = 10), 0)
})

test_that("compute_metrics computes the standard robust Z-score", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    metrics <- compute_metrics(object,
        control = "Time3",
        treatment = "TRT", day = "Time3"
    )
    x <- metrics$Log2FC
    expect_equal(metrics$ZscoreRobust, (x - median(x)) / stats::mad(x))
    expect_equal(median(metrics$ZscoreRobust), 0)
})

test_that("compute_slope regresses on time points, not row order", {
    samples <- c("D1_TRT_A", "D1_TRT_B", "D2_TRT_A", "D2_TRT_B", "D3_TRT_A")
    days <- c("D1", "D1", "D2", "D2", "D3")

    # Two barcodes of the same gene, each increasing by 10 per time point
    data_table <- tibble::tibble(
        Barcode = rep(c("b1", "b2"), each = length(samples)),
        Gene = "G",
        Sample = factor(rep(samples, 2), levels = samples),
        Day = rep(days, 2),
        Treatment = "TRT",
        Frequency = rep(10 * match(days, unique(days)), 2) +
            rep(c(0, 100), each = length(samples))
    )
    object <- create_screenr_object()
    object@data_table <- data_table

    slope <- compute_slope(object, genes = "G", group_var = "TRT")
    expect_equal(unname(slope$Slope), 10)
})

test_that("find_zscore_hit checks the metric argument", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    metrics <- compute_metrics(object,
        control = "Time3",
        treatment = "TRT", day = "Time3"
    )
    expect_error(find_zscore_hit(metrics, metric = "mode"), "should be one of")
    expect_equal(
        find_zscore_hit(metrics),
        find_zscore_hit(metrics, metric = "median")
    )
    expect_s3_class(find_zscore_hit(metrics, metric = "mean"), "tbl_df")
})

test_that("find_robust_zscore_hit has a default number_barcode", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    metrics <- compute_metrics(object,
        control = "Time3",
        treatment = "TRT", day = "Time3"
    )
    expect_equal(
        find_robust_zscore_hit(metrics),
        find_robust_zscore_hit(metrics, number_barcode = 6)
    )
})

test_that("find_camera_hit warns only when thresh or lfc are given", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    matrix <- model.matrix(~ slot(object, "groups"))
    colnames(matrix) <- c("Control", "T1_T2", "Treated")

    deprecation <- "'thresh' and 'lfc' arguments"
    expect_warning(
        find_camera_hit(object, matrix, contrast = "Treated", thresh = 0.05),
        deprecation
    )
    expect_warning(
        find_camera_hit(object, matrix, contrast = "Treated", lfc = 2),
        deprecation
    )

    # Without thresh and lfc there is no deprecation warning
    messages <- character()
    withCallingHandlers(
        find_camera_hit(object, matrix, contrast = "Treated"),
        warning = function(w) {
            messages <<- c(messages, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_false(any(grepl(deprecation, messages)))
})

test_that("plot_barcode_hit errors for a gene without enough barcodes", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    matrix_model <- create_design_matrix(object)
    contrast <- create_contrast(matrix_model,
        treatment = "Treated", control = "Control"
    )

    expect_error(
        plot_barcode_hit(object, matrix_model,
            contrast = contrast,
            gene = "Not_a_gene"
        ),
        "Not_a_gene"
    )
})
