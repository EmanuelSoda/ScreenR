test_that("create_edger_obj uses raw counts aligned to the annotation", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    counts <- object@count_table

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
        control = "TRT",
        treatment = "Time3", day = "Time3"
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
