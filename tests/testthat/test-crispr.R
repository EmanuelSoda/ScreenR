# Functions for CRISPR screens: barcode thresholds, reference-gene quality
# and the CRISPRcleanR wrapper.

simulate_crispr_object <- function(guides = 4, n_genes = 300,
    drop_fraction = 0) {
    old <- if (exists(".Random.seed", envir = globalenv())) {
        get(".Random.seed", envir = globalenv())
    }
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
    set.seed(3)
    genes <- sprintf("G%03d", seq_len(n_genes))
    gene <- rep(genes, each = guides)
    # Remove one guide from a fraction of the genes
    drop <- which(!duplicated(gene))[seq_len(round(drop_fraction * n_genes))]
    gene <- gene[-drop]
    if (length(drop) == 0) gene <- rep(genes, each = guides)
    n <- length(gene)
    barcode <- sprintf("sg%05d", seq_len(n))
    samples <- c("Day1_CTRL_A", "Day1_CTRL_B", "Day1_TRT_A", "Day1_TRT_B")
    counts <- vapply(samples, function(s) {
        stats::rnbinom(n, mu = 300, size = 30)
    }, numeric(n))
    counts <- data.frame(Barcode = barcode, counts, check.names = FALSE)
    suppressMessages(create_screenr_object(
        table = counts, annotation = data.frame(Barcode = barcode, Gene = gene),
        groups = factor(c("CTRL", "CTRL", "TRT", "TRT"))
    ))
}

test_that("barcodes_per_gene counts the barcodes of each gene", {
    object <- simulate_crispr_object(guides = 4, n_genes = 10)
    per_gene <- barcodes_per_gene(object)
    expect_equal(nrow(per_gene), 10)
    expect_true(all(per_gene$NBarcodes == 4))
})

test_that("a threshold that excludes many genes gives a warning", {
    # 4 sgRNAs per gene, 40% of the genes lost one: number_barcode = 3
    # (genes need more than 3) excludes 40% of the genes
    n_barcodes <- c(rep(4, 60), rep(3, 40))
    expect_warning(
        check_barcode_threshold(n_barcodes, 3, strict = TRUE),
        "40% of the genes .* consider number_barcode = 2"
    )
    expect_warning(
        check_barcode_threshold(n_barcodes, 4, strict = FALSE,
            argument = "min_barcodes"),
        "consider min_barcodes = 3"
    )
    expect_silent(check_barcode_threshold(n_barcodes, 2, strict = TRUE))
    expect_silent(check_barcode_threshold(rep(10, 50), 3, strict = TRUE))
})

test_that("the hit-finding functions warn for CRISPR-sized libraries", {
    object <- simulate_crispr_object(guides = 4, drop_fraction = 0.4)
    object <- compute_data_table(normalize_data(object))
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, treatment = "TRT", control = "CTRL")
    expect_warning(find_camera_hit(object, design, contrast, fdr = 1),
        "consider number_barcode")
    metrics <- compute_metrics(object,
        control = "CTRL", treatment = "TRT", day = "Day1"
    )
    expect_warning(find_gene_zscore_hit(metrics, min_barcodes = 4, fdr = 1,
        n_permutations = 200, seed = 1), "consider min_barcodes")
    expect_warning(find_zscore_hit(metrics), "consider number_barcode")
})

test_that("compute_screen_quality measures the separation", {
    table <- data.frame(
        Gene = rep(c(paste0("E", 1:20), paste0("N", 1:30)), each = 4),
        Log2FC = c(rnorm(80, -2, 0.3), rnorm(120, 0, 0.3))
    )
    essential <- paste0("E", 1:20)
    nonessential <- paste0("N", 1:30)
    quality <- compute_screen_quality(table, essential, nonessential)
    expect_equal(quality$NEssential, 20)
    expect_equal(quality$NNonessential, 30)
    expect_lt(quality$NNMD, -2)
    expect_lt(quality$CohenD, -2)
    expect_equal(quality$AUC, 1)
    by_barcode <- compute_screen_quality(table, essential, nonessential,
        level = "barcode")
    expect_equal(by_barcode$NEssential, 80)
    # Reference genes absent from the table give a clear error
    expect_error(compute_screen_quality(table, "X1", nonessential),
        "two essential")
    expect_error(compute_screen_quality(table, essential, nonessential,
        score = "Zscore"), "no 'Zscore' column")
})

test_that("scale_by_reference sets non-essential to 0 and essential to -1", {
    table <- data.frame(
        Gene = rep(c(paste0("E", 1:10), paste0("N", 1:10), "X"), each = 2),
        Log2FC = c(rep(-3, 20), rep(0.5, 20), c(-1.25, -1.25))
    )
    scaled <- scale_by_reference(table, paste0("E", 1:10),
        paste0("N", 1:10))
    expect_true("ScaledLog2FC" %in% colnames(scaled))
    expect_equal(unique(scaled$ScaledLog2FC[1:20]), -1)
    expect_equal(unique(scaled$ScaledLog2FC[21:40]), 0)
    expect_equal(unique(scaled$ScaledLog2FC[41:42]), -0.5)
})

test_that("plot_reference_separation returns a ggplot", {
    table <- data.frame(
        Gene = rep(c(paste0("E", 1:10), paste0("N", 1:10)), each = 3),
        Log2FC = c(rnorm(30, -2), rnorm(30, 0))
    )
    plot <- plot_reference_separation(table, paste0("E", 1:10),
        paste0("N", 1:10))
    expect_s3_class(plot, "ggplot")
})

test_that("hart_reference_genes has the expected classes", {
    data(hart_reference_genes, package = "ScreenR", envir = environment())
    expect_equal(as.vector(table(hart_reference_genes$Class)), c(684, 926))
    expect_false(any(duplicated(hart_reference_genes$Gene)))
})

test_that("correct_crispr_bias explains how to install CRISPRcleanR", {
    expect_error(crisprcleanr_functions("notAnInstalledPackage123"),
        "BiocManager::install")
    expect_type(crisprcleanr_available(), "logical")
})

test_that("the library annotation is checked", {
    annotation <- data.frame(GENES = "A", CHRM = "1", STARTpos = 1,
        ENDpos = 20, row.names = "sg1")
    expect_error(check_library_annotation(annotation[, -2], "sg1"),
        "lacks the columns CHRM")
    expect_error(check_library_annotation(annotation, "sg2"),
        "row names must be the sgRNA identifiers")
    expect_true(check_library_annotation(annotation, "sg1"))
})

test_that("correct_crispr_bias corrects the counts with CRISPRcleanR", {
    skip_if_not(crisprcleanr_available(), "CRISPRcleanR is not installed")
    skip_on_bioc()
    ccr <- "CRISPRcleanR"
    library_annotation <- get(utils::data("KY_Library_v1.0",
        package = ccr, envir = environment()))
    file <- system.file("extdata", "HT-29_counts.tsv", package = ccr)
    counts <- utils::read.delim(file)
    counts <- counts[library_annotation[counts$sgRNA, "CHRM"] %in% "17", ]
    colnames(counts) <- c("Barcode", "Gene", "Plasmid", "R1", "R2", "R3")
    object <- suppressMessages(create_screenr_object(
        table = counts[, -2], annotation = counts[, c("Barcode", "Gene")],
        groups = factor(c("Plasmid", "Late", "Late", "Late")),
        sample_table = data.frame(Sample = c("Plasmid", "R1", "R2", "R3"),
            Day = "Screen", Treatment = c("Plasmid", "Late", "Late", "Late"))
    ))
    corrected <- suppressWarnings(suppressMessages(correct_crispr_bias(
        object, library_annotation = library_annotation,
        control = "Plasmid", treatment = c("R1", "R2", "R3")
    )))
    expect_s4_class(corrected, "screenr_object")
    expect_equal(colnames(corrected), c("Plasmid", "R1", "R2", "R3"))
    expect_true(all(c("logFCs", "corrected_logFCs", "segments") %in%
        names(S4Vectors::metadata(corrected)$crisprcleanr)))
    expect_error(correct_crispr_bias(object, library_annotation,
        control = "Plasmid", treatment = "R9"), "not found")
})
