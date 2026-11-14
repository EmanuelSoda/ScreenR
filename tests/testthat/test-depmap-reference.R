# SYNTHETIC data: tiny files in the schema of DepMap 24Q2 (first column the
# model ID with an empty header, columns "SYMBOL (EntrezID)"), written to a
# temporary directory. No network and no depmap package are needed: the
# retrieval step is replaced.
write_depmap_fixture <- function(dir) {
    labels <- c("ESS1 (1)", "ESS2 (2)", "NON1 (3)", "NON2 (4)", "MID (5)",
        "MISSING (6)", "DUP (7)", "DUP (8)", "NOENTREZ")
    matrix_csv <- function(file, rows) {
        m <- data.frame(id = names(rows), do.call(rbind, rows))
        colnames(m) <- c("", labels)
        utils::write.csv(m, file.path(dir, file), row.names = FALSE,
            na = "")
    }
    matrix_csv("CRISPRGeneDependency.csv", list(
        "ACH-000001" = c(0.95, 0.5, 0.02, 0.1, 0.3, NA, 0.9, 0.9, 0.9),
        "ACH-000002" = c(0.1, 0.1, 0.9, 0.9, 0.9, 0.9, 0.1, 0.1, 0.1)))
    matrix_csv("CRISPRGeneEffect.csv", list(
        "ACH-000001" = c(-1.5, -0.9, 0.1, 0, -0.3, NA, -1, -1, -1),
        "ACH-000002" = c(0, 0, -1, -1, -1, -1, 0, 0, 0)))
    utils::write.csv(data.frame(
        ModelID = c("ACH-000001", "ACH-000002", "ACH-000003"),
        CellLineName = c("Line-A", "Line B", "line-a"),
        StrippedCellLineName = c("LINEA", "LINEB", "LINEA2")),
        file.path(dir, "Model.csv"), row.names = FALSE)
    stats::setNames(file.path(dir, c("CRISPRGeneDependency.csv",
        "CRISPRGeneEffect.csv", "Model.csv")),
        c("CRISPRGeneDependency.csv", "CRISPRGeneEffect.csv", "Model.csv"))
}

mock_depmap <- function(env = parent.frame()) {
    paths <- write_depmap_fixture(withr::local_tempdir(.local_envir = env))
    testthat::local_mocked_bindings(
        depmap_release_files = function(dataset_id) list(
            title = "Synthetic release", paths = paths,
            files = tibble::tibble(name = names(paths), id = 1:3,
                md5 = "synthetic")),
        .env = env)
}

test_that("genes are classified with disjoint cutoffs and absence stays unclassified", {
    mock_depmap()
    ref <- get_depmap_reference("ACH-000001", dataset_id = 1)
    cls <- stats::setNames(ref$genes$Class, ref$genes$Gene)
    expect_equal(ref$essential, c("ESS1", "ESS2"))   # 0.5 is >= cutoff
    expect_equal(ref$nonessential, c("NON1", "NON2")) # 0.1 is <= cutoff
    expect_equal(cls[["MID"]], "unclassified")
    expect_equal(cls[["MISSING"]], "unclassified")   # NA is not non-essential
    expect_true(is.na(ref$genes$GeneEffect[ref$genes$Gene == "MISSING"]))
    # ambiguous symbol (DUP twice) and unmapped label stay unclassified
    expect_equal(cls[["DUP"]], "unclassified")
    expect_equal(cls[["NOENTREZ"]], "unclassified")
    expect_length(intersect(ref$essential, ref$nonessential), 0)
    expect_false(anyDuplicated(c(ref$essential, ref$nonessential)) > 0)
    expect_equal(ref$genes$EntrezID[1:2], 1:2)
    expect_equal(ref$genes$GeneEffect[1], -1.5)
    p <- ref$provenance
    expect_equal(c(p$model_id, p$cell_line_name, p$release),
        c("ACH-000001", "Line-A", "Synthetic release"))
    expect_equal(c(p$n_genes, p$n_probability, p$n_effect, p$n_essential,
        p$n_nonessential, p$n_unclassified, p$n_ambiguous), c(9, 8, 8, 2, 2, 5, 3))
})

test_that("only the requested model is used and cutoffs are configurable", {
    mock_depmap()
    ref <- get_depmap_reference("LineB", 1, dependency_cutoff = 0.8,
        nondependency_cutoff = 0.2)
    expect_equal(ref$provenance$model_id, "ACH-000002")
    expect_equal(sort(ref$essential), c("MID", "MISSING", "NON1", "NON2"))
    # ambiguous DUP and unmapped NOENTREZ are never classified
    expect_equal(sort(ref$nonessential), c("ESS1", "ESS2"))
})

test_that("models are resolved by ID or unambiguous name; no fallback", {
    mock_depmap()
    expect_equal(get_depmap_reference("line b", 1)$provenance$model_id,
        "ACH-000002")
    # "line-a" matches ACH-000001 (Line-A) and ACH-000003 (line-a)
    expect_error(get_depmap_reference("LINE-A", 1), "ambiguous")
    expect_error(get_depmap_reference("Line-X", 1), "not in Model.csv")
    expect_error(get_depmap_reference("ACH-999999", 1), "not in Model.csv")
})

test_that("a model absent from a matrix is an error, not a fallback", {
    mock_depmap()
    expect_error(get_depmap_reference("ACH-000003", 1),
        "not in CRISPRGeneDependency.csv")
})

test_that("invalid arguments and cutoffs are rejected", {
    expect_error(get_depmap_reference(NA_character_, 1), "cell_line")
    expect_error(get_depmap_reference("A549"), "dataset_id")
    expect_error(get_depmap_reference("A549", c(1, 2)), "dataset_id")
    for (bad in list(c(0.5, 0.5), c(0.3, 0.4), c(1.2, 0.1), c(0.5, -0.1),
        c(NA, 0.1))) {
        expect_error(get_depmap_reference("A549", 1, bad[1], bad[2]),
            "cutoff")
    }
})

test_that("gene effect values are not accepted as probabilities", {
    mock_depmap()
    testthat::local_mocked_bindings(
        depmap_read_model = function(path, model_id)
            c("A (1)" = -1.5, "B (2)" = 0.2))
    expect_error(get_depmap_reference("ACH-000001", 1), "outside")
})

test_that("a missing or old depmap package gives an actionable error", {
    skip_if(requireNamespace("depmap", quietly = TRUE) &&
        all(c("DepMapFiles", "dmget") %in% getNamespaceExports("depmap")))
    expect_error(get_depmap_reference("A549", 1), "BiocManager::install")
})

test_that("the reference works with the reference-gene functions and hit tables", {
    mock_depmap()
    ref <- get_depmap_reference("ACH-000001", 1)
    # SYNTHETIC screen: essential genes depleted, non-essential not
    metrics <- tibble::tibble(
        Gene = rep(c("ESS1", "ESS2", "NON1", "NON2", "MID"), each = 4),
        Log2FC = c(-3, -2.5, -3.2, -2.8, -2, -2.2, -1.8, -2.1,
            0.1, -0.1, 0.2, 0, 0.05, 0.15, -0.2, 0.1, -1, 0, -0.5, -0.4))
    quality <- compute_screen_quality(metrics, ref$essential,
        ref$nonessential)
    expect_equal(c(quality$NEssential, quality$NNonessential), c(2, 2))
    expect_equal(quality$AUC, 1)
    expect_s3_class(plot_reference_separation(metrics, ref$essential,
        ref$nonessential), "ggplot")
    expect_true("ScaledLog2FC" %in% colnames(scale_by_reference(metrics,
        ref$essential, ref$nonessential)))
    hits <- data.frame(Gene = c("ESS1", "MID", "OTHER"))
    annotated <- merge(hits, ref$genes, by = "Gene", all.x = TRUE)
    expect_equal(annotated$Class[match(c("ESS1", "MID", "OTHER"),
        annotated$Gene)], c("essential", "unclassified", NA))
})
