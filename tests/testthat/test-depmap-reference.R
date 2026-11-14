# SYNTHETIC data: tiny files in the schema of DepMap 24Q2 (first column the
# model ID with an empty header, columns "SYMBOL (EntrezID)"), shipped in
# inst/extdata/depmap-synthetic. No network and no depmap package are needed.
files_names <- c("CRISPRGeneDependency.csv", "CRISPRGeneEffect.csv",
    "Model.csv")
fixture_files <- function() {
    stats::setNames(file.path(system.file("extdata", "depmap-synthetic",
        package = "ScreenR"), files_names), files_names)
}

# The Figshare route with the retrieval step replaced by the local files.
mock_depmap <- function(env = parent.frame()) {
    paths <- fixture_files()
    testthat::local_mocked_bindings(
        depmap_release_files = function(dataset_id) list(
            title = "Synthetic release", paths = paths,
            files = tibble::tibble(name = names(paths), id = 1:3,
                md5 = "synthetic")),
        .env = env)
}

local_ref <- function(cell_line, ...) {
    get_depmap_reference(cell_line, files = fixture_files(),
        release = "Synthetic local", source = "inst/extdata", ...)
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
    expect_error(get_depmap_reference("A549"), "either")
    expect_error(get_depmap_reference("A549", c(1, 2)), "dataset_id")
    # a fractional or out-of-range ID must not be truncated by depmap
    expect_error(get_depmap_reference("A549", 25880521.9), "integer")
    expect_error(get_depmap_reference("A549", 2^31), "integer")
    expect_error(get_depmap_reference("A549", 0), "integer")
    for (bad in list(c(0.5, 0.5), c(0.3, 0.4), c(1.2, 0.1), c(0.5, -0.1),
        c(NA, 0.1))) {
        expect_error(get_depmap_reference("A549", 1, dependency_cutoff = bad[1],
            nondependency_cutoff = bad[2]),
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
    expect_error(get_depmap_reference("A549", 1), "735fa8971fd79ec7")
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

test_that("a safe join by symbol keeps one row per hit and ambiguous symbols unknown", {
    ref <- local_ref("ACH-000001")
    # the raw table keeps both measurements of the ambiguous symbol DUP
    expect_equal(sum(ref$genes$Gene == "DUP"), 2)
    hits <- data.frame(Gene = c("ESS1", "DUP", "OTHER"))
    genes <- ref$genes
    safe <- genes[!(duplicated(genes$Gene) |
        duplicated(genes$Gene, fromLast = TRUE)), ]
    annotated <- dplyr::left_join(hits, safe, by = "Gene")
    expect_equal(nrow(annotated), nrow(hits))
    expect_equal(annotated$Class, c("essential", NA, NA))
    # the unsafe merge shows what the safe join prevents
    expect_equal(nrow(merge(hits, genes, by = "Gene", all.x = TRUE)), 4)
})

test_that("labels without a valid symbol and positive Entrez ID stay unclassified", {
    dir <- withr::local_tempdir()
    labels <- c("OK (10)", "OVERFLOW (999999999999)", "ZERO (0)", " (11)",
        "NOID", "SPACE  (12)", "TRIM (13)", "(14)")
    write_matrix <- function(file, value) {
        m <- data.frame("ACH-000001", t(rep(value, length(labels))))
        colnames(m) <- c("", labels)
        utils::write.csv(m, file.path(dir, file), row.names = FALSE)
    }
    write_matrix("CRISPRGeneDependency.csv", 0.99)
    write_matrix("CRISPRGeneEffect.csv", -2)
    file.copy(fixture_files()[["Model.csv"]], dir)
    ref <- get_depmap_reference("ACH-000001",
        files = stats::setNames(file.path(dir, files_names), files_names),
        release = "Synthetic", source = "tempdir")
    expect_equal(ref$essential, c("OK", "SPACE", "TRIM"))
    expect_equal(ref$nonessential, character(0))
    expect_equal(nrow(ref$genes), length(labels))  # evidence rows are kept
    expect_equal(ref$genes$GeneEffect, rep(-2, length(labels)))
    expect_equal(ref$provenance$n_ambiguous, 5)
})

test_that("the local files route gives the same reference as the release route", {
    mock_depmap()
    local <- local_ref("ACH-000001")
    release <- get_depmap_reference("ACH-000001", dataset_id = 1)
    expect_equal(local$genes, release$genes)
    expect_equal(local$essential, release$essential)
    p <- local$provenance
    expect_equal(c(p$release, p$source), c("Synthetic local", "inst/extdata"))
    expect_true(is.na(p$dataset_id))
    expect_equal(p$files$name, files_names)
    expect_equal(p$files$md5, unname(tools::md5sum(fixture_files())))
    expect_equal(release$provenance$dataset_id, 1L)
    expect_equal(release$provenance$source, "Figshare dataset 1")
})

test_that("the local files route validates its inputs and does not need depmap", {
    files <- fixture_files()
    expect_error(get_depmap_reference("ACH-000001", dataset_id = 1,
        files = files, release = "r", source = "s"), "either")
    expect_error(get_depmap_reference("ACH-000001", files = files,
        source = "s"), "release")
    expect_error(get_depmap_reference("ACH-000001", files = files,
        release = "r", source = " "), "source")
    expect_error(get_depmap_reference("ACH-000001", files = files[1:2],
        release = "r", source = "s"), "named by file name")
    expect_error(get_depmap_reference("ACH-000001",
        files = unname(files), release = "r", source = "s"), "named")
    expect_error(get_depmap_reference("ACH-000009", files = files,
        release = "r", source = "s"), "not in Model.csv")
})
