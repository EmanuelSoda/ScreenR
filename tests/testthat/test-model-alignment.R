alignment_fixture <- function() {
    object <- get0("object", envir = asNamespace("ScreenR"))
    object <- remove_all_zero_row(object)
    design <- create_design_matrix(object)
    list(object = object, design = design,
        contrast = create_contrast(design, "Treated", "Control"))
}

test_that("design matrices identify samples and reject ambiguous coefficients", {
    x <- alignment_fixture()
    expect_identical(rownames(x$design), colnames(x$object))
    expect_identical(ScreenR:::check_design_samples(x$design, x$object), x$design)
    missing <- x$design; rownames(missing) <- NULL
    expect_error(ScreenR:::check_design_samples(missing, x$object), "row names")
    wrong <- x$design; rownames(wrong)[1] <- "unknown_sample"
    expect_error(ScreenR:::check_design_samples(wrong, x$object), "match the sample")
    duplicate <- x$design; rownames(duplicate)[2] <- rownames(duplicate)[1]
    expect_error(ScreenR:::check_design_samples(duplicate, x$object), "unique")
    blank <- x$design; rownames(blank)[1] <- ""
    expect_error(ScreenR:::check_design_samples(blank, x$object), "non-missing")
    short <- x$design[-1, , drop = FALSE]
    expect_error(ScreenR:::check_design_samples(short, x$object), "one row per sample")
    columns <- x$design; colnames(columns)[2] <- colnames(columns)[1]
    expect_error(ScreenR:::as_contrast(x$contrast, columns), "column names")
    invalid <- x$design; invalid[1, 1] <- NA_real_
    expect_error(ScreenR:::check_design_samples(invalid, x$object), "finite numeric")
})

test_that("named contrasts and matrices align to coefficient identifiers", {
    x <- alignment_fixture()
    convert <- function(contrast) ScreenR:::as_contrast(contrast, x$design)
    expect_identical(convert(rev(x$contrast)), x$contrast)
    matrix <- matrix(rev(x$contrast), ncol = 1,
        dimnames = list(rev(names(x$contrast)), "test"))
    expect_identical(convert(matrix), x$contrast)
    expect_identical(convert(unname(x$contrast)), unname(x$contrast))
    for (bad_name in c("", NA_character_, "unknown_column", names(x$contrast)[2])) {
        bad <- x$contrast; names(bad)[1] <- bad_name
        expect_error(convert(bad), "names matching")
    }
    expect_error(convert(x$contrast[-1]), "one value")
    expect_error(convert(rep(0, length(x$contrast))), "non-zero")
    bad <- x$contrast; bad[1] <- Inf
    expect_error(convert(bad), "finite numeric")
    expect_error(convert(0), "column index")
    expect_error(convert(1.5), "column index")
    expect_error(convert(ncol(x$design) + 1), "column index")
    expect_identical(convert("Treated"), "Treated")
    expect_equal(convert(1), 1)
})

test_that("all public model entry points reject sample reordering", {
    x <- alignment_fixture()
    reordered <- x$object[, rev(seq_len(ncol(x$object))) ]
    calls <- list(
        function() find_camera_hit(reordered, x$design, x$contrast),
        function() find_roast_hit(reordered, x$design, x$contrast, method = "fry"),
        function() compute_barcode_statistics(reordered, x$design, x$contrast),
        function() plot_barcode_hit(reordered, x$design, x$contrast,
            gene = "Gene_34"),
        function() filter_by_variance(reordered, genes = "Gene_34",
            matrix_model = x$design, contrast = x$contrast)
    )
    for (call in calls) expect_error(call(), "sample order differs")
    for (method in c("camera", "fry", "roast", "zscore")) {
        expect_error(find_consistent_hit(reordered, x$design,
            list(T3 = x$contrast, T4 = x$contrast), method = method),
            "sample order differs")
    }
    # A stale design after dropping a sample also fails before fitting.
    expect_error(compute_barcode_statistics(x$object[, -1], x$design,
        x$contrast), "one row per sample")
})

test_that("alignment fixes preserve effect directions and model results", {
    x <- alignment_fixture()
    run <- function(object, design, contrast) {
        compute_barcode_statistics(object, design, contrast)
    }
    original <- run(x$object, x$design, x$contrast)
    expect_equal(run(x$object, x$design, rev(x$contrast)), original)
    columns <- rev(seq_len(ncol(x$design)))
    expect_equal(run(x$object, x$design[, columns], x$contrast), original,
        tolerance = 1e-6)
    reordered <- x$object[, rev(seq_len(ncol(x$object))) ]
    result <- run(reordered, create_design_matrix(reordered), x$contrast)
    expect_equal(result, original, tolerance = 1e-6)
    # Explicitly reordering a known design is also safe.
    expect_equal(run(reordered, x$design[colnames(reordered), ], x$contrast),
        original, tolerance = 1e-6)
    original_fry <- find_roast_hit(x$object, x$design, x$contrast,
        method = "fry", fdr = 1, keep_all = TRUE)
    expect_equal(find_roast_hit(x$object, x$design, rev(x$contrast),
        method = "fry", fdr = 1, keep_all = TRUE), original_fry)
    expected <- find_consistent_hit(x$object, x$design,
        list(A = x$contrast, B = x$contrast), fdr = 1)
    expect_equal(find_consistent_hit(x$object, x$design,
        list(A = rev(x$contrast), B = x$contrast), fdr = 1), expected)
})
