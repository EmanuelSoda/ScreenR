test_that("the conjunction combines raw p-values by gene before adjustment", {
    first <- data.frame(Gene = c("A", "B", "C"),
        PValue = c(.001, .001, .8), FDR = 0,
        Log2FC = c(-1, -2, 1))
    second <- data.frame(Gene = c("C", "A", "B"),
        PValue = c(.001, .002, .9), FDR = 0, Log2FC = c(-1, -1, 2))
    out <- combine_consistent_hits(list(T3 = first, T4 = second), fdr = 1)
    expect_equal(out$Gene, c("A", "C", "B"))
    expect_equal(out$PValue, c(.002, .8, .9))
    expect_equal(out$FDR, p.adjust(c(.002, .8, .9), "BH"))
    expect_equal(out$Log2FC_T4, c(-1, -1, 2))
    expect_equal(combine_consistent_hits(list(T3 = first, T4 = second))$Gene,
        "A")
    third <- first
    third$PValue <- c(.7, .99, .4)
    all_three <- combine_consistent_hits(list(T3 = first, T4 = second,
        T5 = third), fdr = 1, fdr_method = "BY")
    expect_equal(all_three$PValue, c(.7, .8, .99))
    expect_equal(all_three$FDR, p.adjust(all_three$PValue, "BY"))
    expect_true(all(all_three$NContrasts == 3))
})

test_that("incomplete or ambiguous inputs cannot become consistency claims", {
    x <- data.frame(Gene = c("A", "B"), PValue = c(.01, .5))
    expect_error(combine_consistent_hits(list(x, x)), "uniquely named")
    expect_error(combine_consistent_hits(list(A = x)), "at least two")
    expect_error(combine_consistent_hits(list(A = x, A = x)), "uniquely named")
    expect_error(combine_consistent_hits(list(A = x, B = x[1, ])), "same genes")
    expect_error(combine_consistent_hits(list(A = x, B = x[c(1, 1), ])),
        "unique and non-missing")
    for (bad in list(NA_real_, Inf, -.1, 1.1, "0.1")) {
        y <- x; y$PValue[1] <- bad
        expect_error(combine_consistent_hits(list(A = x, B = y)), "PValue")
    }
    for (bad in list(NA_real_, -1, 2, c(.1, .2))) {
        expect_error(combine_consistent_hits(list(A = x, B = x), fdr = bad),
            "fdr")
    }
    empty <- x[FALSE, ]
    out <- combine_consistent_hits(list(A = empty, B = empty))
    expect_equal(nrow(out), 0L)
    expect_true(all(c("Gene", "PValue", "FDR", "PValue_A") %in% names(out)))
})

consistent_fixture <- function() {
    set.seed(582)
    genes <- rep(paste0("G", seq_len(120)), each = 5)
    groups <- rep(c("T3_C", "T3_T", "T4_C", "T4_T"), each = 3)
    mu <- matrix(1000, length(genes), length(groups))
    for (j in which(groups %in% c("T3_T", "T4_T"))) {
        mu[genes %in% paste0("G", 1:5), j] <- 125
        mu[genes %in% paste0("G", 6:10), j] <- 8000
        mu[genes %in% paste0("G", 16:20), j] <-
            if (groups[j] == "T3_T") 125 else 8000
    }
    mu[genes %in% paste0("G", 11:15), groups == "T3_T"] <- 125
    counts <- matrix(rnbinom(length(mu), mu = mu, size = 100), nrow(mu))
    colnames(counts) <- paste0(groups, "_", rep(1:3, 4))
    ids <- paste0("b", seq_along(genes))
    object <- create_screenr_object(
        data.frame(Barcode = ids, counts),
        data.frame(Barcode = ids, Gene = genes), groups = groups)
    object <- normalize_data(object, "tmm")
    design <- create_design_matrix(object)
    contrasts <- list(T3 = create_contrast(design, "T3_T", "T3_C"),
        T4 = create_contrast(design, "T4_T", "T4_C"))
    list(object = object, design = design, contrasts = contrasts)
}

test_that("FRY agrees with direct complete one-sided tests and finds shared effects", {
    x <- consistent_fixture()
    dge <- edgeR::estimateDisp(create_edger_obj(x$object), x$design)
    index <- split(seq_len(nrow(dge)), dge$genes$Gene)
    for (direction in c("Down", "Up")) {
        direct <- lapply(x$contrasts, function(contrast) {
            raw <- limma::fry(dge, index, x$design, contrast)
            data.frame(Gene = rownames(raw), PValue = ifelse(
                raw$Direction == direction, raw$PValue / 2,
                1 - raw$PValue / 2))
        })
        expected <- combine_consistent_hits(direct, direction, fdr = 1)
        actual <- find_consistent_hit(x$object, x$design, x$contrasts,
            direction = direction, fdr = 1)
        expect_equal(actual[, names(expected)], expected)
        hit <- actual$Gene[actual$FDR <= .05]
        shared <- if (direction == "Down") paste0("G", 1:5) else paste0("G", 6:10)
        expect_true(all(shared %in% hit))
        expect_false(any(paste0("G", 16:20) %in% hit))
        # A gene depleted only at T3 is not supported as depleted at T4.
        if (direction == "Down") expect_false(any(paste0("G", 11:15) %in% hit))
    }
})

test_that("every wrapper supports complete tables and preserves seeded RNG", {
    x <- consistent_fixture()
    for (method in c("camera", "roast", "random", "empirical")) {
        zscore <- method %in% c("random", "empirical")
        args <- switch(method, camera = list(), roast = list(nrot = 99, seed = 3),
            random = list(reference = "random", n_permutations = 199, seed = 3),
            empirical = list(reference = "empirical"))
        before <- .Random.seed
        actual <- find_consistent_hit(x$object, x$design, x$contrasts,
            method = if (zscore) "zscore" else method,
            fdr = 1, method_args = args)
        expect_equal(.Random.seed, before)
        direct <- lapply(x$contrasts, function(contrast) {
            if (zscore) {
                do.call(find_gene_zscore_hit, c(list(
                    table_treate_vs_control = compute_barcode_statistics(
                        x$object, x$design, contrast), fdr = 1), args))
            } else {
                fun <- if (method == "camera") find_camera_hit else find_roast_hit
                do.call(fun, c(list(screenR_Object = x$object,
                    matrix_model = x$design, contrast = contrast,
                    fdr = 1, keep_all = TRUE), args))
            }
        })
        expected <- combine_consistent_hits(direct, fdr = 1)
        expect_equal(actual, expected)
        expect_equal(nrow(actual), 120L)
    }
    expect_error(find_consistent_hit(x$object, x$design, x$contrasts,
        fdr = NA_real_), "fdr")
    expect_error(find_consistent_hit(x$object, x$design, x$contrasts,
        method_args = list(fdr = .01)), "reserved")
    expect_error(find_consistent_hit(x$object, x$design, x$contrasts,
        method_args = list(nonsense = 1)), "Unsupported")
    expect_error(find_consistent_hit(x$object, x$design, x$contrasts,
        method = "legacy"), "arg")
})

test_that("keep_all preserves the default directional results", {
    x <- consistent_fixture()
    for (fun in list(find_camera_hit, find_roast_hit)) {
        args <- list(screenR_Object = x$object, matrix_model = x$design,
            contrast = x$contrasts$T3, fdr = 1)
        if (identical(fun, find_roast_hit)) args$method <- "fry"
        old <- do.call(fun, args)
        complete <- do.call(fun, c(args, list(keep_all = TRUE)))
        expect_equal(old, complete[complete$Direction == "Down", ])
        expect_equal(nrow(complete), 120L)
        expect_true(all(complete$PValue[complete$Direction == "Up"] >= .5))
    }
})
