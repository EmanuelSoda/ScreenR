test_that("legacy empirical fits equal the devel 1dfd3b0 fixture exactly", {
    baseline <- readRDS(test_path("fixtures",
        "empirical-null-devel-1dfd3b0.rds"))
    expect_identical(baseline$commit,
        "1dfd3b002fe6386a6d01c6dc1e6bf4dc969d5699")
    for (i in seq_len(nrow(baseline$arguments))) {
        args <- c(list(table_treate_vs_control = baseline$table,
            reference = "empirical", fdr = 1),
            as.list(baseline$arguments[i, ]))
        expect_identical(do.call(find_gene_zscore_hit, args),
            baseline$results[[i]])
        if (args$empirical_fit == "central") {
            args$empirical_fit <- NULL
            expect_identical(do.call(find_gene_zscore_hit, args),
                baseline$results[[i]])
        }
    }
    expect_identical(tail(names(formals(find_gene_zscore_hit)), 1), "n_folds")
})
