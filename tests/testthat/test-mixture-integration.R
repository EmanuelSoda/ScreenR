mixture_integration_screen <- function() {
    set.seed(734)
    n <- 300
    sizes <- sample(3:10, n, replace = TRUE)
    effect <- stats::rnorm(n, sd = 0.3)
    effect[1:15] <- -3
    effect[16:30] <- 3
    score <- rep(effect, sizes) + stats::rnorm(sum(sizes))
    data.frame(Gene = rep(paste0("G", seq_len(n)), sizes),
        Log2FC = score / 2, Zscore = score)
}

test_that("mixture cross-fitting holds out genes and pools p-values once", {
    table <- mixture_integration_screen()
    down <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, fdr = 1)
    null <- attr(down, "null")
    expect_length(null$fits, 5)
    expect_equal(tabulate(null$fold), rep(60L, 5))
    expect_equal(null$x, log(down$NBarcodes[match(null$genes, down$Gene)]) -
        mean(log(down$NBarcodes)))
    ordered <- down[match(null$genes, down$Gene), ]
    for (k in seq_along(null$fits)) {
        keep <- null$fold == k
        expect_equal(null$fits[[k]]$n, 240)
        direct <- mixture_null_fit(ordered$Statistic[!keep], null$x[!keep])
        expect_equal(null$fits[[k]], direct)
        expect_equal(ordered$PValue[keep], mixture_null_cdf(direct,
            ordered$Statistic[keep], null$x[keep]))
    }
    expect_equal(down$FDR, stats::p.adjust(down$PValue, "BH"))
    expect_equal(nrow(null$diagnostics$per_fold), 5)
    expect_true(all(c("convergence", "fallback", "at_bound", "pi0",
        "pi_down", "pi_up") %in% names(null$diagnostics$per_fold)))
    expect_named(null$diagnostics$spread, c("mu0", "s"))
    up <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, direction = "Up", fdr = 1)
    expect_identical(attr(up, "null"), null)
    expect_equal(down$PValue + up$PValue[match(down$Gene, up$Gene)],
        rep(1, nrow(down)))
    storey <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, fdr_method = "storey", fdr = 1)
    expect_equal(storey$FDR, adjust_p(storey$PValue, "storey"))
})

test_that("mixture fold assignment is reproducible and preserves the RNG", {
    table <- mixture_integration_screen()
    set.seed(92)
    before <- .Random.seed
    run <- function(seed) find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = seed, fdr = 1, n_folds = 2)
    first <- run(11)
    expect_identical(.Random.seed, before)
    expect_identical(run(11), first)
    expect_false(identical(attr(run(12), "null")$fold,
        attr(first, "null")$fold))
    session <- run(NULL)
    expect_identical(.Random.seed, before)
    expect_identical(run(NULL), session)
    rm(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", before, envir = globalenv()), add = TRUE)
    run(11)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    run(NULL)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("one-fold diagnostics, plot and fold validation work", {
    table <- mixture_integration_screen()
    result <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, fdr = 1, n_folds = 1)
    null <- attr(result, "null")
    expect_equal(null$fits[[1]]$n, 300)
    expect_equal(null$fold, rep(1L, 300))
    expect_equal(null$diagnostics$spread, c(mu0 = 0, s = 0))
    plot <- plot_empirical_null(table, empirical_fit = "mixture",
        seed = 17, n_folds = 1)
    expect_true(ggplot2::is_ggplot(plot))
    expect_match(plot$labels$subtitle, "Mixture empirical null")
    expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2]]), 512)
    for (folds in list(0, -1, 1.5, NA_real_, Inf, 301, c(2, 3))) {
        expect_error(find_gene_zscore_hit(table, reference = "empirical",
            empirical_fit = "mixture", n_folds = folds), "n_folds")
    }
})

test_that("mixture diagnostics warn when a fold falls back", {
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0),
        weights = c(null = 1, down = 0, up = 0), convergence = 1L,
        fallback = TRUE, n = 20L, starts_converged = 0L,
        at_bound = "beta", message = "test fallback")
    local_mocked_bindings(mixture_null_fit = function(...) fit,
        mixture_null_cdf = function(fit, t, x, lower.tail) {
            stats::pnorm(t, lower.tail = lower.tail)
        })
    expect_warning(result <- cross_fit_mixture_null(seq(-2, 2, length.out = 20),
        rep(0, 20), n_folds = 1), "fold 1.*fallback = TRUE")
    expect_equal(result$diagnostics$per_fold$at_bound, "beta")
    expect_true(result$diagnostics$per_fold$fallback)
})
