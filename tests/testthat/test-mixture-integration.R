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
    expect_true(all(c("convergence", "fallback", "at_bound", "alpha", "pi0",
        "pi_down", "pi_up", "alpha_hat", "alpha_ci_lower", "alpha_ci_upper") %in% names(null$diagnostics$per_fold)))
    expect_named(null$diagnostics$spread, c("mu0", "s"))
    up <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, direction = "Up", fdr = 1)
    expect_identical(attr(up, "null"), null)
    expect_true(all(down$PValue + up$PValue[match(down$Gene, up$Gene)] >=
        1 - 1e-14))
    expect_equal(null$p_down_mle + null$p_up_mle, rep(1, nrow(down)))
    expect_true(all(ordered$PValue >= null$p_down_mle))
    for (k in seq_along(null$fits)) {
        keep <- null$fold == k
        expect_equal(null$p_down_mle[keep], mixture_null_cdf(null$fits[[k]],
            ordered$Statistic[keep], null$x[keep], conservative = FALSE))
        expect_equal(null$diagnostics$per_fold$alpha_hat[k],
            null$fits[[k]]$alpha_hat)
        expect_equal(unname(unlist(null$diagnostics$per_fold[k,
            c("alpha_ci_lower", "alpha_ci_upper")])),
            unname(null$fits[[k]]$alpha_ci))
    }
    storey <- find_gene_zscore_hit(table, reference = "empirical",
        empirical_fit = "mixture", seed = 17, fdr_method = "storey", fdr = 1)
    expect_equal(storey$FDR, adjust_p(storey$PValue, "storey"))
})

test_that("mixture plots show the weighted skew-normal neutral density", {
    fit <- list(
        theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, alpha = 4,
            a = stats::qlogis(0.6), b = 0, dD = 0, kD = 0, dU = 0, kU = 0),
        weights = c(null = 0.8, down = 0.1, up = 0.1)
    )
    genes <- data.frame(Statistic = seq(-3, 3, length.out = 20))
    null <- list(fits = list(fit), x = rep(0, nrow(genes)),
        fold = rep(1L, nrow(genes)),
        diagnostics = list(per_fold = data.frame(pi0 = 0.8, n_test = 20)))
    plot <- plot_mixture_null(genes, null, bins = 10)
    curve <- ggplot2::ggplot_build(plot)$data[[2]]
    expect_equal(curve$y, 0.8 * sn::dsn(curve$x, xi = 0, omega = 1, alpha = 4))
    expect_gt(curve$y[which.min(abs(curve$x - 1))],
        curve$y[which.min(abs(curve$x + 1))])
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
    expect_match(plot$labels$subtitle, "conservative alpha envelope")
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
        mixture_null_cdf = function(fit, t, x, lower.tail, conservative) {
            stats::pnorm(t, lower.tail = lower.tail)
        })
    expect_warning(result <- cross_fit_mixture_null(seq(-2, 2, length.out = 20),
        rep(0, 20), n_folds = 1), "fold 1.*fallback = TRUE")
    expect_equal(result$diagnostics$per_fold$at_bound, "beta")
    expect_true(result$diagnostics$per_fold$fallback)
})
