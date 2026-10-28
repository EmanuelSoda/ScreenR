# Numerical tests of the mixture null (R/mixture_null.R)

# Parameters of the simulated truth, in the order of mixture_null_bounds():
# a null N(0.2 + 0.1 x, exp(0.1 + 0.15 x)^2), pi_0 = 0.85 and two wide
# alternatives at least 3 null sd away (offset 3 + exp(d))
true_theta <- function() {
    c(mu0 = 0.2, beta = 0.1, s = 0.1, gamma = 0.15, a = qlogis(0.7), b = 0,
        dD = log(2), kD = 0, dU = log(2), kU = 0)
}

simulate_mixture <- function(n = 2000, theta = true_theta(), seed = 1) {
    set.seed(seed)
    nbarcodes <- sample(3:10, n, replace = TRUE)
    x <- log(nbarcodes) - mean(log(nbarcodes))
    p <- mixture_null_unpack(theta)
    component <- sample(1:3, n, replace = TRUE, prob = p$weights)
    m0 <- p$mu0 + p$beta * x
    s0 <- exp(p$s + p$gamma * x)
    centre <- m0 + s0 * c(0, -(3 + exp(p$dD)), 3 + exp(p$dU))[component]
    spread <- s0 * c(1, 1 + exp(p$kD), 1 + exp(p$kU))[component]
    list(t = rnorm(n, centre, spread), x = x, component = component)
}

random_theta <- function(seed) {
    set.seed(seed)
    bounds <- mixture_null_bounds()
    lower <- pmax(bounds$lower, c(-2, -2, -1, -1, -4, -4, -3, -3, -3, -3))
    upper <- pmin(bounds$upper, c(2, 2, 1, 1, 4, 4, 2, 2, 2, 2))
    stats::setNames(runif(10, lower, upper), bounds$names)
}

test_that("the analytic gradient matches the finite differences", {
    for (seed in 1:8) {
        theta <- random_theta(seed)
        data <- simulate_mixture(n = 300, theta = true_theta(), seed = seed)
        analytic <- mixture_null_gradient(theta, data$t, data$x)
        numeric <- vapply(seq_along(theta), function(i) {
            h <- 1e-6
            up <- down <- theta
            up[i] <- up[i] + h
            down[i] <- down[i] - h
            (mixture_null_loglik(up, data$t, data$x) -
                mixture_null_loglik(down, data$t, data$x)) / (2 * h)
        }, numeric(1))
        expect_named(analytic, names(theta))
        expect_equal(unname(analytic), numeric,
            tolerance = 1e-5 * max(1, max(abs(numeric))))
    }
})

test_that("the gradient is accurate at extreme weights and far statistics", {
    data <- simulate_mixture(n = 200, seed = 3)
    t <- c(data$t, -60, 80)
    x <- c(data$x, 1, -1)
    theta <- c(mu0 = 0, beta = 0.5, s = 0, gamma = 0.3, a = 12, b = -10,
        dD = 1, kD = 1, dU = -2, kU = -1)
    analytic <- mixture_null_gradient(theta, t, x)
    expect_true(all(is.finite(analytic)))
    expect_true(is.finite(mixture_null_loglik(theta, t, x)))
    numeric <- vapply(seq_along(theta), function(i) {
        h <- 1e-6
        up <- down <- theta
        up[i] <- up[i] + h
        down[i] <- down[i] - h
        (mixture_null_loglik(up, t, x) - mixture_null_loglik(down, t, x)) /
            (2 * h)
    }, numeric(1))
    expect_equal(unname(analytic), numeric,
        tolerance = 1e-4 * max(1, max(abs(numeric))))
})

test_that("the weights are a probability vector with pi_0 >= 0.5", {
    for (a in c(-15, -3, 0, 3, 15)) {
        for (b in c(-15, 0, 15)) {
            w <- mixture_null_weights(a, b)
            expect_equal(sum(w), 1)
            expect_gte(w[["null"]], 0.5)
            expect_true(all(w >= 0))
        }
    }
})

test_that("the log-likelihood equals the log of the mixture density", {
    data <- simulate_mixture(n = 50, seed = 4)
    theta <- random_theta(11)
    p <- mixture_null_unpack(theta)
    m0 <- p$mu0 + p$beta * data$x
    s0 <- exp(p$s + p$gamma * data$x)
    density <- p$weights[["null"]] * dnorm(data$t, m0, s0) +
        p$weights[["down"]] * dnorm(data$t, m0 - s0 * (3 + exp(p$dD)),
            s0 * (1 + exp(p$kD))) +
        p$weights[["up"]] * dnorm(data$t, m0 + s0 * (3 + exp(p$dU)),
            s0 * (1 + exp(p$kU)))
    expect_equal(mixture_null_loglik(theta, data$t, data$x),
        sum(log(density)))
})

test_that("the parameters are recovered from a simulated screen", {
    data <- simulate_mixture(n = 2000, seed = 1)
    fit <- mixture_null_fit(data$t, data$x)
    expect_s3_class(fit, "screenr_mixture_fit")
    expect_equal(fit$convergence, 0L)
    expect_false(fit$fallback)
    expect_gte(fit$starts_converged, 1L)
    expect_equal(fit$n, 2000L)
    truth <- true_theta()
    expect_lt(abs(fit$theta[["mu0"]] - truth[["mu0"]]), 0.15)
    expect_lt(abs(fit$theta[["beta"]] - truth[["beta"]]), 0.15)
    expect_lt(abs(fit$theta[["s"]] - truth[["s"]]), 0.12)
    expect_lt(abs(fit$theta[["gamma"]] - truth[["gamma"]]), 0.2)
    expect_lt(abs(fit$weights[["null"]] - 0.85), 0.08)
    expect_equal(sum(fit$weights), 1)
    # The fit is at least as good as the truth
    expect_gte(fit$loglik,
        mixture_null_loglik(truth, data$t, data$x) - 1e-6)
    # The posterior assigns the far genes to the right alternative
    posterior <- mixture_null_posterior(fit, data$t, data$x)
    far_down <- data$t < fit$theta[["mu0"]] - 6
    far_up <- data$t > fit$theta[["mu0"]] + 6
    expect_gt(mean(posterior[far_down, "down"]), 0.9)
    expect_gt(mean(posterior[far_up, "up"]), 0.9)
})

test_that("the parameters are recovered across several simulations", {
    errors <- vapply(11:16, function(seed) {
        data <- simulate_mixture(n = 2000, seed = seed)
        fit <- mixture_null_fit(data$t, data$x)
        c(mu0 = fit$theta[["mu0"]] - 0.2, s = fit$theta[["s"]] - 0.1,
            pi_0 = fit$weights[["null"]] - 0.85)
    }, numeric(3))
    expect_lt(max(abs(rowMeans(errors))), 0.08)
    expect_lt(max(abs(errors)), 0.2)
})

test_that("a pure null sample is not split between the components", {
    # Regression test of the amendment A1: with alternatives closer than three
    # null sd the pure null was fitted with pi_0 ~ 0.5 and a null sd of 0.7
    for (seed in 1:12) {
        set.seed(seed)
        n <- 2000
        nbarcodes <- sample(3:10, n, replace = TRUE)
        x <- log(nbarcodes) - mean(log(nbarcodes))
        t <- rnorm(n)
        fit <- mixture_null_fit(t, x)
        expect_equal(fit$convergence, 0L, info = paste("seed", seed))
        expect_gt(fit$weights[["null"]], 0.9)
        null_sd <- exp(fit$theta[["s"]])
        expect_gte(null_sd, 0.93)
        expect_lte(null_sd, 1.07)
        p <- mixture_null_cdf(fit, t, x)
        expect_gt(suppressWarnings(stats::ks.test(p, "punif")$p.value), 0.001)
    }
})

test_that("the null is recovered when 10% of the genes are strong hits", {
    for (seed in 1:5) {
        set.seed(seed)
        n <- 2000
        nbarcodes <- sample(3:10, n, replace = TRUE)
        x <- log(nbarcodes) - mean(log(nbarcodes))
        hit <- seq_len(n) <= 0.1 * n
        t <- rnorm(n) + ifelse(hit, -5, 0)
        fit <- mixture_null_fit(t, x)
        expect_equal(fit$convergence, 0L, info = paste("seed", seed))
        expect_lt(abs(fit$weights[["null"]] - 0.9), 0.04)
        expect_lt(abs(fit$theta[["mu0"]]), 0.15)
        null_sd <- exp(fit$theta[["s"]])
        expect_gte(null_sd, 0.93)
        expect_lte(null_sd, 1.07)
        # The typical hit is far in the lower tail of the neutral distribution and the
        # genes that are not hits have uniform p-values
        p <- mixture_null_cdf(fit, t, x)
        expect_lt(median(p[hit]), 1e-5)
        expect_gt(suppressWarnings(stats::ks.test(p[!hit], "punif")$p.value),
            0.001)
    }
})

test_that("the fit respects the bounds and the constraints", {
    bounds <- mixture_null_bounds()
    for (seed in 1:3) {
        data <- simulate_mixture(n = 500, seed = seed)
        fit <- mixture_null_fit(data$t, data$x)
        expect_named(fit$theta, bounds$names)
        expect_true(all(fit$theta >= bounds$lower))
        expect_true(all(fit$theta <= bounds$upper))
        expect_gte(fit$weights[["null"]], 0.5)
        expect_equal(sum(fit$weights), 1)
        expect_true(all(fit$at_bound %in% bounds$names))
    }
})

test_that("a bound hit at the optimum is reported", {
    # Heavy tails need wide alternatives: the weights go to the bound
    set.seed(2)
    t <- c(rnorm(1000), rnorm(1000, sd = 4))
    fit <- mixture_null_fit(t, rep(0, 2000))
    expect_equal(fit$convergence, 0L)
    expect_gt(length(fit$at_bound), 0L)
    expect_true(all(fit$at_bound %in% mixture_null_bounds()$names))
    # x is constant: beta and gamma are not identified and stay at 0
    expect_equal(fit$theta[["beta"]], 0, tolerance = 1e-6)
})

test_that("the fit is deterministic", {
    data <- simulate_mixture(n = 400, seed = 5)
    first <- mixture_null_fit(data$t, data$x)
    second <- mixture_null_fit(data$t, data$x)
    expect_identical(first, second)
})

test_that("more starts never give a worse likelihood", {
    data <- simulate_mixture(n = 600, seed = 6)
    one <- mixture_null_fit(data$t, data$x, n_starts = 1)
    many <- mixture_null_fit(data$t, data$x, n_starts = 8)
    expect_gte(many$loglik, one$loglik - 1e-8)
    expect_gte(many$starts_converged, one$starts_converged)
})

test_that("the neutral CDF is accurate in the far tails", {
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, a = 1, b = 0,
        dD = 0, kD = 0, dU = 0, kU = 0))
    # Asymptotic expansion of log Phi(-z)
    log_tail <- function(z) {
        -z^2 / 2 - log(z) - 0.5 * log(2 * pi) +
            log(1 - 1 / z^2 + 3 / z^4 - 15 / z^6)
    }
    expect_equal(mixture_null_cdf(fit, -40, 0, log.p = TRUE), log_tail(40),
        tolerance = 1e-8)
    expect_equal(mixture_null_cdf(fit, 40, 0, lower.tail = FALSE,
        log.p = TRUE), log_tail(40), tolerance = 1e-8)
    # No cancellation in the upper tail: 1 - pnorm(10) is 7.6e-24 in theory
    expect_gt(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE), 0)
    expect_equal(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE),
        mixture_null_cdf(fit, -10, 0))
    expect_equal(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE),
        7.619853e-24, tolerance = 1e-6)
    # The two tails add up to 1 near the centre
    expect_equal(mixture_null_cdf(fit, 0.3, 0) +
        mixture_null_cdf(fit, 0.3, 0, lower.tail = FALSE), 1)
})

test_that("the neutral CDF uses the centre and the spread given x", {
    fit <- list(theta = c(mu0 = 1, beta = 0.5, s = log(2), gamma = 0.2,
        a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    x <- c(-1, 0, 2)
    t <- c(-3, 1.5, 4)
    expected <- pnorm((t - (1 + 0.5 * x)) / exp(log(2) + 0.2 * x))
    expect_equal(mixture_null_cdf(fit, t, x), expected)
    expect_equal(mixture_null_cdf(fit, t, x, log.p = TRUE), log(expected))
    # A single value of x is used for all the genes
    expect_equal(mixture_null_cdf(fit, t, 0),
        pnorm((t - 1) / 2))
    expect_error(mixture_null_cdf(fit, t, c(0, 1)), "same length")
})

test_that("the posterior probabilities are a probability for each gene", {
    data <- simulate_mixture(n = 400, seed = 7)
    fit <- mixture_null_fit(data$t, data$x)
    posterior <- mixture_null_posterior(fit, c(data$t, -500, 500, -8, 8),
        c(data$x, 0, 0, 0, 0))
    expect_equal(dim(posterior), c(404L, 3L))
    expect_equal(colnames(posterior), c("null", "down", "up"))
    expect_true(all(is.finite(posterior)))
    expect_equal(unname(rowSums(posterior)), rep(1, 404))
    expect_true(all(posterior >= 0 & posterior <= 1))
    # Far from the null the alternatives take over (which of the two is
    # not meaningful so far out: the wider component dominates both tails)
    expect_lt(max(posterior[401:404, "null"]), 0.01)
    expect_gt(posterior[403, "down"], 0.5)
    expect_gt(posterior[404, "up"], 0.5)
})

test_that("pi_0 close to 1 is handled", {
    # Statistics of a single normal distribution (no hits): the fit must be
    # well defined whatever the split of the bulk between the components
    set.seed(8)
    n <- 1500
    nbarcodes <- sample(3:10, n, replace = TRUE)
    x <- log(nbarcodes) - mean(log(nbarcodes))
    t <- rnorm(n, 0.1, 1.2)
    fit <- mixture_null_fit(t, x)
    expect_equal(fit$convergence, 0L)
    expect_false(fit$fallback)
    expect_gte(fit$weights[["null"]], 0.5)
    expect_equal(sum(fit$weights), 1)
    expect_true(all(is.finite(fit$theta)))
    p <- mixture_null_cdf(fit, t, x)
    expect_true(all(p >= 0 & p <= 1))
    # A larger sample of a single normal distribution
    set.seed(9)
    shrunk <- mixture_null_fit(rnorm(3000), rep(0, 3000))
    expect_equal(shrunk$convergence, 0L)
    expect_true(is.finite(shrunk$loglik))
})

test_that("about 100 genes are fitted", {
    data <- simulate_mixture(n = 100, seed = 9)
    fit <- mixture_null_fit(data$t, data$x)
    expect_equal(fit$n, 100L)
    expect_true(all(is.finite(fit$theta)))
    expect_true(is.finite(fit$loglik))
    expect_equal(sum(fit$weights), 1)
    expect_true(fit$convergence == 0L || fit$fallback)
})

test_that("a constant covariate gives a stable fit", {
    data <- simulate_mixture(n = 800, seed = 10)
    fit <- mixture_null_fit(data$t, rep(0, 800))
    expect_equal(fit$convergence, 0L)
    expect_true(all(is.finite(fit$theta)))
})

test_that("the fit falls back to central matching with a warning", {
    data <- simulate_mixture(n = 300, seed = 12)
    expect_warning(
        fit <- mixture_null_fit(data$t, data$x, control = list(maxit = 1)),
        "central matching")
    expect_true(fit$fallback)
    expect_false(fit$convergence == 0L)
    expect_match(fit$message, "fallback")
    central <- fit_null_central(data$t)
    expect_equal(fit$theta[["mu0"]], central[["mean"]])
    expect_equal(exp(fit$theta[["s"]]), central[["sd"]])
    expect_equal(fit$theta[["beta"]], 0)
    expect_equal(fit$theta[["gamma"]], 0)
    expect_equal(fit$at_bound, character(0))
    # The fallback can still be used for the p-values
    p <- mixture_null_cdf(fit, data$t, data$x)
    expect_true(all(p >= 0 & p <= 1))
    # Too few genes also fall back
    expect_warning(mixture_null_fit(rnorm(5), rep(0, 5)), "central matching")
})

test_that("invalid input is refused", {
    expect_error(mixture_null_fit(c(1, NA, 3), c(0, 0, 0)), "finite")
    expect_error(mixture_null_fit(c(1, 2, 3), c(0, 0)), "same length")
    expect_error(mixture_null_fit("a", 0), "numeric")
    expect_error(mixture_null_fit(rnorm(50), rep(0, 50), n_starts = 0),
        "n_starts")
    expect_error(mixture_null_loglik(1:5, rnorm(10), rep(0, 10)), "length 10")
    expect_error(mixture_null_gradient(rep(NA_real_, 10), rnorm(10),
        rep(0, 10)), "length 10")
})
