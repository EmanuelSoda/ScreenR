# Numerical tests of the mixture null (R/mixture_null.R)

# Parameters of the simulated truth, in the order of mixture_null_bounds():
# a skew-normal null SN(0.2 + 0.1 x, exp(0.1 + 0.15 x), alpha = 2), pi_0 = 0.85
# and two wide normal alternatives at least 3 null sd (from the mean) away
true_theta <- function() {
    c(mu0 = 0.2, beta = 0.1, s = 0.1, gamma = 0.15, alpha = 2,
        a = qlogis(0.7), b = 0, dD = log(2), kD = 0, dU = log(2), kU = 0)
}

# Random skew-normal values: xi + omega (delta |u0| + sqrt(1 - delta^2) u1)
rskewnormal <- function(n, xi, omega, alpha) {
    delta <- alpha / sqrt(1 + alpha^2)
    xi + omega * (delta * abs(rnorm(n)) + sqrt(1 - delta^2) * rnorm(n))
}

# Skew-normal density of the standard distribution
dskewnormal <- function(z, alpha) 2 * dnorm(z) * pnorm(alpha * z)

simulate_mixture <- function(n = 2000, theta = true_theta(), seed = 1) {
    set.seed(seed)
    nbarcodes <- sample(3:10, n, replace = TRUE)
    x <- log(nbarcodes) - mean(log(nbarcodes))
    p <- mixture_null_unpack(theta)
    k <- mixture_null_skew_constants(p$alpha)
    component <- sample(1:3, n, replace = TRUE, prob = p$weights)
    xi <- p$mu0 + p$beta * x
    omega <- exp(p$s + p$gamma * x)
    m0 <- xi + omega * k$k1
    s0 <- omega * k$k2
    t <- numeric(n)
    null <- component == 1
    t[null] <- rskewnormal(sum(null), xi[null], omega[null], p$alpha)
    centre <- m0 + s0 * c(0, -(3 + exp(p$dD)), 3 + exp(p$dU))[component]
    spread <- s0 * c(1, 1 + exp(p$kD), 1 + exp(p$kU))[component]
    t[!null] <- rnorm(sum(!null), centre[!null], spread[!null])
    list(t = t, x = x, component = component)
}

random_theta <- function(seed) {
    set.seed(seed)
    bounds <- mixture_null_bounds()
    lower <- pmax(bounds$lower, c(-2, -2, -1, -1, -6, -4, -4, -3, -3, -3, -3))
    upper <- pmin(bounds$upper, c(2, 2, 1, 1, 6, 4, 4, 2, 2, 2, 2))
    stats::setNames(runif(11, lower, upper), bounds$names)
}

finite_differences <- function(theta, t, x, h = 1e-6) {
    vapply(seq_along(theta), function(i) {
        up <- down <- theta
        up[i] <- up[i] + h
        down[i] <- down[i] - h
        (mixture_null_loglik(up, t, x) - mixture_null_loglik(down, t, x)) /
            (2 * h)
    }, numeric(1))
}

# Screen with a skew-normal null of mean 0 and sd 1 (shape alpha), and
# optionally a proportion of normal hits
simulate_screen <- function(seed, alpha, n = 2000, hits = 0) {
    set.seed(seed)
    nbarcodes <- sample(3:10, n, replace = TRUE)
    x <- log(nbarcodes) - mean(log(nbarcodes))
    k <- mixture_null_skew_constants(alpha)
    omega <- 1 / k$k2
    hit <- seq_len(n) <= hits * n
    t <- rskewnormal(n, -omega * k$k1, omega, alpha)
    side <- ifelse(seq_len(n) %% 2 == 0, -1, 1)
    t[hit] <- rnorm(sum(hit), side[hit] * 6, 1.5)
    list(t = t, x = x, hit = hit)
}

# Log of the tail of the skew-normal distribution by numerical integration of
# its density, in pieces around the bulk and relative to the maximum of the
# density on the range (no underflow, no 1 - F)
log_tail_integral <- function(z, alpha, lower) {
    log_density <- function(y) {
        log(2) + dnorm(y, log = TRUE) + pnorm(alpha * y, log.p = TRUE)
    }
    range <- if (lower) c(z - 60, z) else c(z, z + 60)
    inside <- c(-12, -5, -2, 0, 2, 5, 12)
    # Geometric breaks next to z, where the integrand can be very peaked
    near <- z + (if (lower) -1 else 1) * 10^(-5:1)
    candidates <- c(inside, near)
    breaks <- sort(unique(c(range, candidates[candidates > range[1] &
        candidates < range[2]])))
    ref <- max(log_density(seq(range[1], range[2], length.out = 4001)))
    value <- sum(vapply(seq_len(length(breaks) - 1L), function(i) {
        integrate(function(y) exp(log_density(y) - ref), breaks[i],
            breaks[i + 1L], rel.tol = 1e-12, abs.tol = 1e-15)$value
    }, numeric(1)))
    ref + log(value)
}

test_that("the analytic gradient matches the finite differences", {
    for (seed in 1:10) {
        theta <- random_theta(seed)
        data <- simulate_mixture(n = 300, theta = true_theta(), seed = seed)
        analytic <- mixture_null_gradient(theta, data$t, data$x)
        numeric <- finite_differences(theta, data$t, data$x)
        expect_named(analytic, names(theta))
        expect_equal(unname(analytic), numeric,
            tolerance = 1e-5 * max(1, max(abs(numeric))),
            info = paste("seed", seed))
    }
})

test_that("the gradient of alpha is checked at the bounds and near 0", {
    data <- simulate_mixture(n = 300, seed = 21)
    for (alpha in c(-10, -3, -1e-3, 0, 1e-3, 3, 10)) {
        theta <- true_theta()
        theta[["alpha"]] <- alpha
        analytic <- mixture_null_gradient(theta, data$t, data$x)
        numeric <- finite_differences(theta, data$t, data$x)
        expect_equal(unname(analytic), numeric,
            tolerance = 1e-5 * max(1, max(abs(numeric))),
            info = paste("alpha", alpha))
    }
})

test_that("the gradient is accurate at extreme weights and far statistics", {
    data <- simulate_mixture(n = 200, seed = 3)
    t <- c(data$t, -60, 80)
    x <- c(data$x, 1, -1)
    theta <- c(mu0 = 0, beta = 0.5, s = 0, gamma = 0.3, alpha = -8, a = 12,
        b = -10, dD = 1, kD = 1, dU = -2, kU = -1)
    analytic <- mixture_null_gradient(theta, t, x)
    expect_true(all(is.finite(analytic)))
    expect_true(is.finite(mixture_null_loglik(theta, t, x)))
    numeric <- finite_differences(theta, t, x)
    expect_equal(unname(analytic), numeric,
        tolerance = 1e-4 * max(1, max(abs(numeric))))
    # alpha z very negative: log Phi and the ratio phi / Phi stay finite
    theta[["alpha"]] <- 10
    t <- c(data$t, -200)
    x <- c(data$x, 0)
    expect_true(all(is.finite(mixture_null_gradient(theta, t, x))))
    expect_true(is.finite(mixture_null_loglik(theta, t, x)))
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
    k <- mixture_null_skew_constants(p$alpha)
    xi <- p$mu0 + p$beta * data$x
    omega <- exp(p$s + p$gamma * data$x)
    m0 <- xi + omega * k$k1
    s0 <- omega * k$k2
    f_null <- dskewnormal((data$t - xi) / omega, p$alpha) / omega
    f_down <- dnorm(data$t, m0 - s0 * (3 + exp(p$dD)), s0 * (1 + exp(p$kD)))
    f_up <- dnorm(data$t, m0 + s0 * (3 + exp(p$dU)), s0 * (1 + exp(p$kU)))
    density <- p$weights[["null"]] * f_null + p$weights[["down"]] * f_down +
        p$weights[["up"]] * f_up
    expect_equal(mixture_null_loglik(theta, data$t, data$x), sum(log(density)))
})

test_that("the mean and the sd of the skew-normal null are m0 and sd0", {
    for (alpha in c(-10, -3, -0.5, 0, 0.5, 3, 10)) {
        k <- mixture_null_skew_constants(alpha)
        f <- function(z) dskewnormal(z, alpha)
        mean <- integrate(function(z) z * f(z), -40, 40, rel.tol = 1e-10)$value
        var <- integrate(function(z) (z - mean)^2 * f(z), -40, 40,
            rel.tol = 1e-10)$value
        expect_equal(mean, k$k1, tolerance = 1e-8)
        expect_equal(sqrt(var), k$k2, tolerance = 1e-8)
        # The derivatives with respect to alpha
        h <- 1e-6
        up <- mixture_null_skew_constants(alpha + h)
        down <- mixture_null_skew_constants(alpha - h)
        expect_equal(k$dk1, (up$k1 - down$k1) / (2 * h), tolerance = 1e-6)
        expect_equal(k$dk2, (up$k2 - down$k2) / (2 * h), tolerance = 1e-6)
    }
})

test_that("Owen's T function matches the numerical integral", {
    integrand <- function(h, a) {
        integrate(function(y) exp(-h^2 * (1 + y^2) / 2) / (1 + y^2), 0, a,
            rel.tol = 1e-12, abs.tol = 0)$value / (2 * pi)
    }
    for (a in c(0.05, 0.5, 1, 2, 5, 10)) {
        h <- c(0, 0.01, 0.3, 1, 2.5, 5, 10, 20)
        expected <- vapply(h, integrand, numeric(1), a = a)
        expect_equal(owens_t(h, a), expected, tolerance = 1e-9,
            info = paste("a", a))
        # Even in h and odd in a
        expect_equal(owens_t(-h, a), owens_t(h, a))
        expect_equal(owens_t(h, -a), -owens_t(h, a))
    }
    expect_equal(owens_t(c(-1, 0, 2), 0), c(0, 0, 0))
    # Closed forms: T(0, a) = atan(a) / (2 pi), T(h, 1) = Phi(h) Phi(-h) / 2
    expect_equal(owens_t(0, 7), atan(7) / (2 * pi), tolerance = 1e-12)
    h <- c(0.2, 1, 3, 6)
    expect_equal(owens_t(h, 1), pnorm(h) * pnorm(-h) / 2, tolerance = 1e-12)
    # T(h, Inf) = Phi(-h) / 2 is approached for a large
    expect_equal(owens_t(2, 10), pnorm(-2) / 2, tolerance = 5e-3)
})

test_that("the skew-normal CDF matches the numerical integral of its density", {
    worst <- 0
    for (alpha in c(-10, -5, -3, -1, -0.3, 0.1, 0.5, 1, 2, 3, 5, 10)) {
        for (z in c(-12, -8, -5, -3, -1, -0.1, 0, 0.1, 1, 3, 5, 8)) {
            for (lower in c(TRUE, FALSE)) {
                expected <- log_tail_integral(z, alpha, lower)
                got <- skew_normal_log_cdf(z, alpha, lower.tail = lower)
                worst <- max(worst, abs(got - expected) /
                    max(1, abs(expected)))
            }
        }
    }
    expect_lt(worst, 1e-9)
    # The two tails add up to 1 where neither is small
    z <- seq(-1.5, 1.5, 0.5)
    for (alpha in c(-8, -2, 0, 2, 8)) {
        expect_equal(exp(skew_normal_log_cdf(z, alpha)) +
            exp(skew_normal_log_cdf(z, alpha, lower.tail = FALSE)), rep(1, 7))
    }
    # alpha = 0 is the normal distribution
    expect_equal(skew_normal_log_cdf(c(-30, -2, 4), 0),
        pnorm(c(-30, -2, 4), log.p = TRUE))
    expect_equal(skew_normal_log_cdf(c(-2, 4), 1e-9),
        pnorm(c(-2, 4), log.p = TRUE), tolerance = 1e-8)
})

test_that("the skew-normal CDF is accurate and finite in the far tails", {
    for (alpha in c(-10, -3, 3, 10)) {
        z <- c(-60, -40, -20, 20, 40, 60)
        lower <- skew_normal_log_cdf(z, alpha)
        upper <- skew_normal_log_cdf(z, alpha, lower.tail = FALSE)
        expect_true(all(is.finite(lower)))
        expect_true(all(is.finite(upper)))
        expect_true(all(lower <= 0) && all(upper <= 0))
        # Monotone: the lower tail increases, the upper tail decreases
        expect_true(all(diff(lower[1:3]) > 0))
        expect_true(all(diff(upper[4:6]) < 0))
    }
    # Against the integral of the density, 40 sd away from the centre
    for (alpha in c(-10, -3, 3, 10)) {
        for (z in c(-40, 40)) {
            for (lower in c(TRUE, FALSE)) {
                expect_equal(skew_normal_log_cdf(z, alpha, lower),
                    log_tail_integral(z, alpha, lower), tolerance = 1e-9)
            }
        }
    }
    # No 1 - F cancellation: the upper tail at z = 12 is 3.55e-33 (alpha = 10)
    expect_gt(exp(skew_normal_log_cdf(12, 10, lower.tail = FALSE)), 0)
    expect_equal(exp(skew_normal_log_cdf(12, 10, lower.tail = FALSE)),
        exp(skew_normal_log_cdf(-12, -10)))
    expect_equal(exp(skew_normal_log_cdf(12, 10, lower.tail = FALSE)),
        3.552964e-33, tolerance = 1e-6)
})

test_that("the parameters are recovered from a simulated screen", {
    data <- simulate_mixture(n = 2000, seed = 1)
    fit <- mixture_null_fit(data$t, data$x)
    expect_s3_class(fit, "screenr_mixture_fit")
    expect_equal(fit$convergence, 0L)
    expect_false(fit$fallback)
    expect_gte(fit$starts_converged, 1L)
    expect_equal(fit$n, 2000L)
    expect_named(fit$theta, mixture_null_bounds()$names)
    truth <- true_theta()
    expect_lt(abs(fit$theta[["beta"]] - truth[["beta"]]), 0.15)
    expect_lt(abs(fit$theta[["gamma"]] - truth[["gamma"]]), 0.2)
    expect_lt(abs(fit$weights[["null"]] - 0.85), 0.08)
    expect_equal(sum(fit$weights), 1)
    # The mean, the sd and the skewness sign of the null
    k_fit <- mixture_null_skew_constants(fit$theta[["alpha"]])
    k_true <- mixture_null_skew_constants(truth[["alpha"]])
    mean_fit <- fit$theta[["mu0"]] + exp(fit$theta[["s"]]) * k_fit$k1
    mean_true <- truth[["mu0"]] + exp(truth[["s"]]) * k_true$k1
    expect_lt(abs(mean_fit - mean_true), 0.12)
    expect_lt(abs(exp(fit$theta[["s"]]) * k_fit$k2 -
        exp(truth[["s"]]) * k_true$k2), 0.12)
    expect_gt(fit$theta[["alpha"]], 0.5)
    # The fit is at least as good as the truth
    expect_gte(fit$loglik, mixture_null_loglik(truth, data$t, data$x) - 1e-6)
    # The posterior assigns the far genes to the right alternative
    posterior <- mixture_null_posterior(fit, data$t, data$x)
    far_down <- data$t < mean_fit - 6
    far_up <- data$t > mean_fit + 6
    expect_gt(mean(posterior[far_down, "down"]), 0.9)
    expect_gt(mean(posterior[far_up, "up"]), 0.9)
})

test_that("a skew-normal null with 10% normal hits is recovered", {
    for (alpha in c(-3, 0, 3)) {
        for (seed in 1:3) {
            data <- simulate_screen(seed, alpha, hits = 0.1)
            fit <- mixture_null_fit(data$t, data$x)
            info <- paste("alpha", alpha, "seed", seed)
            expect_equal(fit$convergence, 0L, info = info)
            expect_lt(abs(fit$weights[["null"]] - 0.9), 0.04)
            # The null has mean 0 and sd 1 whatever its shape
            k <- mixture_null_skew_constants(fit$theta[["alpha"]])
            null_mean <- fit$theta[["mu0"]] + exp(fit$theta[["s"]]) * k$k1
            null_sd <- exp(fit$theta[["s"]]) * k$k2
            expect_lt(abs(null_mean), 0.12)
            expect_lt(abs(null_sd - 1), 0.1)
            # The sign and the order of magnitude of the shape
            expect_lt(abs(fit$theta[["alpha"]] - alpha), 2.5)
            if (alpha != 0) {
                expect_equal(sign(fit$theta[["alpha"]]), sign(alpha))
            }
            # The genes without effect have uniform p-values in both tails and
            # the hits are far in the tail of the neutral distribution
            p_down <- mixture_null_cdf(fit, data$t, data$x)
            p_up <- mixture_null_cdf(fit, data$t, data$x, lower.tail = FALSE)
            expect_gt(suppressWarnings(
                stats::ks.test(p_down[!data$hit], "punif")$p.value), 0.001)
            expect_gt(suppressWarnings(
                stats::ks.test(p_up[!data$hit], "punif")$p.value), 0.001)
            side <- data$t[data$hit] < 0
            expect_lt(median(p_down[data$hit][side]), 1e-4)
            expect_lt(median(p_up[data$hit][!side]), 1e-4)
        }
    }
})

test_that("the parameters are recovered across several simulations", {
    errors <- vapply(11:16, function(seed) {
        data <- simulate_mixture(n = 2000, seed = seed)
        fit <- mixture_null_fit(data$t, data$x)
        truth <- true_theta()
        k_fit <- mixture_null_skew_constants(fit$theta[["alpha"]])
        k_true <- mixture_null_skew_constants(truth[["alpha"]])
        c(mean = fit$theta[["mu0"]] + exp(fit$theta[["s"]]) * k_fit$k1 -
            (truth[["mu0"]] + exp(truth[["s"]]) * k_true$k1),
            sd = exp(fit$theta[["s"]]) * k_fit$k2 -
                exp(truth[["s"]]) * k_true$k2,
            pi_0 = fit$weights[["null"]] - 0.85)
    }, numeric(3))
    expect_lt(max(abs(rowMeans(errors))), 0.08)
    expect_lt(max(abs(errors)), 0.2)
})

test_that("a pure skew-normal null is not split between the components", {
    # Regression test of the amendments A1 and A3: a sample of the null alone
    # must keep pi_0 near 1 and give uniform p-values, for any skewness of the
    # null (the skewed tail must not be taken by an alternative)
    for (alpha in c(-3, 0, 3)) {
        for (seed in 1:12) {
            data <- simulate_screen(seed, alpha)
            fit <- mixture_null_fit(data$t, data$x)
            info <- paste("alpha", alpha, "seed", seed)
            expect_equal(fit$convergence, 0L, info = info)
            expect_gt(fit$weights[["null"]], 0.9)
            k <- mixture_null_skew_constants(fit$theta[["alpha"]])
            null_sd <- exp(fit$theta[["s"]]) * k$k2
            expect_gte(null_sd, 0.93)
            expect_lte(null_sd, 1.07)
            for (lower in c(TRUE, FALSE)) {
                p <- mixture_null_cdf(fit, data$t, data$x, lower.tail = lower)
                expect_gt(suppressWarnings(stats::ks.test(p, "punif")$p.value),
                    0.001)
            }
        }
    }
})

test_that("a pure normal null gives the same p-values as the normal fit", {
    # alpha is weakly identified for a normal null, but the p-values must stay
    # uniform whatever the fitted alpha
    data <- simulate_screen(5, alpha = 0)
    fit <- mixture_null_fit(data$t, data$x)
    expect_lt(abs(fit$theta[["alpha"]]), 3)
    p <- mixture_null_cdf(fit, data$t, data$x)
    expect_lt(mean(p < 0.01), 0.02)
    expect_lt(mean(p < 0.05), 0.07)
})

test_that("the fit respects the bounds and the constraints", {
    bounds <- mixture_null_bounds()
    expect_equal(bounds$lower[["alpha"]], -10)
    expect_equal(bounds$upper[["alpha"]], 10)
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

test_that("the shape alpha is reported when it reaches the bound", {
    # Half-normal data: the skew-normal null wants an infinite shape
    set.seed(3)
    t <- abs(rnorm(3000))
    fit <- mixture_null_fit(t, rep(0, 3000))
    expect_equal(fit$convergence, 0L)
    expect_gt(fit$theta[["alpha"]], 5)
    expect_true(all(fit$theta <= mixture_null_bounds()$upper))
    expect_true("alpha" %in% fit$at_bound || fit$theta[["alpha"]] < 10)
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

test_that("the starts include the skewness-based and the symmetric shape", {
    data <- simulate_screen(2, alpha = 3)
    starts <- mixture_null_starts(data$t, 5)
    shapes <- vapply(starts, function(s) s[["alpha"]], numeric(1))
    expect_length(starts, 5L)
    expect_gt(shapes[1], 0.5)
    expect_equal(shapes[2], 0)
    expect_true(all(vapply(starts, length, integer(1)) == 11L))
    negative <- mixture_null_starts(-data$t, 5)
    expect_lt(negative[[1]][["alpha"]], -0.5)
    expect_equal(mixture_null_alpha_from_skewness(c(1, 2)), 0)
    expect_equal(mixture_null_alpha_from_skewness(rep(1, 5)), 0)
    # The sample skewness of normal data gives a small shape
    set.seed(1)
    expect_lt(abs(mixture_null_alpha_from_skewness(rnorm(1e5))), 0.8)
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

test_that("the neutral CDF of a normal null is accurate in the far tails", {
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, alpha = 0,
        a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    # Asymptotic expansion of log Phi(-z)
    log_tail <- function(z) {
        -z^2 / 2 - log(z) - 0.5 * log(2 * pi) +
            log(1 - 1 / z^2 + 3 / z^4 - 15 / z^6)
    }
    expect_equal(mixture_null_cdf(fit, -40, 0, log.p = TRUE), log_tail(40),
        tolerance = 1e-8)
    expect_equal(mixture_null_cdf(fit, 40, 0, lower.tail = FALSE,
        log.p = TRUE), log_tail(40), tolerance = 1e-8)
    expect_gt(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE), 0)
    expect_equal(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE),
        7.619853e-24, tolerance = 1e-6)
    expect_equal(mixture_null_cdf(fit, 0.3, 0) +
        mixture_null_cdf(fit, 0.3, 0, lower.tail = FALSE), 1)
})

test_that("the neutral CDF of a skew-normal null is accurate in the far tails", {
    for (alpha in c(-10, -3, 3, 10)) {
        fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
        t <- c(-40, -15, 15, 40)
        for (lower in c(TRUE, FALSE)) {
            lp <- mixture_null_cdf(fit, t, 0, lower.tail = lower, log.p = TRUE)
            expect_true(all(is.finite(lp)))
            expect_equal(lp, skew_normal_log_cdf(t, alpha, lower))
            expect_equal(mixture_null_cdf(fit, t, 0, lower.tail = lower),
                exp(lp))
        }
    }
    # The heavy side of the skew-normal null at the same distance from the
    # mean is more probable than the light side
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, alpha = 4,
        a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    expect_gt(mixture_null_cdf(fit, 3, 0, lower.tail = FALSE),
        mixture_null_cdf(fit, -3, 0))
})

test_that("the neutral CDF uses the location and the scale given x", {
    fit <- list(theta = c(mu0 = 1, beta = 0.5, s = log(2), gamma = 0.2,
        alpha = 1.5, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    x <- c(-1, 0, 2)
    t <- c(-3, 1.5, 4)
    z <- (t - (1 + 0.5 * x)) / exp(log(2) + 0.2 * x)
    expected <- vapply(z, function(v) {
        integrate(function(y) dskewnormal(y, 1.5), -60, v, rel.tol = 1e-12)$value
    }, numeric(1))
    expect_equal(mixture_null_cdf(fit, t, x), expected, tolerance = 1e-9)
    expect_equal(mixture_null_cdf(fit, t, x, log.p = TRUE), log(expected),
        tolerance = 1e-9)
    expect_equal(mixture_null_cdf(fit, t, x, lower.tail = FALSE), 1 - expected,
        tolerance = 1e-9)
    # A single value of x is used for all the genes
    expect_equal(mixture_null_cdf(fit, t, 0),
        mixture_null_cdf(fit, t, rep(0, 3)))
    expect_error(mixture_null_cdf(fit, t, c(0, 1)), "same length")
})

test_that("the density of the components adds up to the density of the mixture", {
    data <- simulate_mixture(n = 300, seed = 8)
    fit <- mixture_null_fit(data$t, data$x)
    grid <- seq(-12, 12, length.out = 401)
    x <- rep(0.3, length(grid))
    parts <- vapply(c("null", "down", "up"),
        function(component) mixture_null_density(fit, grid, x, component),
        numeric(length(grid)))
    all <- mixture_null_density(fit, grid, x, "all")
    expect_equal(unname(rowSums(parts)), all)
    expect_equal(mixture_null_density(fit, grid, x), parts[, "null"])
    # Each weighted density integrates to its weight
    step <- grid[2] - grid[1]
    expect_equal(colSums(parts) * step, fit$weights, tolerance = 1e-3)
    expect_equal(sum(all) * step, 1, tolerance = 1e-3)
    # The null density is the weighted skew-normal density
    p <- mixture_null_unpack(fit$theta)
    z <- (grid - (p$mu0 + p$beta * x)) / exp(p$s + p$gamma * x)
    expect_equal(parts[, "null"], unname(fit$weights[["null"]] *
        dskewnormal(z, p$alpha) / exp(p$s + p$gamma * x)))
    expect_error(mixture_null_density(fit, grid, x, "other"), "arg")
    expect_error(mixture_null_density(fit, grid, c(0, 1)), "same length")
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
    # Far from the null the alternatives take over
    expect_lt(max(posterior[401:404, "null"]), 0.01)
    expect_gt(posterior[403, "down"], 0.5)
    expect_gt(posterior[404, "up"], 0.5)
    # The posterior is the ratio of the weighted densities
    ratio <- mixture_null_density(fit, data$t, data$x, "null") /
        mixture_null_density(fit, data$t, data$x, "all")
    expect_equal(unname(posterior[seq_along(data$t), "null"]), ratio)
})

test_that("pi_0 close to 1 is handled", {
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
    expect_equal(fit$theta[["alpha"]], 0)
    expect_equal(fit$theta[["beta"]], 0)
    expect_equal(fit$theta[["gamma"]], 0)
    expect_equal(fit$at_bound, character(0))
    # The fallback is a normal null and gives the p-values of central matching
    p <- mixture_null_cdf(fit, data$t, data$x)
    expect_true(all(p >= 0 & p <= 1))
    expect_equal(p, pnorm(data$t, central[["mean"]], central[["sd"]]))
    # Too few genes also fall back
    expect_warning(mixture_null_fit(rnorm(5), rep(0, 5)), "central matching")
})

test_that("invalid input is refused", {
    expect_error(mixture_null_fit(c(1, NA, 3), c(0, 0, 0)), "finite")
    expect_error(mixture_null_fit(c(1, 2, 3), c(0, 0)), "same length")
    expect_error(mixture_null_fit("a", 0), "numeric")
    expect_error(mixture_null_fit(rnorm(50), rep(0, 50), n_starts = 0),
        "n_starts")
    expect_error(mixture_null_loglik(1:5, rnorm(10), rep(0, 10)), "length 11")
    expect_error(mixture_null_gradient(rep(NA_real_, 11), rnorm(10),
        rep(0, 10)), "length 11")
})
