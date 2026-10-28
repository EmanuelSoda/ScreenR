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

# Skew-normal density of the standard distribution, from the sn package
dskewnormal <- function(z, alpha) sn::dsn(z, 0, 1, alpha)

# The profile fit optimises the 10 parameters at every alpha of a grid of 33
# or more values: the tests use one default start at each grid point (the
# warm starts of the continuation do the rest) and small samples, to stay
# fast. The default of mixture_null_fit is used by the determinism tests.
fast_fit <- function(t, x) mixture_null_fit(t, x, n_starts = 1)

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

test_that("the log density equals sn::dsn", {
    set.seed(1)
    for (alpha in c(-10, -3, -0.5, 0, 0.5, 3, 10)) {
        theta <- true_theta()
        theta[["alpha"]] <- alpha
        theta[["beta"]] <- 0.3
        theta[["gamma"]] <- -0.2
        t <- c(rnorm(200, 0, 3), -40, 40)
        x <- c(rnorm(200, 0, 0.5), 0.5, -0.5)
        parts <- mixture_null_components(theta, t, x)
        p <- mixture_null_unpack(theta)
        expected <- sn::dsn(t, p$mu0 + p$beta * x, exp(p$s + p$gamma * x),
            alpha, log = TRUE) + log(p$weights[["null"]])
        expect_equal(unname(parts$log_f[, "null"]), expected,
            tolerance = 1e-12, info = paste("alpha", alpha))
    }
    # and the closed form 2 phi(z) Phi(alpha z)
    expect_equal(dskewnormal(c(-2, 0.3, 4), 3),
        2 * dnorm(c(-2, 0.3, 4)) * pnorm(3 * c(-2, 0.3, 4)))
})

test_that("the neutral CDF agrees with sn::psn in both tails", {
    # Moderate z: the lower tail is sn::psn, the upper tail is its mirror image
    z <- c(-6, -4, -2.5, -1, -0.2, 0, 0.2, 1, 2.5, 4, 6)
    for (alpha in c(-10, -5, -3, -1, -0.3, 0, 0.1, 0.5, 1, 2, 3, 5, 10)) {
        fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
        lower <- mixture_null_cdf(fit, z, 0)
        upper <- mixture_null_cdf(fit, z, 0, lower.tail = FALSE)
        expect_equal(lower, sn::psn(z, 0, 1, alpha), info = paste(alpha))
        expect_equal(upper, sn::psn(-z, 0, 1, -alpha), info = paste(alpha))
        # The two tails add up to 1
        expect_equal(lower + upper, rep(1, length(z)), info = paste(alpha))
        # The log of the p-value is the log of the value
        expect_equal(mixture_null_cdf(fit, z, 0, log.p = TRUE), log(lower))
        expect_equal(mixture_null_cdf(fit, z, 0, lower.tail = FALSE,
            log.p = TRUE), log(upper))
    }
})

test_that("the symmetry identity of the skew-normal tails holds", {
    # 1 - F(t; xi, omega, alpha) = F(-t; -xi, omega, -alpha), for the upper
    # tail by sn::psn and for the mixture_null_cdf of the mirrored fit
    t <- c(-3, -0.7, 0.4, 2.2, 5)
    x <- c(-1, 0.3, 0, 0.8, 1.5)
    for (alpha in c(-4, -0.8, 0.8, 4)) {
        theta <- c(mu0 = 0.4, beta = 0.3, s = 0.2, gamma = -0.1,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0)
        mirrored <- theta
        mirrored[c("mu0", "beta", "alpha")] <- -theta[c("mu0", "beta", "alpha")]
        fit <- list(theta = theta)
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = FALSE),
            mixture_null_cdf(list(theta = mirrored), -t, x))
        xi <- 0.4 + 0.3 * x
        omega <- exp(0.2 - 0.1 * x)
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = FALSE),
            sn::psn(-t, -xi, omega, -alpha))
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = FALSE),
            1 - sn::psn(t, xi, omega, alpha))
    }
})

test_that("the neutral CDF matches the numerical integral of the density", {
    # Independent of sn: integral of the density, computed relative to its
    # maximum so that it does not underflow
    worst <- 0
    for (alpha in c(-10, -3, -1, 0.5, 2, 5, 10)) {
        fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
        # z near 0 is included: a quadrature of Owen's T truncated there lost
        # 1e-5 of the upper tail of a negative alpha
        for (z in c(-6, -3, -1, -1e-3, 0, 1e-5, 1e-3, 1, 3, 6)) {
            for (lower in c(TRUE, FALSE)) {
                expected <- exp(log_tail_integral(z, alpha, lower))
                got <- mixture_null_cdf(fit, z, 0, lower.tail = lower)
                # Relative error, where sn::psn is accurate (see below for
                # the tail that it loses beyond 1e-10)
                if (expected > 1e-10) {
                    worst <- max(worst, abs(got / expected - 1))
                }
            }
        }
    }
    expect_lt(worst, 1e-6)
})

test_that("the neutral CDF of a normal null is accurate in the far tails", {
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, alpha = 0,
        a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    # Asymptotic expansion of log Phi(-z)
    log_tail <- function(z) {
        -z^2 / 2 - log(z) - 0.5 * log(2 * pi) +
            log(1 - 1 / z^2 + 3 / z^4 - 15 / z^6)
    }
    expect_equal(mixture_null_cdf(fit, -30, 0, log.p = TRUE), log_tail(30),
        tolerance = 1e-8)
    expect_equal(mixture_null_cdf(fit, 30, 0, lower.tail = FALSE,
        log.p = TRUE), log_tail(30), tolerance = 1e-8)
    expect_gt(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE), 0)
    expect_equal(mixture_null_cdf(fit, 10, 0, lower.tail = FALSE),
        7.619853e-24, tolerance = 1e-6)
    expect_equal(mixture_null_cdf(fit, 0.3, 0) +
        mixture_null_cdf(fit, 0.3, 0, lower.tail = FALSE), 1)
    # Same as pnorm
    expect_equal(mixture_null_cdf(fit, c(-12, -2, 4), 0),
        pnorm(c(-12, -2, 4)))
})

test_that("the log of an underflowing p-value is -Inf, never NaN", {
    fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0, alpha = 0,
        a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
    # pnorm underflows to 0 beyond about 38 sd
    expect_equal(mixture_null_cdf(fit, -40, 0), 0)
    expect_equal(mixture_null_cdf(fit, -40, 0, log.p = TRUE), -Inf)
    expect_equal(mixture_null_cdf(fit, 40, 0, lower.tail = FALSE,
        log.p = TRUE), -Inf)
    # A skew-normal tail that psn loses before the double range ends: the
    # value is finite, in [0, 1], and the log is finite or -Inf
    fit$theta[["alpha"]] <- 3
    z <- c(-60, -30, -12, -8, -5, 5, 30, 60)
    for (lower in c(TRUE, FALSE)) {
        p <- mixture_null_cdf(fit, z, 0, lower.tail = lower)
        expect_true(all(is.finite(p)) && all(p >= 0 & p <= 1))
        expect_false(anyNA(mixture_null_cdf(fit, z, 0, lower.tail = lower,
            log.p = TRUE)))
    }
    # The heavy tails stay accurate far out (alpha = 3: lower tail of the
    # negative side is the light one)
    expect_gt(mixture_null_cdf(fit, 5, 0, lower.tail = FALSE), 0)
})

test_that("sn::psn is accurate in the tails used to call hits", {
    # Probabilities down to 1e-10, in the light tail of every shape: the
    # relative error against the numerical integral stays below 1e-6
    for (alpha in c(-5, -1, -0.5, 0.5, 1, 5)) {
        fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
        light_is_lower <- alpha > 0
        zs <- if (light_is_lower) -(3:6) else 3:6
        for (z in zs) {
            expected <- exp(log_tail_integral(z, alpha, light_is_lower))
            got <- mixture_null_cdf(fit, z, 0, lower.tail = light_is_lower)
            if (expected > 1e-10) {
                expect_equal(got, expected, tolerance = 1e-6,
                    info = paste("alpha", alpha, "z", z))
            }
        }
    }
})

test_that("the parameters are recovered from a simulated screen", {
    data <- simulate_mixture(n = 2000, seed = 1)
    fit <- fast_fit(data$t, data$x)
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
        for (seed in 1) {
            data <- simulate_screen(seed, alpha, n = 1000, hits = 0.1)
            fit <- fast_fit(data$t, data$x)
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
            p_down <- mixture_null_cdf(fit, data$t, data$x,
                conservative = FALSE)
            p_up <- mixture_null_cdf(fit, data$t, data$x, lower.tail = FALSE,
                conservative = FALSE)
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
    errors <- vapply(11:13, function(seed) {
        data <- simulate_mixture(n = 800, seed = seed)
        fit <- fast_fit(data$t, data$x)
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

test_that("a pure skew-normal null is not split and is calibrated", {
    # Regression test of the amendments A1 and A3: a sample of the null alone
    # must keep pi_0 near 1 and give uniform p-values at the maximum
    # likelihood estimate, for any skewness of the null (the skewed tail must
    # not be taken by an alternative). The conservative p-values (amendment
    # A4) are never smaller than the maximum likelihood ones and are never
    # anti-conservative: the one sided Kolmogorov-Smirnov test against the
    # uniform distribution (alternative "greater": more small p-values than
    # the uniform) does not reject, and the small p-values are not in excess.
    # 800 genes: with a few hundred a pure null can truly be split (the
    # maximum likelihood estimate itself has pi_0 near 0.5), as in the legacy
    # multi-start fit.
    small <- list(conservative = numeric(0), mle = numeric(0))
    for (alpha in c(-3, 0, 3)) {
        for (seed in 1:12) {
            data <- simulate_screen(seed, alpha, n = 800)
            fit <- fast_fit(data$t, data$x)
            info <- paste("alpha", alpha, "seed", seed)
            expect_equal(fit$convergence, 0L, info = info)
            expect_gt(fit$weights[["null"]], 0.9)
            k <- mixture_null_skew_constants(fit$theta[["alpha"]])
            null_sd <- exp(fit$theta[["s"]]) * k$k2
            expect_gte(null_sd, 0.8, label = info)
            expect_lte(null_sd, 1.2, label = info)
            for (lower in c(TRUE, FALSE)) {
                p_mle <- mixture_null_cdf(fit, data$t, data$x,
                    lower.tail = lower, conservative = FALSE)
                p_cons <- mixture_null_cdf(fit, data$t, data$x,
                    lower.tail = lower)
                expect_true(all(p_cons >= p_mle), info = info)
                expect_gt(suppressWarnings(
                    stats::ks.test(p_mle, "punif")$p.value), 0.001)
                expect_gt(suppressWarnings(stats::ks.test(p_cons, "punif",
                    alternative = "greater")$p.value), 0.001)
                small$mle <- c(small$mle, mean(p_mle < 0.05))
                small$conservative <- c(small$conservative,
                    mean(p_cons < 0.05))
            }
        }
    }
    # Over the 72 samples the conservative envelope rejects no more than the
    # nominal level (the maximum likelihood p-values are at the level, up to
    # the noise of a fit on 800 genes)
    expect_lte(mean(small$conservative), 0.05)
    expect_lte(mean(small$conservative), mean(small$mle))
    expect_lt(mean(small$mle), 0.07)
})

test_that("a pure normal null gives the same p-values as the normal fit", {
    # alpha is weakly identified for a normal null, but the p-values must stay
    # uniform whatever the fitted alpha
    data <- simulate_screen(5, alpha = 0, n = 800)
    fit <- fast_fit(data$t, data$x)
    expect_lt(abs(fit$theta[["alpha"]]), 3)
    p <- mixture_null_cdf(fit, data$t, data$x, conservative = FALSE)
    expect_lt(mean(p < 0.01), 0.02)
    expect_lt(mean(p < 0.05), 0.07)
})

test_that("the fit respects the bounds and the constraints", {
    bounds <- mixture_null_bounds()
    expect_equal(bounds$lower[["alpha"]], -10)
    expect_equal(bounds$upper[["alpha"]], 10)
    for (seed in 1) {
        data <- simulate_mixture(n = 300, seed = seed)
        fit <- fast_fit(data$t, data$x)
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
    t <- abs(rnorm(600))
    fit <- fast_fit(t, rep(0, 600))
    expect_equal(fit$convergence, 0L)
    expect_gt(fit$theta[["alpha"]], 5)
    expect_true(all(fit$theta <= mixture_null_bounds()$upper))
    expect_true("alpha" %in% fit$at_bound || fit$theta[["alpha"]] < 10)
})

test_that("a bound hit at the optimum is reported", {
    # Heavy tails need wide alternatives: the weights go to the bound
    set.seed(2)
    t <- c(rnorm(500), rnorm(500, sd = 4))
    fit <- fast_fit(t, rep(0, 1000))
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
    # The default fit (the other tests use one start at each grid point)
    data <- simulate_mixture(n = 80, seed = 5)
    first <- mixture_null_fit(data$t, data$x)
    second <- mixture_null_fit(data$t, data$x)
    expect_identical(first, second)
})

test_that("the fit does not depend on the order of the genes", {
    # Exactly identical, not only equal: the genes are sorted inside the fit
    data <- simulate_mixture(n = 120, seed = 15)
    base <- fast_fit(data$t, data$x)
    set.seed(1)
    shuffled <- sample(length(data$t))
    expect_identical(fast_fit(data$t[shuffled], data$x[shuffled]), base)
    expect_identical(fast_fit(rev(data$t), rev(data$x)), base)
})

test_that("more starts never give a clearly worse likelihood", {
    data <- simulate_mixture(n = 100, seed = 6)
    one <- mixture_null_fit(data$t, data$x, n_starts = 1)
    many <- mixture_null_fit(data$t, data$x, n_starts = 2)
    # The optimiser stops at a relative tolerance of 1e-9 of the mean
    # log-likelihood, so equal fits differ by about 1e-4 in total
    expect_gte(many$loglik, one$loglik - 1e-3)
    expect_gte(many$starts_converged, one$starts_converged)
})

test_that("the profile has the grid, the interval and the maximum", {
    data <- simulate_screen(3, alpha = 2, n = 400, hits = 0.1)
    fit <- fast_fit(data$t, data$x)
    names_theta <- mixture_null_bounds()$names
    expect_s3_class(fit$profile, "data.frame")
    expect_named(fit$profile, c("alpha", "loglik",
        setdiff(names_theta, "alpha")))
    # Step 0.25 over -4 to 4 at least, inside the bounds of alpha
    expect_gte(nrow(fit$profile), 33L)
    expect_equal(diff(fit$profile$alpha), rep(0.25, nrow(fit$profile) - 1L))
    expect_lte(min(fit$profile$alpha), -4)
    expect_gte(max(fit$profile$alpha), 4)
    expect_true(all(abs(fit$profile$alpha) <= 10))
    expect_equal(fit$alpha_hat, fit$theta[["alpha"]])
    # The maximum is at least as good as every grid point, and is refined
    # between the neighbours of the best one
    expect_gte(fit$loglik, max(fit$profile$loglik) - 1e-6)
    best <- fit$profile$alpha[which.max(fit$profile$loglik)]
    expect_lte(abs(fit$alpha_hat - best), 0.25 + 1e-9)
    # The grid logliks are the log-likelihood of the profiled parameters
    row <- fit$profile[7, ]
    theta <- unlist(row[names_theta])
    expect_equal(row$loglik, mixture_null_loglik(theta, data$t, data$x),
        tolerance = 1e-9)
    # The interval is the set where the profile is within qchisq(.95, 1) / 2
    cut <- fit$loglik - stats::qchisq(0.95, 1) / 2
    inside <- fit$profile$alpha[fit$profile$loglik >= cut]
    expect_named(fit$alpha_ci, c("lower", "upper"))
    expect_lte(fit$alpha_ci[["lower"]], fit$alpha_hat)
    expect_gte(fit$alpha_ci[["upper"]], fit$alpha_hat)
    expect_lte(fit$alpha_ci[["lower"]], min(inside))
    expect_gte(fit$alpha_ci[["lower"]], min(inside) - 0.25 - 1e-9)
    expect_gte(fit$alpha_ci[["upper"]], max(inside))
    expect_lte(fit$alpha_ci[["upper"]], max(inside) + 0.25 + 1e-9)
    # The interval does not touch the edge of the grid (it would be extended)
    expect_lt(fit$profile$loglik[1], cut)
    expect_lt(fit$profile$loglik[nrow(fit$profile)], cut)
})

test_that("the grid is extended while the interval touches its edge", {
    # Half-normal data: the likelihood keeps growing with alpha up to the bound
    set.seed(4)
    t <- abs(rnorm(600))
    fit <- fast_fit(t, rep(0, 600))
    expect_gt(max(fit$profile$alpha), 4)
    expect_lte(max(fit$profile$alpha), 10)
    expect_equal(fit$alpha_ci[["upper"]], 10)
})

test_that("the conservative p-values are never smaller than the MLE ones", {
    data <- simulate_screen(2, alpha = 1, n = 500, hits = 0.1)
    fit <- fast_fit(data$t, data$x)
    expect_gt(fit$alpha_ci[["upper"]] - fit$alpha_ci[["lower"]], 0.3)
    t <- c(data$t, seq(-9, 9, by = 0.5))
    x <- c(data$x, rep(c(-0.5, 0.4, 0), length.out = 37))
    for (lower in c(TRUE, FALSE)) {
        mle <- mixture_null_cdf(fit, t, x, lower.tail = lower,
            conservative = FALSE)
        cons <- mixture_null_cdf(fit, t, x, lower.tail = lower)
        expect_true(all(cons >= mle))
        expect_true(any(cons > mle))
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = lower,
            conservative = TRUE), cons)
        # log.p is the log of the same values
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = lower,
            log.p = TRUE), log(cons))
        expect_equal(mixture_null_cdf(fit, t, x, lower.tail = lower,
            log.p = TRUE, conservative = FALSE), log(mle))
        # The MLE value is the tail of theta alone
        expect_equal(mle, mixture_null_tail(fit$theta, t, x, lower))
    }
    # The envelope is the pointwise maximum over the profiled grid points
    # inside the interval and the maximum likelihood estimate
    cut <- fit$loglik - stats::qchisq(0.95, 1) / 2
    inside <- fit$profile[fit$profile$loglik >= cut, ]
    names_theta <- mixture_null_bounds()$names
    expected <- mixture_null_tail(fit$theta, t, x, TRUE)
    for (i in seq_len(nrow(inside))) {
        expected <- pmax(expected, mixture_null_tail(
            unlist(inside[i, names_theta]), t, x, TRUE))
    }
    expect_equal(mixture_null_cdf(fit, t, x), expected)
    # Without a profile (a hand made fit) both are the tail of theta
    plain <- list(theta = fit$theta)
    expect_equal(mixture_null_cdf(plain, t, x),
        mixture_null_cdf(plain, t, x, conservative = FALSE))
})

test_that("the alpha interval covers the true alpha in a small simulation", {
    # The coverage over 100 or more replicates is reported outside the tests
    covered <- vapply(1:6, function(seed) {
        alpha <- c(-2, 0, 2)[(seed - 1L) %% 3L + 1L]
        data <- simulate_screen(100 + seed, alpha, n = 250, hits = 0.05)
        fit <- fast_fit(data$t, data$x)
        fit$alpha_ci[["lower"]] <= alpha && alpha <= fit$alpha_ci[["upper"]]
    }, logical(1))
    expect_gte(sum(covered), 5L)
})

test_that("the neutral CDF of a skew-normal null is finite where it should be", {
    for (alpha in c(-10, -3, 3, 10)) {
        fit <- list(theta = c(mu0 = 0, beta = 0, s = 0, gamma = 0,
            alpha = alpha, a = 1, b = 0, dD = 0, kD = 0, dU = 0, kU = 0))
        # The heavy tail (the lower one for alpha < 0) is finite far out,
        # up to the underflow of the double (about 37 sd)
        t <- if (alpha < 0) c(-30, -15) else c(15, 30)
        lp <- mixture_null_cdf(fit, t, 0, lower.tail = alpha < 0, log.p = TRUE)
        expect_true(all(is.finite(lp)))
        expect_equal(exp(lp), mixture_null_cdf(fit, t, 0,
            lower.tail = alpha < 0))
        # Beyond the double range the log is -Inf, as documented
        expect_equal(mixture_null_cdf(fit, 40 * sign(alpha), 0,
            lower.tail = alpha < 0, log.p = TRUE), -Inf)
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
    fit <- fast_fit(data$t, data$x)
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
    # R1: the null component is the weight times sn::dsn
    xi <- p$mu0 + p$beta * x
    omega <- exp(p$s + p$gamma * x)
    expect_equal(mixture_null_density(fit, grid, x, "null"),
        fit$weights[["null"]] * sn::dsn(grid, xi, omega, p$alpha),
        tolerance = 1e-12)
    expect_equal(all, unname(fit$weights[["null"]] *
        sn::dsn(grid, xi, omega, p$alpha) + parts[, "down"] + parts[, "up"]),
        tolerance = 1e-12)
    expect_error(mixture_null_density(fit, grid, x, "other"), "arg")
    expect_error(mixture_null_density(fit, grid, c(0, 1)), "same length")
})

test_that("the posterior probabilities are a probability for each gene", {
    data <- simulate_mixture(n = 400, seed = 7)
    fit <- fast_fit(data$t, data$x)
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
    n <- 600
    nbarcodes <- sample(3:10, n, replace = TRUE)
    x <- log(nbarcodes) - mean(log(nbarcodes))
    t <- rnorm(n, 0.1, 1.2)
    fit <- fast_fit(t, x)
    expect_equal(fit$convergence, 0L)
    expect_false(fit$fallback)
    expect_gte(fit$weights[["null"]], 0.5)
    expect_equal(sum(fit$weights), 1)
    expect_true(all(is.finite(fit$theta)))
    p <- mixture_null_cdf(fit, t, x)
    expect_true(all(p >= 0 & p <= 1))
    set.seed(9)
    shrunk <- fast_fit(rnorm(800), rep(0, 800))
    expect_equal(shrunk$convergence, 0L)
    expect_true(is.finite(shrunk$loglik))
})

test_that("about 100 genes are fitted", {
    data <- simulate_mixture(n = 100, seed = 9)
    fit <- fast_fit(data$t, data$x)
    expect_equal(fit$n, 100L)
    expect_true(all(is.finite(fit$theta)))
    expect_true(is.finite(fit$loglik))
    expect_equal(sum(fit$weights), 1)
    expect_true(fit$convergence == 0L || fit$fallback)
})

test_that("a constant covariate gives a stable fit", {
    data <- simulate_mixture(n = 400, seed = 10)
    fit <- fast_fit(data$t, rep(0, 400))
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
    # There is no profile: alpha_hat is 0 and the interval is unknown
    expect_null(fit$profile)
    expect_equal(fit$alpha_hat, 0)
    expect_true(all(is.na(fit$alpha_ci)))
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
