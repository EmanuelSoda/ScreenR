#' @title Names and box bounds of the parameters of the mixture null
#' @description Internal function: the parameters of the mixture null, in the
#'     order used by \code{\link{mixture_null_fit}}, and the box bounds used by
#'     the optimiser. mu0 and s (the location and the log scale of the null)
#'     are not bounded.
#' @return A list with the parameter names, the lower and the upper bounds.
#' @keywords internal
mixture_null_bounds <- function() {
    names <- c("mu0", "beta", "s", "gamma", "alpha", "a", "b", "dD", "kD",
        "dU", "kU")
    list(
        names = names,
        lower = stats::setNames(
            c(-Inf, -5, -Inf, -2, -10, -15, -15, -6, -6, -6, -6), names),
        upper = stats::setNames(
            c(Inf, 5, Inf, 2, 10, 15, 15, 4, 3, 4, 3), names)
    )
}

#' @title Check the arguments of the mixture null
#' @description Internal function: checks that the statistics and the
#'     covariate are numeric and finite and gives the covariate the length of
#'     the statistics (a single value is repeated).
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @return A list with t and x, both of the same length.
#' @keywords internal
mixture_null_check_data <- function(t, x) {
    if (!is.numeric(t) || !is.numeric(x)) {
        stop("t and x must be numeric.")
    }
    if (length(x) == 1L) {
        x <- rep(x, length(t))
    }
    if (length(x) != length(t)) {
        stop("t and x must have the same length.")
    }
    if (any(!is.finite(t)) || any(!is.finite(x))) {
        stop("t and x must be finite.")
    }
    list(t = as.numeric(t), x = as.numeric(x))
}

#' @title Unpack the parameters of the mixture null
#' @description Internal function: transforms the unconstrained parameters
#'     theta into the location, the scale and the shape of the null, the
#'     shifts and the widths of the alternatives and the weights of the
#'     mixture. The null is the skew-normal distribution with location
#'     xi(x) = mu0 + beta x, scale omega(x) = exp(s + gamma x) and shape alpha,
#'     whose mean m0(x) and standard deviation sd0(x) are given by
#'     \code{\link{mixture_null_skew_constants}}. The depleted and the
#'     enriched components are normal, centred at least three null sd below and
#'     above the mean of the null (at m0(x) -/+ sd0(x) (3 + exp(d))) and wider
#'     than the null (sd0(x) (1 + exp(k))). The minimum distance of three null
#'     sd makes the null identifiable: with a smaller one, a pure null sample
#'     is split between the components and the null spread is underestimated.
#'     Effects smaller than about three null sd are therefore absorbed by the
#'     null (wider spread, conservative p-values) and never the reverse. The
#'     weights are pi_0 = 0.5 + 0.5 plogis(a), pi_D = (1 - pi_0) plogis(b) and
#'     pi_U = 1 - pi_0 - pi_D.
#' @param theta The vector c(mu0, beta, s, gamma, alpha, a, b, dD, kD, dU, kU).
#' @return A list with the named parameters and the weights.
#' @keywords internal
mixture_null_unpack <- function(theta) {
    if (!is.numeric(theta) || length(theta) != 11L || any(is.na(theta))) {
        stop("theta must be a numeric vector of length 11 without NA.")
    }
    p <- as.list(stats::setNames(as.numeric(theta),
        mixture_null_bounds()$names))
    p$weights <- mixture_null_weights(p$a, p$b)
    p
}

#' @title Weights of the mixture null
#' @description Internal function: the weights pi_0, pi_D and pi_U from the
#'     parameters a and b. pi_0 is at least 0.5.
#' @param a,b Unconstrained parameters of the weights.
#' @return A named vector with null, down and up.
#' @keywords internal
mixture_null_weights <- function(a, b) {
    pi_0 <- 0.5 + 0.5 * stats::plogis(a)
    pi_d <- 0.5 * stats::plogis(-a) * stats::plogis(b)
    pi_u <- 0.5 * stats::plogis(-a) * stats::plogis(-b)
    c(null = pi_0, down = pi_d, up = pi_u)
}

#' @title Mean and sd of the skew-normal null
#' @description Internal function: for the skew-normal distribution with shape
#'     alpha, delta = alpha / sqrt(1 + alpha^2), the mean is
#'     location + scale k1 with k1 = delta sqrt(2 / pi) and the standard
#'     deviation is scale k2 with k2 = sqrt(1 - 2 delta^2 / pi). The
#'     derivatives with respect to alpha are used by the gradient.
#' @param alpha The shape of the skew-normal distribution.
#' @return A list with k1, k2 and their derivatives dk1 and dk2.
#' @keywords internal
mixture_null_skew_constants <- function(alpha) {
    delta <- alpha / sqrt(1 + alpha^2)
    d_delta <- (1 + alpha^2)^(-1.5)
    k1 <- delta * sqrt(2 / pi)
    k2 <- sqrt(1 - 2 * delta^2 / pi)
    list(k1 = k1, k2 = k2, dk1 = d_delta * sqrt(2 / pi),
        dk2 = -2 * delta * d_delta / (pi * k2))
}

#' @title Log density of the components of the mixture null
#' @description Internal function: the log of the weight times the density of
#'     each component, for each gene, with the intermediate values needed by
#'     the gradient. The log of the skew-normal density is
#'     log 2 - log omega + log phi(z) + log Phi(alpha z), with
#'     z = (t - xi) / omega, and log Phi comes from \code{pnorm(log.p = TRUE)}.
#' @param theta The parameters of the mixture null.
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @return A list with the matrix of the log weighted densities (columns null,
#'     down, up) and the standardized values and scales used by the gradient.
#' @keywords internal
mixture_null_components <- function(theta, t, x) {
    p <- mixture_null_unpack(theta)
    k <- mixture_null_skew_constants(p$alpha)
    xi <- p$mu0 + p$beta * x
    omega <- exp(p$s + p$gamma * x)
    m0 <- xi + omega * k$k1
    s0 <- omega * k$k2
    c_d <- 3 + exp(p$dD)
    v_d <- 1 + exp(p$kD)
    c_u <- 3 + exp(p$dU)
    v_u <- 1 + exp(p$kU)
    z <- (t - xi) / omega
    u <- p$alpha * z
    log_big_phi <- stats::pnorm(u, log.p = TRUE)
    sd_d <- s0 * v_d
    sd_u <- s0 * v_u
    w_d <- (t - (m0 - s0 * c_d)) / sd_d
    w_u <- (t - (m0 + s0 * c_u)) / sd_u
    log_weight <- c(
        null = log(0.5 + 0.5 * stats::plogis(p$a)),
        down = log(0.5) + stats::plogis(-p$a, log.p = TRUE) +
            stats::plogis(p$b, log.p = TRUE),
        up = log(0.5) + stats::plogis(-p$a, log.p = TRUE) +
            stats::plogis(-p$b, log.p = TRUE)
    )
    half_log_2pi <- 0.5 * log(2 * pi)
    log_density <- cbind(
        null = log(2) - log(omega) - half_log_2pi - 0.5 * z^2 + log_big_phi,
        down = -0.5 * w_d^2 - log(sd_d) - half_log_2pi,
        up = -0.5 * w_u^2 - log(sd_u) - half_log_2pi
    )
    list(
        log_f = sweep(log_density, 2, log_weight, "+"),
        z = z, u = u, log_big_phi = log_big_phi, w_d = w_d, w_u = w_u,
        xi = xi, omega = omega, m0 = m0, s0 = s0, sd_d = sd_d, sd_u = sd_u,
        c_d = c_d, v_d = v_d, c_u = c_u, v_u = v_u, k = k, params = p
    )
}

#' @title Log-sum-exp of the rows of a matrix
#' @description Internal function: stable computation of the log of the sum of
#'     the exponentials of each row.
#' @param m A numeric matrix.
#' @return A vector with one value for each row.
#' @keywords internal
mixture_null_row_logsumexp <- function(m) {
    top <- do.call(pmax, lapply(seq_len(ncol(m)), function(j) m[, j]))
    top + log(rowSums(exp(m - top)))
}

#' @title Log-likelihood of the mixture null
#' @description Internal function: the log-likelihood of the mixture
#'     pi_0 f_0(t | x) + pi_D f_D(t | x) + pi_U f_U(t | x), computed with the
#'     log-sum-exp. See \code{\link{mixture_null_unpack}} for the
#'     parameterisation.
#' @param theta The vector c(mu0, beta, s, gamma, alpha, a, b, dD, kD, dU, kU).
#' @param t The gene statistics.
#' @param x The covariate of the genes (the centred log of the number of
#'     barcodes).
#' @return A scalar.
#' @keywords internal
mixture_null_loglik <- function(theta, t, x) {
    data <- mixture_null_check_data(t, x)
    parts <- mixture_null_components(theta, data$t, data$x)
    sum(mixture_null_row_logsumexp(parts$log_f))
}

#' @title Gradient of the log-likelihood of the mixture null
#' @description Internal function: the analytic gradient of
#'     \code{\link{mixture_null_loglik}} with respect to theta. The centres and
#'     the widths of the alternatives are proportional to the mean and the sd
#'     of the null, so mu0, beta, s, gamma and alpha act on all the components.
#'     The derivative of log Phi(alpha z) is alpha phi(alpha z) / Phi(alpha z),
#'     computed as exp(log phi - log Phi) so that it is stable when alpha z is
#'     very negative.
#' @inheritParams mixture_null_loglik
#' @return A named vector of length 11.
#' @keywords internal
mixture_null_gradient <- function(theta, t, x) {
    data <- mixture_null_check_data(t, x)
    t <- data$t
    x <- data$x
    parts <- mixture_null_components(theta, t, x)
    p <- parts$params
    k <- parts$k
    total <- mixture_null_row_logsumexp(parts$log_f)
    # Responsibilities of the components
    r <- exp(parts$log_f - total)
    # Null: derivatives of the log density with respect to the location xi,
    # to log omega and to alpha (ratio = phi(alpha z) / Phi(alpha z))
    z <- parts$z
    ratio <- exp(stats::dnorm(parts$u, log = TRUE) - parts$log_big_phi)
    g_xi_null <- (z - p$alpha * ratio) / parts$omega
    g_lo_null <- -1 + z^2 - p$alpha * z * ratio
    g_al_null <- z * ratio
    # Alternatives: derivatives of the log density with respect to the centre
    # (gm) and to the log of the sd (gs)
    gm_d <- parts$w_d / parts$sd_d
    gm_u <- parts$w_u / parts$sd_u
    gs_d <- parts$w_d^2 - 1
    gs_u <- parts$w_u^2 - 1
    r_n <- r[, "null"]
    r_d <- r[, "down"]
    r_u <- r[, "up"]
    omega <- parts$omega
    s0 <- parts$s0
    # The centres move with xi (the mean of the null is xi + omega k1) and the
    # sd with omega (sd0 = omega k2)
    g_xi <- r_n * g_xi_null + r_d * gm_d + r_u * gm_u
    d_mean_lo <- omega * k$k1
    g_lo <- r_n * g_lo_null +
        r_d * (gm_d * (d_mean_lo - parts$c_d * s0) + gs_d) +
        r_u * (gm_u * (d_mean_lo + parts$c_u * s0) + gs_u)
    g_al <- r_n * g_al_null +
        r_d * (gm_d * omega * (k$dk1 - parts$c_d * k$dk2) +
            gs_d * k$dk2 / k$k2) +
        r_u * (gm_u * omega * (k$dk1 + parts$c_u * k$dk2) +
            gs_u * k$dk2 / k$k2)
    pi_0 <- 0.5 + 0.5 * stats::plogis(p$a)
    d_pi_0 <- 0.5 * stats::plogis(p$a) * stats::plogis(-p$a)
    # 1 - pi_0 = 0.5 plogis(-a): avoids the cancellation for large a
    grad_a <- sum(r_n * d_pi_0 / pi_0 -
        (r_d + r_u) * d_pi_0 / (0.5 * stats::plogis(-p$a)))
    grad_b <- sum(r_d * stats::plogis(-p$b) - r_u * stats::plogis(p$b))
    c(
        mu0 = sum(g_xi),
        beta = sum(g_xi * x),
        s = sum(g_lo),
        gamma = sum(g_lo * x),
        alpha = sum(g_al),
        a = grad_a,
        b = grad_b,
        dD = sum(r_d * gm_d * -s0 * exp(p$dD)),
        kD = sum(r_d * gs_d) * exp(p$kD) / parts$v_d,
        dU = sum(r_u * gm_u * s0 * exp(p$dU)),
        kU = sum(r_u * gs_u) * exp(p$kU) / parts$v_u
    )
}

#' @title Starting values of the mixture null
#' @description Internal function: deterministic starting values for the
#'     optimiser, derived from the central matching fit of the statistics.
#'     The shape alpha of the first start is estimated from the sample
#'     skewness of the central part of the statistics (within 2.5 sd of the
#'     centre), the second start is symmetric (alpha = 0) and the others mix
#'     them; the location and the scale are set so that the mean and the sd of
#'     the null are those of the central fit. The start k has beta = gamma = 0,
#'     pi_0 in 0.95, 0.9, 0.8, ... and different shifts and widths of the
#'     alternatives (always at least three null sd from the centre of the
#'     null).
#' @param t The gene statistics.
#' @param n_starts The number of starting values.
#' @return A list of vectors of length 11, or NULL when the central fit is not
#'     usable.
#' @keywords internal
mixture_null_starts <- function(t, n_starts) {
    central <- fit_null_central(t)
    centre <- central[["mean"]]
    spread <- central[["sd"]]
    if (!is.finite(centre) || !is.finite(spread) || spread <= 0) {
        spread <- stats::sd(t)
        centre <- stats::median(t)
    }
    if (!is.finite(spread) || spread <= 0) {
        return(NULL)
    }
    n_down <- sum(t < centre - 1.5 * spread)
    n_up <- sum(t > centre + 1.5 * spread)
    b <- stats::qlogis((n_down + 1) / (n_down + n_up + 2))
    alpha_skew <- mixture_null_alpha_from_skewness(
        t[abs(t - centre) <= 2.5 * spread])
    alphas <- c(alpha_skew, 0, alpha_skew, 0, -alpha_skew, 2 * alpha_skew)
    pi_0 <- c(0.95, 0.9, 0.8, 0.7, 0.6, 0.99, 0.85, 0.75, 0.65, 0.55)
    d <- c(0, -0.5, 0.5, 0, -0.5, 0.5)
    k <- c(0, 0.5, -0.5, 0.5, 0, -0.5)
    bounds <- mixture_null_bounds()
    lapply(seq_len(n_starts), function(i) {
        j <- (i - 1L) %% length(pi_0) + 1L
        h <- (i - 1L) %% length(d) + 1L
        alpha <- min(max(alphas[(i - 1L) %% length(alphas) + 1L], -8), 8)
        constants <- mixture_null_skew_constants(alpha)
        omega <- spread / constants$k2
        theta <- c(mu0 = centre - omega * constants$k1, beta = 0,
            s = log(omega), gamma = 0, alpha = alpha,
            a = stats::qlogis(2 * pi_0[j] - 1), b = b,
            dD = d[h], kD = k[h], dU = d[h], kU = k[h])
        pmin(pmax(theta, bounds$lower), bounds$upper)
    })
}

#' @title Shape of the skew-normal distribution from the sample skewness
#' @description Internal function: the skewness of the skew-normal
#'     distribution is a monotone function of delta = alpha / sqrt(1 +
#'     alpha^2) that reaches at most 0.995; the sample skewness is clamped to
#'     0.95 and inverted.
#' @param v The values (the central part of the statistics).
#' @return The shape alpha (0 when v is too short or has no spread).
#' @keywords internal
mixture_null_alpha_from_skewness <- function(v) {
    if (length(v) < 3L || !is.finite(stats::sd(v)) || stats::sd(v) <= 0) {
        return(0)
    }
    centred <- v - mean(v)
    skew <- mean(centred^3) / mean(centred^2)^1.5
    skew <- max(min(skew, 0.95), -0.95)
    g <- abs(skew)^(2 / 3)
    delta <- sign(skew) * sqrt(pi / 2 * g / (g + ((4 - pi) / 2)^(2 / 3)))
    delta / sqrt(1 - delta^2)
}

#' @title Fit the mixture null by maximum likelihood
#' @description Internal function used by the cross-fitting of
#'     \code{\link{find_gene_zscore_hit}} (empirical_fit = "mixture"). It fits
#'     the three components mixture pi_0 f_0(t | x) + pi_D f_D(t | x) +
#'     pi_U f_U(t | x) of the gene statistics t, where f_0 is the skew-normal
#'     null whose location and scale depend on the covariate x, and f_D and f_U
#'     are the normal depleted and enriched genes (see
#'     \code{\link{mixture_null_unpack}}). The log-likelihood is maximised with
#'     L-BFGS-B, the analytic gradient and the box bounds of
#'     \code{\link{mixture_null_bounds}}, from n_starts deterministic starting
#'     values; the best converged fit is kept. When no start converges, the fit
#'     falls back to central matching (\code{\link{fit_null_central}}, a normal
#'     null with alpha = 0) with a warning.
#' @param t The gene statistics.
#' @param x The covariate of the genes (the centred log of the number of
#'     barcodes).
#' @param n_starts The number of starting values (default 5).
#' @param control A list with maxit (default 500) and factr (default 1e7), as
#'     in \code{\link[stats]{optim}}.
#' @return A list of class "screenr_mixture_fit" with theta (named parameters),
#'     weights (null, down, up), loglik, convergence (0 when ok), message, n,
#'     starts_converged, at_bound (the parameters on a bound) and fallback.
#' @importFrom stats optim plogis qlogis setNames sd
#' @keywords internal
mixture_null_fit <- function(t, x, n_starts = 5L, control = list()) {
    data <- mixture_null_check_data(t, x)
    t <- data$t
    x <- data$x
    n_starts <- as.integer(n_starts)
    if (length(n_starts) != 1L || is.na(n_starts) || n_starts < 1L) {
        stop("n_starts must be a positive integer.")
    }
    bounds <- mixture_null_bounds()
    n <- length(t)
    settings <- list(maxit = 500L, factr = 1e7)
    settings[names(control)] <- control

    # The objective is the negative mean log-likelihood; non finite values
    # (the spread of the null going to 0 or Inf) are penalised
    objective <- function(theta) {
        value <- -mixture_null_loglik(theta, t, x) / n
        if (is.finite(value)) value else 1e10
    }
    gradient <- function(theta) {
        grad <- -mixture_null_gradient(theta, t, x) / n
        grad[!is.finite(grad)] <- 0
        unname(grad)
    }

    starts <- if (n >= 10L) mixture_null_starts(t, n_starts) else NULL
    runs <- lapply(starts, function(start) {
        tryCatch(
            stats::optim(start, objective, gradient, method = "L-BFGS-B",
                lower = bounds$lower, upper = bounds$upper,
                control = list(maxit = settings$maxit,
                    factr = settings$factr)),
            error = function(e) NULL)
    })
    converged <- vapply(runs, function(run) {
        !is.null(run) && run$convergence == 0L && is.finite(run$value) &&
            run$value < 1e10
    }, logical(1))
    messages <- vapply(runs, function(run) {
        if (is.null(run)) "optimiser error" else as.character(run$message)
    }, character(1))

    if (any(converged)) {
        values <- vapply(runs[converged], function(run) run$value, numeric(1))
        best <- runs[converged][[which.min(values)]]
        theta <- stats::setNames(best$par, bounds$names)
        tol <- 1e-6
        at_lower <- is.finite(bounds$lower) &
            theta - bounds$lower <= tol * pmax(1, abs(bounds$lower))
        at_upper <- is.finite(bounds$upper) &
            bounds$upper - theta <= tol * pmax(1, abs(bounds$upper))
        at_bound <- bounds$names[at_lower | at_upper]
        fit <- list(theta = theta, convergence = 0L,
            message = as.character(best$message), at_bound = at_bound,
            fallback = FALSE)
    } else {
        reason <- if (n < 10L) {
            "fewer than 10 genes"
        } else if (length(messages) > 0L) {
            paste(unique(messages), collapse = "; ")
        } else {
            "the statistics have no spread"
        }
        warning("The mixture null did not converge (", reason, "): using ",
            "the central matching fit instead.")
        central <- if (stats::sd(t) > 0) {
            fit_null_central(t)
        } else {
            c(mean = t[1], sd = 1)
        }
        theta <- c(mu0 = central[["mean"]], beta = 0,
            s = log(central[["sd"]]), gamma = 0, alpha = 0, a = 15, b = 0,
            dD = 0, kD = 0, dU = 0, kU = 0)
        fit <- list(theta = theta, convergence = 1L,
            message = paste("fallback to central matching:", reason),
            at_bound = character(0), fallback = TRUE)
    }
    fit$weights <- mixture_null_weights(fit$theta[["a"]], fit$theta[["b"]])
    fit$loglik <- mixture_null_loglik(fit$theta, t, x)
    fit$n <- n
    fit$starts_converged <- sum(converged)
    fit <- fit[c("theta", "weights", "loglik", "convergence", "message", "n",
        "starts_converged", "at_bound", "fallback")]
    class(fit) <- "screenr_mixture_fit"
    fit
}

#' @title Gauss-Legendre nodes and weights
#' @description Internal function: the nodes and the weights of the
#'     Gauss-Legendre rule on [-1, 1] (Golub-Welsch algorithm), computed once
#'     for each number of nodes.
#' @param n The number of nodes.
#' @return A list with the nodes x and the weights w.
#' @keywords internal
gauss_legendre_rule <- function(n = 100L) {
    key <- as.character(n)
    if (!is.null(.gauss_legendre_cache[[key]])) {
        return(.gauss_legendre_cache[[key]])
    }
    i <- seq_len(n - 1L)
    off <- i / sqrt(4 * i^2 - 1)
    jacobi <- matrix(0, n, n)
    jacobi[cbind(i, i + 1L)] <- off
    jacobi[cbind(i + 1L, i)] <- off
    e <- eigen(jacobi, symmetric = TRUE)
    ord <- order(e$values)
    rule <- list(x = e$values[ord], w = 2 * e$vectors[1L, ord]^2)
    .gauss_legendre_cache[[key]] <- rule
    rule
}
.gauss_legendre_cache <- new.env(parent = emptyenv())

#' @title Log of an integral with the Gauss-Legendre rule
#' @description Internal function: the log of the integral of exp(log_f(y))
#'     over [lo, lo + width] for each element of lo, computed relative to the
#'     value ref (the log of the maximum of the integrand) so that it does not
#'     under- or overflow.
#' @param log_f A function of a matrix of points, returning the log of the
#'     integrand (the rows are the elements of lo).
#' @param lo,width The start and the length of the intervals.
#' @param ref The log of the scale of the integrand, for each interval.
#' @return A vector with the log of the integrals.
#' @keywords internal
log_gauss_legendre <- function(log_f, lo, width, ref) {
    rule <- gauss_legendre_rule()
    y <- lo + outer(width / 2, 1 + rule$x)
    values <- exp(log_f(y) - ref)
    ref + log(width / 2) + log(as.vector(values %*% rule$w))
}

#' @title Log of Owen's T function
#' @description Internal function: log T(h, a) with
#'     T(h, a) = 1 / (2 pi) int_0^a exp(-h^2 (1 + y^2) / 2) / (1 + y^2) dy,
#'     for h >= 0 and a > 0, by Gauss-Legendre quadrature. The integrand
#'     decreases from y = 0, so the integral is truncated where it has fallen
#'     by exp(-50) relative to its start (y = 10 / h) and the log is computed
#'     relative to its maximum: it is accurate in the far tails.
#' @param h The (non negative) abscissae.
#' @param a The (positive) shape, a single value.
#' @return A vector with log T(h, a).
#' @keywords internal
owens_log_t <- function(h, a) {
    width <- pmin(a, 10 / pmax(h, 1e-100))
    log_f <- function(y) -h^2 * (1 + y^2) / 2 - log1p(y^2)
    ref <- -h^2 / 2
    log_gauss_legendre(log_f, rep(0, length(h)), width, ref) - log(2 * pi)
}

#' @title Log of the upper integral of Owen's T function
#' @description Internal function: log of
#'     1 / (2 pi) int_a^Inf exp(-h^2 (1 + y^2) / 2) / (1 + y^2) dy, which is
#'     T(Inf, ...) - T(h, a) = Phi(-h) / 2 - T(h, a) for h >= 0, computed
#'     directly so that there is no cancellation. The part above y = 1 uses
#'     y = 1 / u; both parts are truncated where the integrand has fallen by
#'     exp(-50) relative to its maximum.
#' @param h The (non negative) abscissae.
#' @param a The (positive) lower limit, a single value.
#' @return A vector with the log of the integral.
#' @keywords internal
owens_log_t_tail <- function(h, a) {
    n <- length(h)
    eps <- 100 / pmax(h, 1e-100)^2
    # Part above b = max(a, 1), in u = 1 / y: the integrand increases with u
    b <- max(a, 1)
    root_b <- sqrt(b^2 + eps)
    width_u <- eps / ((root_b + b) * b * root_b)
    log_f_u <- function(u) -h^2 * (1 + u^(-2)) / 2 - log1p(u^2)
    ref_u <- -h^2 * (1 + b^2) / 2 - log1p(1 / b^2)
    upper <- log_gauss_legendre(log_f_u, 1 / b - width_u, width_u, ref_u)
    if (a >= 1) {
        return(upper - log(2 * pi))
    }
    # Part between a and 1, in y: the integrand decreases with y
    root_a <- sqrt(a^2 + eps)
    width_y <- pmin(1 - a, eps / (root_a + a))
    log_f_y <- function(y) -h^2 * (1 + y^2) / 2 - log1p(y^2)
    ref_y <- -h^2 * (1 + a^2) / 2 - log1p(a^2)
    lower <- log_gauss_legendre(log_f_y, rep(a, n), width_y, ref_y)
    top <- pmax(lower, upper)
    top + log(exp(lower - top) + exp(upper - top)) - log(2 * pi)
}

#' @title Owen's T function
#' @description Internal function: Owen's T function
#'     T(h, a) = 1 / (2 pi) int_0^a exp(-h^2 (1 + y^2) / 2) / (1 + y^2) dy,
#'     even in h and odd in a. It is evaluated in-package (no dependency) by
#'     \code{\link{owens_log_t}}, accurate for |a| <= 10 and for large |h|.
#' @param h The abscissae.
#' @param a The shape, a single value.
#' @return A vector with T(h, a).
#' @keywords internal
owens_t <- function(h, a) {
    if (a == 0) {
        return(rep(0, length(h)))
    }
    sign(a) * exp(owens_log_t(abs(h), abs(a)))
}

#' @title Cumulative distribution function of the skew-normal distribution
#' @description Internal function: the log of the cumulative distribution
#'     function of the standard skew-normal distribution with shape alpha,
#'     F(z) = Phi(z) - 2 T(z, alpha) with Owen's T function. The upper tail is
#'     obtained by symmetry, P(Z > z; alpha) = F(-z; -alpha), and each case
#'     is computed as a sum of positive terms (or as 2 T of the upper integral
#'     in the lower tail of a positive alpha), so that there is no 1 - F
#'     cancellation and the far tails are accurate.
#' @param z The standardized values (t - xi) / omega.
#' @param alpha The shape, a single value.
#' @param lower.tail If TRUE (default) P(Z <= z), otherwise P(Z > z).
#' @return A vector with the log of the probabilities.
#' @keywords internal
skew_normal_log_cdf <- function(z, alpha, lower.tail = TRUE) {
    if (!lower.tail) {
        z <- -z
        alpha <- -alpha
    }
    if (alpha == 0) {
        return(stats::pnorm(z, log.p = TRUE))
    }
    h <- abs(z)
    out <- numeric(length(z))
    if (alpha < 0) {
        # F = Phi(z) + 2 T(|z|, |alpha|), a sum of positive terms
        second <- log(2) + owens_log_t(h, -alpha)
        first <- stats::pnorm(z, log.p = TRUE)
        top <- pmax(first, second)
        return(top + log(exp(first - top) + exp(second - top)))
    }
    left <- z <= 0
    if (any(left)) {
        # F = 2 (T(Inf) - T(h, alpha)) = 2 upper integral
        out[left] <- log(2) + owens_log_t_tail(h[left], alpha)
    }
    if (any(!left)) {
        # F = 1 - S with S = Phi(-z) + 2 T(z, alpha) <= 0.97
        first <- stats::pnorm(-z[!left], log.p = TRUE)
        second <- log(2) + owens_log_t(h[!left], alpha)
        top <- pmax(first, second)
        log_s <- top + log(exp(first - top) + exp(second - top))
        out[!left] <- log1p(-exp(log_s))
    }
    out
}

#' @title Neutral cumulative distribution function of the mixture null
#' @description Internal function: the cumulative distribution function of the
#'     skew-normal null component, F_0(t | x) = Phi(z) - 2 T(z, alpha) with
#'     z = (t - xi(x)) / omega(x) and T Owen's T function
#'     (\code{\link{skew_normal_log_cdf}}), used for the p-values of the
#'     depleted (lower tail) and enriched (upper tail) genes. The upper tail is
#'     computed directly and not as 1 - F_0, and log.p gives the logarithm of
#'     the p-value, so the far tails are accurate.
#' @param fit A fit returned by \code{\link{mixture_null_fit}}.
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @param lower.tail If TRUE (default) P(T <= t), otherwise P(T > t).
#' @param log.p If TRUE the logarithm of the probability is returned.
#' @return A vector with one value for each gene.
#' @keywords internal
mixture_null_cdf <- function(fit, t, x, lower.tail = TRUE, log.p = FALSE) {
    data <- mixture_null_check_data(t, x)
    p <- mixture_null_unpack(fit$theta)
    z <- (data$t - (p$mu0 + p$beta * data$x)) / exp(p$s + p$gamma * data$x)
    log_p <- skew_normal_log_cdf(z, p$alpha, lower.tail = lower.tail)
    if (log.p) log_p else exp(log_p)
}

#' @title Posterior probability of the components of the mixture null
#' @description Internal function: the probability that each gene belongs to
#'     the null, the depleted or the enriched component, given its statistic
#'     and covariate.
#' @param fit A fit returned by \code{\link{mixture_null_fit}}.
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @return A matrix with one row for each gene and the columns null, down and
#'     up, whose rows sum to 1.
#' @keywords internal
mixture_null_posterior <- function(fit, t, x) {
    data <- mixture_null_check_data(t, x)
    parts <- mixture_null_components(fit$theta, data$t, data$x)
    exp(parts$log_f - mixture_null_row_logsumexp(parts$log_f))
}

#' @title Weighted density of the components of the mixture null
#' @description Internal function: the density of the components of the fitted
#'     mixture at the statistics t, weighted by the weights of the mixture
#'     (pi_0 f_0, pi_D f_D, pi_U f_U), so that the three curves add up to the
#'     density of the mixture. Used by the plots, which therefore never
#'     re-implement the densities.
#' @param fit A fit returned by \code{\link{mixture_null_fit}}.
#' @param t The gene statistics (or a grid).
#' @param x The covariate of the genes.
#' @param component "null", "down", "up" or "all" (the sum of the three).
#' @return A vector with one value for each element of t.
#' @keywords internal
mixture_null_density <- function(fit, t, x,
    component = c("null", "down", "up", "all")) {
    component <- match.arg(component)
    data <- mixture_null_check_data(t, x)
    parts <- mixture_null_components(fit$theta, data$t, data$x)
    if (component == "all") {
        exp(mixture_null_row_logsumexp(parts$log_f))
    } else {
        unname(exp(parts$log_f[, component]))
    }
}
