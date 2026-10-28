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
#'     It is the same density as \code{sn::dsn(log = TRUE)} (checked in the
#'     tests); it is written out here because the gradient needs log Phi and
#'     because \code{pnorm(log.p = TRUE)} stays finite when alpha z is very
#'     negative.
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
    log_f <- cbind(
        null = log(2) - log(omega) - half_log_2pi - 0.5 * z^2 + log_big_phi +
            log_weight[["null"]],
        down = -0.5 * w_d^2 - log(sd_d) - half_log_2pi + log_weight[["down"]],
        up = -0.5 * w_u^2 - log(sd_u) - half_log_2pi + log_weight[["up"]]
    )
    list(
        log_f = log_f,
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
    top <- m[, 1L]
    for (j in seq_len(ncol(m))[-1L]) {
        top <- pmax(top, m[, j])
    }
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
#' @param parts The result of \code{\link{mixture_null_components}} at theta,
#'     when it has already been computed (the optimiser evaluates the
#'     log-likelihood and the gradient at the same point).
#' @return A named vector of length 11.
#' @keywords internal
mixture_null_gradient <- function(theta, t, x, parts = NULL) {
    data <- mixture_null_check_data(t, x)
    t <- data$t
    x <- data$x
    if (is.null(parts)) {
        parts <- mixture_null_components(theta, t, x)
    }
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

#' @title Setup of the starting values of the mixture null
#' @description Internal function: the quantities of the central matching fit
#'     of the statistics that the starting values share (the centre and the
#'     spread of the null, the log odds of the depleted genes and the shape
#'     suggested by the skewness of the central part), computed once for all
#'     the starts of a fit.
#' @param t The gene statistics.
#' @return A list with centre, spread, b and alpha_skew, or NULL when the
#'     central fit is not usable.
#' @keywords internal
mixture_null_start_setup <- function(t) {
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
    list(centre = centre, spread = spread,
        b = stats::qlogis((n_down + 1) / (n_down + n_up + 2)),
        alpha_skew = mixture_null_alpha_from_skewness(
            t[abs(t - centre) <= 2.5 * spread]))
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
#'     null). When alpha is given, all the starts have that shape (the starts
#'     of the profile fit, where alpha is fixed).
#' @param t The gene statistics.
#' @param n_starts The number of starting values.
#' @param alpha The shape of all the starts, or NULL for the shapes above.
#' @param setup The result of \code{\link{mixture_null_start_setup}} (computed
#'     from t when missing).
#' @return A list of vectors of length 11, or NULL when the central fit is not
#'     usable.
#' @keywords internal
mixture_null_starts <- function(t, n_starts, alpha = NULL,
    setup = mixture_null_start_setup(t)) {
    if (is.null(setup)) {
        return(NULL)
    }
    alphas <- if (is.null(alpha)) {
        a <- setup$alpha_skew
        pmin(pmax(c(a, 0, a, 0, -a, 2 * a), -8), 8)
    } else {
        alpha
    }
    pi_0 <- c(0.95, 0.9, 0.8, 0.7, 0.6, 0.99, 0.85, 0.75, 0.65, 0.55)
    d <- c(0, -0.5, 0.5, 0, -0.5, 0.5)
    k <- c(0, 0.5, -0.5, 0.5, 0, -0.5)
    bounds <- mixture_null_bounds()
    lapply(seq_len(n_starts), function(i) {
        j <- (i - 1L) %% length(pi_0) + 1L
        h <- (i - 1L) %% length(d) + 1L
        alpha_i <- alphas[(i - 1L) %% length(alphas) + 1L]
        constants <- mixture_null_skew_constants(alpha_i)
        omega <- setup$spread / constants$k2
        theta <- c(mu0 = setup$centre - omega * constants$k1, beta = 0,
            s = log(omega), gamma = 0, alpha = alpha_i,
            a = stats::qlogis(2 * pi_0[j] - 1), b = setup$b,
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

#' @title Maximise the mixture null with the shape alpha fixed
#' @description Internal function: the other ten parameters of the mixture
#'     null are optimised by L-BFGS-B with the analytic gradient, from each of
#'     the given starting values, while the shape alpha is kept at a fixed
#'     value. The run with the best converged log-likelihood is kept.
#' @param alpha The fixed shape.
#' @param starts A list of starting vectors of length 11 (their alpha is
#'     replaced by the fixed one).
#' @param t,x The gene statistics and the covariate.
#' @param settings A list with maxit and factr, as in \code{\link[stats]{optim}}.
#' @return A list with theta (alpha included), loglik, n_converged and message,
#'     or NULL when no start converged.
#' @keywords internal
mixture_null_fit_alpha <- function(alpha, starts, t, x, settings) {
    bounds <- mixture_null_bounds()
    free <- bounds$names != "alpha"
    n <- length(t)
    expand <- function(par) {
        theta <- numeric(11)
        theta[free] <- par
        theta[!free] <- alpha
        stats::setNames(theta, bounds$names)
    }
    # The objective is the negative mean log-likelihood; non finite values
    # (the spread of the null going to 0 or Inf) are penalised. The optimiser
    # asks for the value and the gradient at the same point: the components
    # are computed once for both.
    memo <- new.env(parent = emptyenv())
    memo$par <- NULL
    components_at <- function(par) {
        if (is.null(memo$par) || !identical(memo$par, par)) {
            memo$par <- par
            memo$theta <- expand(par)
            memo$parts <- mixture_null_components(memo$theta, t, x)
        }
        memo$parts
    }
    objective <- function(par) {
        parts <- components_at(par)
        value <- -sum(mixture_null_row_logsumexp(parts$log_f)) / n
        if (is.finite(value)) value else 1e10
    }
    gradient <- function(par) {
        parts <- components_at(par)
        grad <- -mixture_null_gradient(memo$theta, t, x, parts)[free] / n
        grad[!is.finite(grad)] <- 0
        unname(grad)
    }
    runs <- lapply(starts, function(start) {
        par <- pmin(pmax(start[free], bounds$lower[free]), bounds$upper[free])
        tryCatch(
            stats::optim(unname(par), objective, gradient, method = "L-BFGS-B",
                lower = unname(bounds$lower[free]),
                upper = unname(bounds$upper[free]),
                control = list(maxit = settings$maxit, factr = settings$factr)),
            error = function(e) NULL)
    })
    converged <- vapply(runs, function(run) {
        !is.null(run) && run$convergence == 0L && is.finite(run$value) &&
            run$value < 1e10
    }, logical(1))
    messages <- vapply(runs, function(run) {
        if (is.null(run)) "optimiser error" else as.character(run$message)
    }, character(1))
    if (!any(converged)) {
        return(list(theta = NULL, loglik = NA_real_, n_converged = 0L,
            message = paste(unique(messages), collapse = "; ")))
    }
    values <- vapply(runs[converged], function(run) run$value, numeric(1))
    best <- runs[converged][[which.min(values)]]
    list(theta = expand(best$par), loglik = -best$value * n,
        n_converged = sum(converged),
        message = as.character(best$message))
}

#' @title Profile log-likelihood of the shape of the mixture null
#' @description Internal function: the profile fit of the mixture null. The
#'     shape alpha of the skew-normal null is weakly identified (at gene level
#'     the log-likelihood can be almost flat over a wide range of alpha, with
#'     several shallow modes), so it is not optimised with the other
#'     parameters from random or few starts. Instead alpha is fixed on a grid
#'     (step 0.25 over -4 to 4, extended by whole steps toward -10 and 10 while
#'     the 95\% interval touches the edge of the grid) and the other ten
#'     parameters are optimised at each grid point, from the solutions of the
#'     neighbouring grid points (continuation, outward from alpha = 0 and then
#'     once more in each direction over the grid) and from the default
#'     starts of \code{\link{mixture_null_starts}} with that alpha (grid
#'     points more than 5 qchisq(0.95, 1) / 2, about 9.6, below the best
#'     log-likelihood found, which are far outside the 95\% interval, are fitted
#'     from the warm start alone and are left out of the second continuation).
#'     There is no random start: the result depends on the data only. The maximum
#'     likelihood estimate is the best grid point refined by a one dimensional
#'     optimisation of alpha between its two neighbours.
#' @param t,x The gene statistics and the covariate (the fit sorts them, see
#'     \code{\link{mixture_null_fit}}).
#' @param n_starts The number of default starts at each grid point.
#' @param settings A list with maxit and factr.
#' @return A list with the grid (data.frame alpha, loglik and the other ten
#'     parameters, only the grid points where the optimisation converged),
#'     theta, loglik and message of the maximum likelihood estimate, the number
#'     of converged runs, or NULL when no grid point converged.
#' @keywords internal
mixture_null_profile <- function(t, x, n_starts, settings) {
    step <- 0.25
    k_max <- 40L
    crit <- stats::qchisq(0.95, 1) / 2
    setup <- mixture_null_start_setup(t)
    if (is.null(setup)) {
        return(NULL)
    }
    points <- list()
    n_converged <- 0L
    key <- function(k) as.character(k)
    # Fit the grid point k from the warm starts and, optionally, the defaults
    fit_point <- function(k, warm = list(), defaults = TRUE) {
        alpha <- k * step
        starts <- c(warm, if (defaults) {
            mixture_null_starts(t, n_starts, alpha = alpha, setup = setup)
        })
        run <- mixture_null_fit_alpha(alpha, starts, t, x, settings)
        n_converged <<- n_converged + run$n_converged
        old <- points[[key(k)]]
        if (!is.null(run$theta) &&
            (is.null(old) || run$loglik > old$loglik)) {
            points[[key(k)]] <<- run
        }
        invisible(NULL)
    }
    theta_at <- function(k) points[[key(k)]]$theta
    # Continuation along the grid points ks: the warm start of each point is
    # the closest fitted one already visited in the sweep (or `last`). Once a
    # point is more than `far` below the best log-likelihood found so far, the
    # points beyond it (the profile only goes down there) are fitted from the
    # warm start alone: they are far outside the 95% interval.
    far <- 5 * crit
    best_ll <- function() max(vapply(points, function(p) p$loglik, numeric(1)))
    sweep <- function(ks, defaults, last = NULL) {
        for (k in ks) {
            hopeless <- length(points) > 0L && !is.null(last) &&
                last$loglik < best_ll() - far
            fit_point(k, if (is.null(last)) list() else list(last$theta),
                defaults && !hopeless)
            if (!is.null(points[[key(k)]])) last <- points[[key(k)]]
        }
    }
    extend <- function() {
        repeat {
            ks <- as.integer(names(points))
            ll <- vapply(points, function(p) p$loglik, numeric(1))
            cut <- max(ll) - crit
            lo <- min(ks)
            hi <- max(ks)
            grown <- FALSE
            if (hi < k_max && ll[[key(hi)]] >= cut) {
                fit_point(hi + 1L, list(theta_at(hi)))
                grown <- grown || !is.null(points[[key(hi + 1L)]])
            }
            if (lo > -k_max && ll[[key(lo)]] >= cut) {
                fit_point(lo - 1L, list(theta_at(lo)))
                grown <- grown || !is.null(points[[key(lo - 1L)]])
            }
            if (!grown) break
        }
    }
    n_base <- as.integer(4 / step)
    # Outward from alpha = 0, upward and downward
    sweep(0L, TRUE)
    sweep(seq_len(n_base), TRUE, points[[key(0L)]])
    sweep(-seq_len(n_base), TRUE, points[[key(0L)]])
    if (length(points) == 0L) {
        return(NULL)
    }
    extend()
    # One more continuation in each direction over the grid points that are
    # not far below the maximum
    ks <- sort(as.integer(names(points)))
    ll <- vapply(ks, function(k) points[[key(k)]]$loglik, numeric(1))
    ks <- ks[ll >= max(ll) - far]
    sweep(ks, FALSE)
    sweep(rev(ks), FALSE)
    extend()

    ks <- sort(as.integer(names(points)))
    alphas <- ks * step
    loglik <- vapply(ks, function(k) points[[key(k)]]$loglik, numeric(1))
    best_k <- ks[which.max(loglik)]
    best <- points[[key(best_k)]]

    # Refine the maximum between the neighbours of the best grid point (the
    # profile at an alpha is started from the three closest solutions)
    close <- Filter(Negate(is.null), lapply(best_k + (-1:1), theta_at))
    profile_at <- function(alpha) {
        run <- mixture_null_fit_alpha(alpha, close, t, x, settings)
        n_converged <<- n_converged + run$n_converged
        run
    }
    refined <- NULL
    lower <- max(-10, (best_k - 1) * step)
    upper <- min(10, (best_k + 1) * step)
    if (upper > lower) {
        opt <- stats::optimize(function(a) {
            run <- profile_at(a)
            if (is.null(run$theta)) 1e10 else -run$loglik
        }, c(lower, upper), tol = 1e-3)
        run <- profile_at(opt$minimum)
        if (!is.null(run$theta) && run$loglik > best$loglik) {
            refined <- run
        }
    }
    mle <- if (is.null(refined)) best else refined
    grid <- data.frame(alpha = alphas, loglik = loglik)
    other <- do.call(rbind, lapply(ks, function(k) points[[key(k)]]$theta))
    grid <- cbind(grid, as.data.frame(other[, colnames(other) != "alpha",
        drop = FALSE]))
    rownames(grid) <- NULL
    list(grid = grid, theta = mle$theta, loglik = mle$loglik,
        message = mle$message, n_converged = n_converged, step = step)
}

#' @title Profile interval of the shape of the mixture null
#' @description Internal function: the 95\% profile likelihood interval of
#'     alpha, the set of the alpha whose profile log-likelihood is at least the
#'     maximum minus qchisq(0.95, 1) / 2, from the grid of
#'     \code{\link{mixture_null_profile}}. The ends of the interval are the
#'     outermost grid points inside the set, extended to the crossing of the
#'     threshold by linear interpolation toward the next grid point (or the
#'     bound of alpha). If the set has several parts the interval is the hull.
#'     The maximum likelihood estimate is always inside.
#' @param grid The profile grid (alpha and loglik).
#' @param alpha_hat,loglik The maximum likelihood estimate and its
#'     log-likelihood.
#' @param level The coverage (0.95).
#' @return A named vector (lower, upper).
#' @keywords internal
mixture_null_alpha_interval <- function(grid, alpha_hat, loglik, level = 0.95) {
    cut <- loglik - stats::qchisq(level, 1) / 2
    inside <- which(grid$loglik >= cut)
    bounds <- mixture_null_bounds()
    edge <- function(i, j, limit) {
        # i: outermost inside grid point, j: its outer neighbour (may not exist)
        if (j < 1L || j > nrow(grid)) {
            return(limit)
        }
        gap <- grid$loglik[i] - grid$loglik[j]
        if (gap <= 0) {
            return(grid$alpha[j])
        }
        grid$alpha[i] + (grid$alpha[j] - grid$alpha[i]) *
            (grid$loglik[i] - cut) / gap
    }
    lo_i <- min(inside)
    hi_i <- max(inside)
    c(lower = min(alpha_hat, edge(lo_i, lo_i - 1L, bounds$lower[["alpha"]])),
        upper = max(alpha_hat, edge(hi_i, hi_i + 1L, bounds$upper[["alpha"]])))
}

#' @title Fit the mixture null by maximum likelihood
#' @description Internal function used by the cross-fitting of
#'     \code{\link{find_gene_zscore_hit}} (empirical_fit = "mixture"). It fits
#'     the three components mixture pi_0 f_0(t | x) + pi_D f_D(t | x) +
#'     pi_U f_U(t | x) of the gene statistics t, where f_0 is the skew-normal
#'     null whose location and scale depend on the covariate x, and f_D and f_U
#'     are the normal depleted and enriched genes (see
#'     \code{\link{mixture_null_unpack}}). The shape alpha of the null is weakly
#'     identified at gene level, so the fit is a deterministic profile fit
#'     (\code{\link{mixture_null_profile}}): the log-likelihood is maximised
#'     over the other parameters (L-BFGS-B, analytic gradient, box bounds of
#'     \code{\link{mixture_null_bounds}}) on a grid of alpha with continuation
#'     warm starts and n_starts default starts at each point, and the maximum
#'     is refined in one dimension. There is no random start, and the
#'     statistics are sorted internally by (t, x), so the result does not depend
#'     on the order of the genes (it is identical, not only equal, for any
#'     permutation of them). \code{theta} is the profiled maximum likelihood
#'     estimate; the profile and its 95\% interval are kept for the
#'     conservative p-values of \code{\link{mixture_null_cdf}}. When no grid
#'     point converges, or there are fewer than 10 genes, the fit falls back to
#'     central matching (\code{\link{fit_null_central}}, a normal null with
#'     alpha = 0) with a warning, and has no profile.
#' @param t The gene statistics.
#' @param x The covariate of the genes (the centred log of the number of
#'     barcodes).
#' @param n_starts The number of default starts at each grid point (default 2:
#'     more starts cost time and, on the gene-level HAP1 data, did not change
#'     the log-likelihood of the profile by more than 0.003).
#' @param control A list with maxit (default 500) and factr (default 1e7), as
#'     in \code{\link[stats]{optim}}.
#' @return A list of class "screenr_mixture_fit" with theta (named parameters,
#'     the profiled maximum likelihood estimate), weights (null, down, up),
#'     loglik, convergence (0 when ok), message, n, starts_converged (the
#'     number of converged optimisations), at_bound (the parameters on a
#'     bound), fallback, alpha_hat (the shape of theta), alpha_ci (the 95\%
#'     profile interval of alpha, c(lower, upper); NA for the fallback) and
#'     profile (data.frame with alpha, loglik and the other ten parameters
#'     profiled at each grid point; NULL for the fallback).
#' @importFrom stats optim optimize plogis qchisq qlogis setNames sd
#' @keywords internal
mixture_null_fit <- function(t, x, n_starts = 2L, control = list()) {
    data <- mixture_null_check_data(t, x)
    n_starts <- as.integer(n_starts)
    if (length(n_starts) != 1L || is.na(n_starts) || n_starts < 1L) {
        stop("n_starts must be a positive integer.")
    }
    # A canonical order of the genes: the sums of the likelihood, and so the
    # whole optimisation, do not depend on the order of the input
    ord <- order(data$t, data$x)
    t <- data$t[ord]
    x <- data$x[ord]
    bounds <- mixture_null_bounds()
    n <- length(t)
    settings <- list(maxit = 500L, factr = 1e7)
    settings[names(control)] <- control

    profile <- if (n >= 10L) {
        mixture_null_profile(t, x, n_starts, settings)
    } else {
        NULL
    }

    if (!is.null(profile)) {
        theta <- profile$theta
        tol <- 1e-6
        at_lower <- is.finite(bounds$lower) &
            theta - bounds$lower <= tol * pmax(1, abs(bounds$lower))
        at_upper <- is.finite(bounds$upper) &
            bounds$upper - theta <= tol * pmax(1, abs(bounds$upper))
        fit <- list(theta = theta, convergence = 0L,
            message = profile$message,
            at_bound = bounds$names[at_lower | at_upper], fallback = FALSE,
            alpha_hat = theta[["alpha"]],
            alpha_ci = mixture_null_alpha_interval(profile$grid,
                theta[["alpha"]], profile$loglik),
            profile = profile$grid)
        starts_converged <- profile$n_converged
    } else {
        reason <- if (n < 10L) {
            "fewer than 10 genes"
        } else if (is.null(mixture_null_start_setup(t))) {
            "the statistics have no spread"
        } else {
            "no profile optimisation converged"
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
            at_bound = character(0), fallback = TRUE, alpha_hat = 0,
            alpha_ci = c(lower = NA_real_, upper = NA_real_), profile = NULL)
        starts_converged <- 0L
    }
    fit$weights <- mixture_null_weights(fit$theta[["a"]], fit$theta[["b"]])
    fit$loglik <- mixture_null_loglik(fit$theta, t, x)
    fit$n <- n
    fit$starts_converged <- starts_converged
    fit <- fit[c("theta", "weights", "loglik", "convergence", "message", "n",
        "starts_converged", "at_bound", "fallback", "alpha_hat", "alpha_ci",
        "profile")]
    class(fit) <- "screenr_mixture_fit"
    fit
}

#' @title Tail probability of the skew-normal null of one parameter vector
#' @description Internal function: F_0(t | x) (lower tail) or 1 - F_0(t | x)
#'     (upper tail) of the skew-normal null of theta, from \code{sn::psn}; the
#'     upper tail by symmetry, never as 1 - \code{sn::psn(...)}.
#' @param theta The parameters of the mixture null.
#' @param t,x The statistics and the covariate (same length).
#' @param lower.tail TRUE for P(T <= t), FALSE for P(T > t).
#' @return A vector with one value for each gene.
#' @keywords internal
mixture_null_tail <- function(theta, t, x, lower.tail) {
    p <- mixture_null_unpack(theta)
    xi <- p$mu0 + p$beta * x
    omega <- exp(p$s + p$gamma * x)
    if (lower.tail) {
        sn::psn(t, xi, omega, p$alpha)
    } else {
        sn::psn(-t, -xi, omega, -p$alpha)
    }
}

#' @title Neutral cumulative distribution function of the mixture null
#' @description Internal function: the cumulative distribution function of the
#'     skew-normal null component, F_0(t | x), with location
#'     xi(x) = mu0 + beta x, scale omega(x) = exp(s + gamma x) and shape alpha,
#'     used for the p-values of the depleted (lower tail) and enriched (upper
#'     tail) genes. It is computed by \code{sn::psn}, the reference
#'     implementation of the skew-normal distribution of the sn package. The
#'     lower tail is \code{sn::psn(t, xi, omega, alpha)}. \code{sn::psn} has
#'     no \code{lower.tail} and no \code{log.p}, so the upper tail is obtained
#'     by symmetry, 1 - F_0(t) = \code{sn::psn(-t, -xi, omega, -alpha)} (never
#'     as 1 - \code{sn::psn(...)}, which would lose all the digits in the
#'     tail), and the logarithm of the p-value is the log of that value.
#'     Limits of \code{sn::psn}: a probability that underflows to 0 gives a
#'     log.p of -Inf (probabilities below about 1e-308, for instance a normal
#'     null more than 37 sd from the centre); and in the tail that is light
#'     (the upper tail when alpha < 0, the lower tail when alpha > 0) the
#'     value is computed as a difference, so it keeps about 6 significant digits
#'     only down to about 1e-13 for |alpha| = 1, 1e-28 for |alpha| = 0.5, 1e-49
#'     for |alpha| = 3 and 1e-79 for |alpha| = 5, and it can be exactly 0
#'     (-Inf with log.p) well before 1e-308 for the smaller shapes (below about
#'     1e-24 for |alpha| = 1). The p-values used to call hits (above 1e-10) are
#'     not affected.
#'
#'     With \code{conservative = TRUE} (default) the shape alpha is not taken
#'     at its maximum likelihood estimate only. At gene level the profile
#'     likelihood of alpha is almost flat, and the tail p-values depend on it,
#'     so the p-value of a gene is the maximum, over the grid values of alpha
#'     inside the 95\% profile interval \code{fit$alpha_ci} and the maximum
#'     likelihood estimate, of the tail probability in the requested direction,
#'     each computed with the other parameters profiled at that alpha
#'     (\code{fit$profile}). It is therefore never smaller than the
#'     \code{conservative = FALSE} value, which uses \code{fit$theta} (the
#'     profiled maximum likelihood estimate) only. A fit without a profile
#'     (the fallback, or a hand made fit) has the same value in both cases.
#' @param fit A fit returned by \code{\link{mixture_null_fit}}.
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @param lower.tail If TRUE (default) P(T <= t), otherwise P(T > t).
#' @param log.p If TRUE the logarithm of the probability is returned (-Inf
#'     when the probability underflows to 0).
#' @param conservative If TRUE (default) the maximum over the shapes inside the
#'     95\% profile interval, otherwise the maximum likelihood estimate only.
#' @return A vector with one value for each gene.
#' @keywords internal
mixture_null_cdf <- function(fit, t, x, lower.tail = TRUE, log.p = FALSE,
    conservative = TRUE) {
    data <- mixture_null_check_data(t, x)
    value <- mixture_null_tail(fit$theta, data$t, data$x, lower.tail)
    if (conservative && !is.null(fit$profile) && nrow(fit$profile) > 0L) {
        cut <- fit$loglik - stats::qchisq(0.95, 1) / 2
        inside <- fit$profile[fit$profile$loglik >= cut, , drop = FALSE]
        names_theta <- mixture_null_bounds()$names
        for (i in seq_len(nrow(inside))) {
            theta <- stats::setNames(as.numeric(inside[i, names_theta]),
                names_theta)
            value <- pmax(value,
                mixture_null_tail(theta, data$t, data$x, lower.tail))
        }
    }
    if (log.p) log(value) else value
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
#'     re-implement the densities. The null component is evaluated by
#'     \code{sn::dsn}, the reference implementation of the skew-normal density
#'     (the closed form inside the likelihood and the gradient is checked
#'     against it in the tests). The plots draw the null at the maximum
#'     likelihood estimate of the shape (\code{fit$alpha_hat}); the p-values
#'     use the conservative envelope of \code{\link{mixture_null_cdf}}.
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
    p <- mixture_null_unpack(fit$theta)
    null <- function() {
        p$weights[["null"]] * sn::dsn(data$t, p$mu0 + p$beta * data$x,
            exp(p$s + p$gamma * data$x), p$alpha)
    }
    alternative <- function(name) {
        parts <- mixture_null_components(fit$theta, data$t, data$x)
        exp(parts$log_f[, name])
    }
    value <- switch(component,
        null = null(),
        down = alternative("down"),
        up = alternative("up"),
        all = null() + alternative("down") + alternative("up"))
    unname(value)
}
