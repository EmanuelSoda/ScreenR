#' @title Names and box bounds of the parameters of the mixture null
#' @description Internal function: the parameters of the mixture null, in the
#'     order used by \code{\link{mixture_null_fit}}, and the box bounds used by
#'     the optimiser. mu0 and s (the centre and the log spread of the null)
#'     are not bounded.
#' @return A list with the parameter names, the lower and the upper bounds.
#' @keywords internal
mixture_null_bounds <- function() {
    names <- c("mu0", "beta", "s", "gamma", "a", "b", "dD", "kD", "dU", "kU")
    list(
        names = names,
        lower = stats::setNames(
            c(-Inf, -5, -Inf, -2, -15, -15, -6, -6, -6, -6), names),
        upper = stats::setNames(
            c(Inf, 5, Inf, 2, 15, 15, 4, 3, 4, 3), names)
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
#'     theta into the centre, the spread, the shifts and the widths of the
#'     components and the weights of the mixture. The null is
#'     N(mu0 + beta x, exp(s + gamma x)^2); the depleted and the enriched
#'     components are centred at least three null sd below and above the null
#'     (at m0(x) -/+ s0(x) (3 + exp(d))) and are wider than the null. The
#'     minimum distance of three null sd makes the null identifiable: with a
#'     smaller one, a pure null sample is split between the components and the
#'     null spread is underestimated. Effects smaller than about three null sd
#'     are therefore absorbed by the null (wider spread, conservative
#'     p-values) and never the reverse. The weights are pi_0 = 0.5 + 0.5 plogis(a),
#'     pi_D = (1 - pi_0) plogis(b) and pi_U = 1 - pi_0 - pi_D.
#' @param theta The vector c(mu0, beta, s, gamma, a, b, dD, kD, dU, kU).
#' @return A list with the named parameters and the weights.
#' @keywords internal
mixture_null_unpack <- function(theta) {
    if (!is.numeric(theta) || length(theta) != 10L || any(is.na(theta))) {
        stop("theta must be a numeric vector of length 10 without NA.")
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

#' @title Log density of the components of the mixture null
#' @description Internal function: the log of the weight times the density of
#'     each component, for each gene, with the intermediate values needed by
#'     the gradient.
#' @param theta The parameters of the mixture null.
#' @param t The gene statistics.
#' @param x The covariate of the genes.
#' @return A list with the matrix of the log weighted densities (columns null,
#'     down, up) and the standardized values, centres and spreads.
#' @keywords internal
mixture_null_components <- function(theta, t, x) {
    p <- mixture_null_unpack(theta)
    m0 <- p$mu0 + p$beta * x
    s0 <- exp(p$s + p$gamma * x)
    c_d <- 3 + exp(p$dD)
    v_d <- 1 + exp(p$kD)
    c_u <- 3 + exp(p$dU)
    v_u <- 1 + exp(p$kU)
    mean <- cbind(null = m0, down = m0 - s0 * c_d, up = m0 + s0 * c_u)
    sd <- cbind(null = s0, down = s0 * v_d, up = s0 * v_u)
    z <- (t - mean) / sd
    log_weight <- c(
        null = log(0.5 + 0.5 * stats::plogis(p$a)),
        down = log(0.5) + stats::plogis(-p$a, log.p = TRUE) +
            stats::plogis(p$b, log.p = TRUE),
        up = log(0.5) + stats::plogis(-p$a, log.p = TRUE) +
            stats::plogis(-p$b, log.p = TRUE)
    )
    log_density <- -0.5 * z^2 - log(sd) - 0.5 * log(2 * pi)
    list(
        log_f = sweep(log_density, 2, log_weight, "+"),
        z = z, sd = sd, s0 = s0, c_d = c_d, v_d = v_d, c_u = c_u, v_u = v_u,
        params = p
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
#' @param theta The vector c(mu0, beta, s, gamma, a, b, dD, kD, dU, kU).
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
#'     \code{\link{mixture_null_loglik}} with respect to theta.
#' @inheritParams mixture_null_loglik
#' @return A named vector of length 10.
#' @keywords internal
mixture_null_gradient <- function(theta, t, x) {
    data <- mixture_null_check_data(t, x)
    t <- data$t
    x <- data$x
    parts <- mixture_null_components(theta, t, x)
    p <- parts$params
    total <- mixture_null_row_logsumexp(parts$log_f)
    # Responsibilities of the components
    r <- exp(parts$log_f - total)
    # Derivative of the log density of each component with respect to its
    # centre (gm) and to the log of its spread (gs)
    gm <- parts$z / parts$sd
    gs <- parts$z^2 - 1
    rc <- r * gm
    rs <- r * gs
    # Derivative of the centres of the depleted and enriched components with
    # respect to s (they are proportional to the spread of the null)
    shift_d <- -parts$s0 * parts$c_d
    shift_u <- parts$s0 * parts$c_u
    d_s <- rs[, "null"] + rs[, "down"] + rs[, "up"] +
        rc[, "down"] * shift_d + rc[, "up"] * shift_u
    d_mu <- rowSums(rc)
    pi_0 <- 0.5 + 0.5 * stats::plogis(p$a)
    d_pi_0 <- 0.5 * stats::plogis(p$a) * stats::plogis(-p$a)
    # 1 - pi_0 = 0.5 plogis(-a): avoids the cancellation for large a
    grad_a <- sum(r[, "null"] * d_pi_0 / pi_0 -
        (r[, "down"] + r[, "up"]) * d_pi_0 / (0.5 * stats::plogis(-p$a)))
    grad_b <- sum(r[, "down"] * stats::plogis(-p$b) -
        r[, "up"] * stats::plogis(p$b))
    grad <- c(
        mu0 = sum(d_mu),
        beta = sum(d_mu * x),
        s = sum(d_s),
        gamma = sum(d_s * x),
        a = grad_a,
        b = grad_b,
        dD = sum(rc[, "down"] * -parts$s0 * exp(p$dD)),
        kD = sum(rs[, "down"]) * exp(p$kD) / parts$v_d,
        dU = sum(rc[, "up"] * parts$s0 * exp(p$dU)),
        kU = sum(rs[, "up"]) * exp(p$kU) / parts$v_u
    )
    grad
}

#' @title Starting values of the mixture null
#' @description Internal function: deterministic starting values for the
#'     optimiser, derived from the central matching fit of the statistics.
#'     The start k has beta = gamma = 0, pi_0 in 0.95, 0.9, 0.8, ... and
#'     different shifts and widths of the alternatives (the alternatives are
#'     always at least three null sd from the centre of the null).
#' @param t The gene statistics.
#' @param n_starts The number of starting values.
#' @return A list of vectors of length 10, or NULL when the central fit is not
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
    pi_0 <- c(0.95, 0.9, 0.8, 0.7, 0.6, 0.99, 0.85, 0.75, 0.65, 0.55)
    d <- c(0, -0.5, 0.5, 0, -0.5, 0.5)
    k <- c(0, 0.5, -0.5, 0.5, 0, -0.5)
    bounds <- mixture_null_bounds()
    lapply(seq_len(n_starts), function(i) {
        j <- (i - 1L) %% length(pi_0) + 1L
        h <- (i - 1L) %% length(d) + 1L
        theta <- c(mu0 = centre, beta = 0, s = log(spread), gamma = 0,
            a = stats::qlogis(2 * pi_0[j] - 1), b = b,
            dD = d[h], kD = k[h], dU = d[h], kU = k[h])
        pmin(pmax(theta, bounds$lower), bounds$upper)
    })
}

#' @title Fit the mixture null by maximum likelihood
#' @description Internal function used by the cross-fitting of
#'     \code{\link{find_gene_zscore_hit}} (empirical_fit = "mixture"). It fits
#'     the three components mixture pi_0 f_0(t | x) + pi_D f_D(t | x) +
#'     pi_U f_U(t | x) of the gene statistics t, where f_0 is the normal null
#'     whose centre and spread depend on the covariate x, and f_D and f_U are
#'     the depleted and enriched genes (see \code{\link{mixture_null_unpack}}).
#'     The log-likelihood is maximised with L-BFGS-B, the analytic gradient
#'     and the box bounds of \code{\link{mixture_null_bounds}}, from n_starts
#'     deterministic starting values; the best converged fit is kept. When no
#'     start converges, the fit falls back to central matching
#'     (\code{\link{fit_null_central}}) with a warning.
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
            s = log(central[["sd"]]), gamma = 0, a = 15, b = 0,
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

#' @title Neutral cumulative distribution function of the mixture null
#' @description Internal function: the cumulative distribution function of the
#'     null component, F_0(t | x) = Phi((t - m0(x)) / s0(x)), used for the
#'     p-values of the depleted (lower tail) and enriched (upper tail) genes.
#'     The upper tail is computed directly and not as 1 - F_0, and log.p gives
#'     the logarithm of the p-value, so the far tails are accurate.
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
    centre <- p$mu0 + p$beta * data$x
    spread <- exp(p$s + p$gamma * data$x)
    stats::pnorm((data$t - centre) / spread, lower.tail = lower.tail,
        log.p = log.p)
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
