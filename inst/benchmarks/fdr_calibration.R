# FDR calibration release gate for the optional mixture empirical null.
# Run from an installed package:
# Rscript inst/benchmarks/fdr_calibration.R --output=calibration
# Smoke run (does not certify the release gate): --replicates=2
# Full mode is exactly 500 replicates in each of the 48 scenarios.
# Constants: barcode SD 1, baseline gene-effect tau 0.4; shift = half the
# baseline marginal gene-mean null SD; inflation = 1.3 times that SD.
# Effects are either fixed at 4 baseline null SDs or sampled independently
# per hit from U(1.5, 5) baseline null SDs, with the sign set by direction.
# KS cutoff: qbinom(0.95, 500, 0.01) / 500 (one-sided 95% upper quantile).
# No new runtime dependencies are required.

simulate_calibration_screen <- function(n_genes, pi0, null, seed,
    effect_size = "fixed4") {
    set.seed(seed)
    n_barcodes <- sample(3:10, n_genes, replace = TRUE)
    # Barcode noise has SD 1. At the gene-mean level the baseline null SD
    # includes both a shared gene effect (tau = 0.4) and barcode noise.
    # Inflation multiplies this marginal gene-mean SD by exactly 1.3;
    # a shifted centre moves it by half this baseline null SD.
    barcode_variance <- mean(1 / (3:10))
    null_sd <- sqrt(0.4^2 + barcode_variance)
    tau <- if (null == "inflated") {
        sqrt((1.3 * null_sd)^2 - barcode_variance)
    } else 0.4
    shift <- if (null == "shifted") 0.5 * null_sd else 0
    effect <- rep(4 * null_sd, n_genes)
    n_hits <- round(n_genes * (1 - pi0))
    truth <- rep("null", n_genes)
    if (n_hits > 0) {
        hit <- sample.int(n_genes, n_hits)
        truth[hit] <- rep(c("Down", "Up"), length.out = n_hits)
    }
    if (effect_size == "uniform") {
        effect[truth != "null"] <- stats::runif(n_hits, 1.5, 5) * null_sd
    }
    u <- stats::rnorm(n_genes, mean = shift, sd = tau)
    u[truth == "Down"] <- -effect[truth == "Down"]
    u[truth == "Up"] <- effect[truth == "Up"]
    genes <- paste0("G", seq_len(n_genes))
    score <- rep(u, n_barcodes) + stats::rnorm(sum(n_barcodes))
    list(table = data.frame(Gene = rep(genes, n_barcodes),
        Zscore = score, Log2FC = score),
        truth = stats::setNames(truth, genes),
        parameters = data.frame(barcode_sd = 1, tau = tau, shift = shift,
            baseline_null_sd = null_sd,
            null_sd_multiplier = if (null == "inflated") 1.3 else 1,
            effect_min_sd = if (effect_size == "fixed4") 4 else 1.5,
            effect_max_sd = if (effect_size == "fixed4") 4 else 5))
}

summarise_calibration <- function(results, n_replicates) {
    keys <- c("scenario", "n_genes", "pi0", "null", "effect_size",
        "barcode_sd", "tau", "shift", "baseline_null_sd", "null_sd_multiplier",
        "effect_min_sd", "effect_max_sd", "method", "direction", "q")
    groups <- split(results, interaction(results[c("scenario", "method",
        "direction", "q")], drop = TRUE))
    summary <- do.call(rbind, lapply(groups, function(group) {
        out <- group[1, keys, drop = FALSE]
        out$replicates <- nrow(group)
        out$mean_fdp <- mean(group$fdp)
        out$se_fdp <- stats::sd(group$fdp) / sqrt(nrow(group))
        out$fdp_upper_95 <- out$mean_fdp + 1.96 * out$se_fdp
        out$power <- mean(group$power, na.rm = TRUE)
        out$runtime_seconds <- mean(group$runtime_seconds)
        out$ks_rejection_rate <- if (out$pi0 == 1) {
            mean(group$ks_p < 0.01)
        } else NA_real_
        # Upper one-sided 95% binomial quantile under the specified 1%
        # rejection rate at 500 replicates. The full gate is never relaxed
        # to accommodate quick runs or observed failures.
        out$ks_upper_bound <- stats::qbinom(0.95, 500, 0.01) / 500
        out$fdp_pass <- out$fdp_upper_95 <= out$q
        out$ks_pass <- out$pi0 != 1 ||
            out$ks_rejection_rate <= out$ks_upper_bound
        out
    }))
    rownames(summary) <- NULL
    central <- summary[summary$method == "central", ]
    compare <- c("scenario", "direction", "q")
    central_key <- do.call(paste, central[compare])
    summary_key <- do.call(paste, summary[compare])
    summary$runtime_vs_central <- summary$runtime_seconds /
        central$runtime_seconds[match(summary_key, central_key)]
    mixture <- summary$method == "mixture"
    expected <- expand.grid(pi0 = c(1, 0.95, 0.9, 0.8),
        null = c("correct", "shifted", "inflated"), n_genes = c(300L, 2000L),
        effect_size = c("fixed4", "uniform"), direction = c("Down", "Up"),
        q = c(0.05, 0.10), stringsAsFactors = FALSE)
    coverage_keys <- names(expected)
    expected_keys <- do.call(paste, expected[coverage_keys])
    observed_keys <- do.call(paste, summary[mixture, coverage_keys])
    complete <- length(observed_keys) == length(expected_keys) &&
        setequal(observed_keys, expected_keys) &&
        all(summary$replicates[mixture] == n_replicates)
    list(table = summary,
        gate = list(full_run = n_replicates == 500L,
            replicates = n_replicates, complete = complete,
            fdp_pass = all(summary$fdp_pass[mixture]),
            ks_pass = all(summary$ks_pass[mixture]),
            passed = n_replicates == 500L && complete &&
                all(summary$fdp_pass[mixture] & summary$ks_pass[mixture])))
}

run_fdr_calibration <- function(n_replicates = 500L,
    output = "fdr-calibration", seed = 20261028L) {
    if (length(n_replicates) != 1 || !is.finite(n_replicates) ||
        n_replicates < 2 || n_replicates > 500 ||
        n_replicates != as.integer(n_replicates)) {
        stop("n_replicates must be an integer from 2 to 500; full mode is 500.")
    }
    scenarios <- expand.grid(pi0 = c(1, 0.95, 0.9, 0.8),
        null = c("correct", "shifted", "inflated"),
        n_genes = c(300L, 2000L), effect_size = c("fixed4", "uniform"),
        stringsAsFactors = FALSE)
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    results <- vector("list", nrow(scenarios) * n_replicates * 8L)
    index <- 0L
    for (scenario in seq_len(nrow(scenarios))) {
        settings <- scenarios[scenario, ]
        message("Scenario ", scenario, "/", nrow(scenarios), ": ",
            paste(settings, collapse = ", "))
        for (replicate in seq_len(n_replicates)) {
            replicate_seed <- seed + (scenario - 1L) * 500L + replicate
            screen <- simulate_calibration_screen(settings$n_genes,
                settings$pi0, settings$null, replicate_seed, settings$effect_size)
            for (method in c("mixture", "central")) {
                for (direction in c("Down", "Up")) {
                    elapsed <- system.time({
                        fit <- ScreenR::find_gene_zscore_hit(screen$table,
                            reference = "empirical", empirical_fit = method,
                            direction = direction, fdr_method = "BH", fdr = 1,
                            seed = replicate_seed, n_folds = 5)
                    })[["elapsed"]]
                    truth <- screen$truth[fit$Gene]
                    ks_p <- if (settings$pi0 == 1) {
                        stats::ks.test(fit$PValue, "punif")$p.value
                    } else NA_real_
                    for (q in c(0.05, 0.10)) {
                        rejected <- fit$FDR <= q
                        # FDP counts discoveries among the true null genes.
                        false <- rejected & truth == "null"
                        n_true <- sum(truth == direction)
                        index <- index + 1L
                        results[[index]] <- data.frame(scenario = scenario,
                            settings, screen$parameters, replicate = replicate,
                            seed = replicate_seed, method = method,
                            direction = direction, q = q,
                            rejections = sum(rejected),
                            fdp = sum(false) / max(1, sum(rejected)),
                            power = if (n_true == 0) NA_real_ else
                                sum(rejected & truth == direction) / n_true,
                            ks_p = ks_p, runtime_seconds = elapsed)
                    }
                }
            }
        }
        # Preserve completed scenarios for long full-gate runs.
        utils::write.csv(do.call(rbind, results[seq_len(index)]),
            file.path(output, "replicates.csv"), row.names = FALSE)
    }
    results <- do.call(rbind, results)
    summary <- summarise_calibration(results, n_replicates)
    utils::write.csv(summary$table, file.path(output, "summary.csv"),
        row.names = FALSE)
    saveRDS(list(results = results, summary = summary$table,
        gate = summary$gate, scenarios = scenarios, seed = seed,
        session = utils::sessionInfo()), file.path(output, "calibration.rds"))
    gate <- summary$gate
    lines <- c(if (gate$full_run) "FULL RELEASE GATE" else
            "QUICK SMOKE RUN: cannot certify the 500-replicate release gate",
        paste("Replicates per scenario:", n_replicates),
        paste("All predefined scenarios complete:", gate$complete),
        paste("FDP upper 95% CI <= q in every scenario:", gate$fdp_pass),
        paste("All-null KS rejection rate within binomial bound:", gate$ks_pass),
        paste("Release gate passed:", gate$passed))
    writeLines(lines, file.path(output, "gate.txt"))
    message(paste(lines, collapse = "\n"))
    invisible(summary)
}

if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    replicates <- 500L
    output <- "fdr-calibration"
    for (arg in args) {
        if (startsWith(arg, "--replicates=")) {
            replicates <- as.integer(sub("^--replicates=", "", arg))
        } else if (startsWith(arg, "--output=")) {
            output <- sub("^--output=", "", arg)
        } else stop("Unknown argument: ", arg)
    }
    result <- run_fdr_calibration(replicates, output)
    if (result$gate$full_run && !result$gate$passed) quit(status = 1L)
}
