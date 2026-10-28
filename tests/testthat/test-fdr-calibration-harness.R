calibration_environment <- function() {
    envir <- new.env(parent = globalenv())
    sys.source(system.file("benchmarks", "fdr_calibration.R",
        package = "ScreenR"), envir = envir)
    envir
}

test_that("calibration simulation has the predefined scenario structure", {
    benchmark <- calibration_environment()
    screen <- benchmark$simulate_calibration_screen(300, 0.8, "correct", 19)
    expect_identical(screen,
        benchmark$simulate_calibration_screen(300, 0.8, "correct", 19))
    expect_true(all(table(screen$table$Gene) %in% 3:10))
    expect_equal(as.integer(table(screen$truth)[c("null", "Down", "Up")]),
        c(240L, 30L, 30L))
    shifted <- benchmark$simulate_calibration_screen(300, 1, "shifted", 19)
    correct <- benchmark$simulate_calibration_screen(300, 1, "correct", 19)
    expect_equal(shifted$table$Zscore - correct$table$Zscore,
        rep(0.5 * sqrt(0.4^2 + mean(1 / (3:10))), nrow(correct$table)))
})

test_that("calibration gates are strict and smoke runs cannot certify release", {
    benchmark <- calibration_environment()
    results <- data.frame(scenario = 1, n_genes = 300, pi0 = 1,
        effect_size = "fixed4", barcode_sd = 1, tau = 0.4, shift = 0,
        baseline_null_sd = sqrt(0.4^2 + mean(1 / (3:10))),
        null_sd_multiplier = 1, effect_min_sd = 4, effect_max_sd = 4,
        null = "correct", method = "mixture", direction = "Down", q = 0.05,
        fdp = rep(0, 500), power = NA_real_, runtime_seconds = 1, ks_p = 0.5)
    passed <- benchmark$summarise_calibration(results, 500)
    expect_true(passed$gate$fdp_pass)
    expect_true(passed$gate$ks_pass)
    expect_false(passed$gate$complete)
    expect_false(passed$gate$passed)
    expect_equal(passed$table$fdp_upper_95, 0)
    expect_false(benchmark$summarise_calibration(results[1:2, ], 2)$gate$passed)
    results$fdp[1:50] <- 1
    expect_false(benchmark$summarise_calibration(results, 500)$gate$fdp_pass)
    results$fdp <- 0
    bound <- stats::qbinom(0.95, 500, 0.01)
    results$ks_p[seq_len(bound)] <- 0.001
    expect_true(benchmark$summarise_calibration(results, 500)$gate$ks_pass)
    results$ks_p[bound + 1] <- 0.001
    expect_false(benchmark$summarise_calibration(results, 500)$gate$ks_pass)
    results$method <- "central"
    results <- rbind(results, transform(results,
        method = "mixture", fdp = 0, ks_p = 0.5))
    expect_true(benchmark$summarise_calibration(results, 500)$gate$fdp_pass)
    expect_true(benchmark$summarise_calibration(results, 500)$gate$ks_pass)
})

test_that("parallel calibration preserves replicate seeds and results", {
    skip_on_os("windows")
    benchmark <- calibration_environment()
    # Exercise dispatch and pooling cheaply; statistical fitting is tested
    # separately. Forked workers inherit this binding and explicit seeds.
    benchmark$calibration_gene_test <- function(table, seed,
        direction, ...) {
        genes <- sort(unique(table$Gene))
        set.seed(seed)
        p <- runif(length(genes))
        if (direction == "Up") p <- 1 - p
        data.frame(Gene = genes, PValue = p, FDR = p.adjust(p, "BH"))
    }
    # system.time() normally forces a full GC before every call. Timing is
    # not under test here, and hundreds of GCs are costly in the full suite.
    benchmark$system.time <- function(expr) {
        force(expr)
        c(elapsed = 0.001)
    }
    serial <- tempfile("calibration-serial-")
    parallel <- tempfile("calibration-parallel-")
    on.exit(unlink(c(serial, parallel), recursive = TRUE), add = TRUE)
    suppressMessages(benchmark$run_fdr_calibration(2L, serial, cores = 1L))
    suppressMessages(benchmark$run_fdr_calibration(2L, parallel, cores = 2L))
    first <- readRDS(file.path(serial, "calibration.rds"))
    second <- readRDS(file.path(parallel, "calibration.rds"))
    keep <- setdiff(names(first$results), "runtime_seconds")
    expect_identical(first$results[keep], second$results[keep])
    expect_identical(first$gate, second$gate)
    expect_equal(nrow(second$results), 48L * 2L * 8L)
    expect_equal(second$results$seed, 20261028L +
        (second$results$scenario - 1L) * 500L + second$results$replicate)
    expect_equal(second$timing$cores, 2L)
    expect_gt(second$timing$projected_full_seconds, 0)
    expect_false(second$gate$full_run)
})
