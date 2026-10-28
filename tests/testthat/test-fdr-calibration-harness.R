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
