test_that("the app finds the hits of the example CRISPR screen", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    skip_if_not_installed("DT")
    shiny::testServer(screenr_app(), {
        session$setInputs(source = "hap1", normalization = "cpm",
            crispr = TRUE, dark = "light")
        session$setInputs(create = 1)
        expect_s4_class(state$object, "screenr_object")
        session$setInputs(treatment = "T18", control = "T0",
            direction = "Down", methods = c("fry", "zscore"), fdr = 0.05,
            min_barcodes = 3)
        session$setInputs(run_hits = 1)
        expect_named(state$results$hit_list, c("fry", "zscore"))
        expect_gt(sum(hits()$Hit), 100)
    })
})

test_that("the sample table is guessed from the sample names", {
    samples <- app_guess_samples(c("T0", "T3_A", "T3_B", "D14-1", "D14-2"))
    expect_equal(samples$Group, c("T0", "T3", "T3", "D14", "D14"))
})

test_that("screenr_app() checks the object", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    skip_if_not_installed("DT")
    expect_error(screenr_app(data.frame()), "ScreenR object")
})
