# Benchmark ScreenR against MAGeCK on simulated shRNA screens.
# Usage: Rscript run_benchmark.R <ScreenR lib> <mageck executable> <cores>
args <- commandArgs(TRUE)
suppressMessages({
    library(ScreenR, lib.loc = args[1])
    library(dplyr)
    library(parallel)
})
source("simulate.R")
source("run_methods.R")
mageck <- args[2]
cores <- as.integer(args[3])

base <- list(n_genes = 500, n_barcodes = 10, n_reps = 3, hit_fraction = 0.1,
    effect = -1, prop_inactive = 0.2, dispersion = 0.05, off_target = 0)
scenarios <- list(
    S0_null = modifyList(base, list(hit_fraction = 0)),
    S1_baseline = base,
    S2_weak_effect = modifyList(base, list(effect = -0.5)),
    S3_two_replicates = modifyList(base, list(n_reps = 2)),
    S4_noisy = modifyList(base, list(dispersion = 0.2)),
    S5_off_target = modifyList(base, list(off_target = 0.03))
)
runs <- expand.grid(scenario = names(scenarios), seed = 1:5,
    stringsAsFactors = FALSE)

run_one <- function(k) {
    scenario <- runs$scenario[k]
    seed <- runs$seed[k]
    out_file <- file.path("results", sprintf("%s_seed%d.rds", scenario, seed))
    if (file.exists(out_file)) return(readRDS(out_file))
    sim <- do.call(simulate_screen, c(scenarios[[scenario]], seed = seed))
    workdir <- file.path("work", sprintf("%s_seed%d", scenario, seed))
    results <- c(run_mageck(sim, workdir, mageck), run_screenr(sim))
    evaluation <- bind_rows(lapply(names(results), function(m) {
        cbind(method = m, evaluate(results[[m]], sim$truth))
    }))
    evaluation$scenario <- scenario
    evaluation$seed <- seed
    out <- list(evaluation = evaluation, results = results, truth = sim$truth)
    saveRDS(out, out_file)
    out
}

dir.create("results", showWarnings = FALSE)
dir.create("work", showWarnings = FALSE)
all_runs <- mclapply(seq_len(nrow(runs)), function(k) {
    tryCatch(run_one(k), error = function(e) {
        message("Run ", k, " failed: ", conditionMessage(e))
        NULL
    })
}, mc.cores = cores)
evaluation <- bind_rows(lapply(all_runs, function(x) x$evaluation))
saveRDS(evaluation, "evaluation.rds")
cat("Finished", sum(!vapply(all_runs, is.null, logical(1))), "of",
    nrow(runs), "runs\n")
