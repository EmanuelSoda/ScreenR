# Run BAGEL2, gscreend and the plain edgeR/limma workflows on the same 30
# simulated screens as simulation/run_benchmark.R (same scenarios, same
# seeds, so the counts are identical) and evaluate them in the same way.
# Usage (from benchmark/competitors/):
#   Rscript run_competitors_simulation.R <BAGEL.py> <python for BAGEL2> <cores>
args <- commandArgs(TRUE)
suppressMessages({
    library(dplyr)
    library(parallel)
})
source("../simulation/simulate.R")
source("../simulation/run_methods.R") # evaluate()
source("competitor_methods.R")
bagel <- normalizePath(args[1])
# not normalizePath(): it would resolve the virtualenv symlink to the
# system python, which does not have the BAGEL2 dependencies
python <- args[2]
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
dir.create("results", showWarnings = FALSE)

run_one <- function(k) {
    scenario <- runs$scenario[k]
    seed <- runs$seed[k]
    out_file <- file.path("results", sprintf("%s_seed%d.rds", scenario, seed))
    if (file.exists(out_file)) return(readRDS(out_file))
    sim <- do.call(simulate_screen, c(scenarios[[scenario]], seed = seed))
    ctrl <- grep("^CTRL", sim$samples, value = TRUE)
    trt <- grep("^TRT", sim$samples, value = TRUE)
    gene <- sim$annotation$Gene
    results <- list(
        edgeR_gene_QL = run_edger_gene(sim$counts, gene, ctrl, trt),
        limma_voom_camera = run_voom_camera(sim$counts, gene, ctrl, trt),
        gscreend = run_gscreend(sim$counts, gene, ctrl, trt)
    )
    # BAGEL2 is supervised: train on half of the true hits and half of the
    # genes without effect (cross-validated Bayes factors). Not applicable
    # to the screens without hits.
    hits <- sim$truth$Gene[sim$truth$hit]
    if (length(hits) > 0) {
        nulls <- sim$truth$Gene[!sim$truth$hit]
        set.seed(seed)
        results$BAGEL2 <- run_bagel2(sim$counts, gene, ctrl, trt,
            essentials = sample(hits, length(hits) %/% 2),
            nonessentials = sample(nulls, length(nulls) %/% 2),
            workdir = file.path("work", sprintf("%s_seed%d", scenario, seed)),
            bagel = bagel, python = python, seed = seed)
    }
    evaluation <- bind_rows(lapply(names(results), function(m) {
        cbind(method = m, evaluate(results[[m]], sim$truth))
    }))
    # BAGEL2 has no p-values: the fraction of null p-values is meaningless
    evaluation$null_p_below[evaluation$method == "BAGEL2"] <- NA
    evaluation$scenario <- scenario
    evaluation$seed <- seed
    out <- list(evaluation = evaluation, results = results, truth = sim$truth)
    saveRDS(out, out_file)
    out
}

all_runs <- mclapply(seq_len(nrow(runs)), function(k) {
    tryCatch(run_one(k), error = function(e) {
        message("Run ", k, " failed: ", conditionMessage(e))
        NULL
    })
}, mc.cores = cores)
evaluation <- bind_rows(lapply(all_runs, function(x) x$evaluation))
saveRDS(evaluation, "evaluation_competitors.rds")
cat("Finished", sum(!vapply(all_runs, is.null, logical(1))), "of",
    nrow(runs), "runs\n")
