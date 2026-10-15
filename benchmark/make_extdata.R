# Copy the precomputed benchmark results used by the "Benchmark against
# MAGeCK" vignette into inst/extdata/benchmark (small CSV files).
# Usage (from benchmark/): Rscript make_extdata.R
# Needs results/ filled by run_benchmark.R, run_realdata.R and
# realdata/screenr_dataset.R.
suppressMessages(library(dplyr))
out <- "../inst/extdata/benchmark"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
round_numeric <- function(x) mutate(x, across(where(is.double), ~ signif(.x, 8)))

# Simulation: one row per method, scenario, seed and FDR level
files <- list.files("results/simulation_v2", "\\.rds$", full.names = TRUE)
simulation <- bind_rows(lapply(files, function(f) readRDS(f)$evaluation))
write.csv(round_numeric(simulation), file.path(out, "simulation.csv"),
    row.names = FALSE)
before <- readRDS("results/simulation_v1_evaluation.rds")
write.csv(round_numeric(before), file.path(out, "simulation_before_1.15.10.csv"),
    row.names = FALSE)

# MAGeCK gene results of the real screens (depletion p-values)
k562 <- readRDS("results/k562_methods.rds")$res
mageck_k562 <- bind_rows(lapply(c("MAGeCK_RRA", "MAGeCK_MLE"), function(m)
    data.frame(method = m, k562[[m]][, c("Gene", "PValue", "FDR")])))
write.csv(round_numeric(mageck_k562), file.path(out, "mageck_k562.csv"),
    row.names = FALSE)
screenr <- readRDS("results/screenr_dataset.rds")$res
mageck_screenr <- bind_rows(lapply(c("MAGeCK_RRA", "MAGeCK_MLE"), function(m)
    data.frame(method = m, screenr[[m]][, c("Gene", "PValue", "FDR")])))
write.csv(round_numeric(mageck_screenr),
    file.path(out, "mageck_screenr_dataset.csv"), row.names = FALSE)
writeLines(c(
    "Precomputed results of the benchmark of ScreenR against MAGeCK 0.5.9.5,",
    "used by the vignette 'Benchmark against MAGeCK'. Produced by the scripts",
    "in the benchmark/ folder of https://github.com/EmanuelSoda/ScreenR",
    "(run_benchmark.R, run_realdata.R, screenr_dataset.R, make_extdata.R).",
    "",
    "simulation.csv: evaluation of each method on 30 simulated screens",
    "  (6 scenarios x 5 seeds) at FDR 1, 5, 10, 20%: number of genes called,",
    "  false discovery proportion, true positive rate, fraction of null genes",
    "  with p <= alpha, area under the precision-recall curve, run time (s).",
    "simulation_before_1.15.10.csv: the same for the first benchmark",
    "  (ScreenR 1.15.9, 3 seeds, no non-targeting shRNAs).",
    "mageck_k562.csv: MAGeCK RRA and MLE gene p-values and FDR (depletion)",
    "  for k562_count_table (Plasmid vs T14).",
    "mageck_screenr_dataset.csv: the same for count_table (Time4 TRT vs CTRL)."
), file.path(out, "README.txt"))
print(file.info(list.files(out, full.names = TRUE))["size"])
