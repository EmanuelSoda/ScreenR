# Benchmark of ScreenR against MAGeCK

Scripts used to compare the hit-finding methods of ScreenR with MAGeCK
(RRA and MLE) on simulated screens and on real shRNA screens. They are not
part of the package (the folder is in `.Rbuildignore`).

## Requirements

* R with ScreenR (>= 1.15.10), dplyr and parallel.
* MAGeCK 0.5.9.5: `bash install_mageck.sh mageck` installs it in a Python
  virtual environment; the executable is then `mageck/env/bin/mageck`.

## Simulated screens (`simulation/`)

`simulate.R` generates pooled shRNA screens with known hits: 500 genes with
10 shRNAs each plus 500 non-targeting shRNAs, negative binomial counts,
log-normal library abundance, 10% of genes depleted in the treated samples
with a per-shRNA knockdown efficiency (20% of the shRNAs inactive).

`run_benchmark.R` runs 6 scenarios x 5 seeds:

| Scenario | Change from the baseline |
|---|---|
| S0_null | no true hits |
| S1_baseline | 3 vs 3 replicates, log2 effect -1, dispersion 0.05 |
| S2_weak_effect | log2 effect -0.5 |
| S3_two_replicates | 2 vs 2 replicates |
| S4_noisy | dispersion 0.2 |
| S5_off_target | 3% of shRNAs depleted whatever their gene |

Methods (`run_methods.R`): MAGeCK RRA, MAGeCK RRA with `--control-sgrna`,
MAGeCK MLE; ScreenR CAMERA (BH and Storey FDR), ROAST, FRY, gene Z-score
permutation test (mean, median, non-targeting reference, edgeR statistics)
and the legacy `find_zscore_hit` rule. For each method and FDR level (1%, 5%,
10%, 20%) it records the false discovery proportion, the true positive rate,
the fraction of null p-values below the level, the AUPRC and the run time.

```sh
cd simulation
Rscript run_benchmark.R <ScreenR lib> <path/to/mageck> <cores>
# results/<scenario>_seed<k>.rds and evaluation.rds
```

`calibration_null.R` checks the p-value distribution of the ScreenR methods
on null screens. `v1_before_calibration/` holds the first version of the
benchmark (ScreenR <= 1.15.9: two-sided CAMERA/ROAST p-values, binomial
gene test, no non-targeting shRNAs, 3 seeds), kept to reproduce the
before/after comparison.

## Real screens (`realdata/`)

* K562 essentiality screen (Morgens et al. 2016, dataset `k562_count_table`
  in ScreenR): Plasmid vs day 14, evaluated on the core essential (true
  hits) and non-essential (true negatives) reference genes of Hart et al.
* ScreenR example dataset (`count_table`): Time4 treated vs control,
  concordance between methods (no ground truth).

```sh
cd realdata
Rscript run_realdata.R <ScreenR lib> <path/to/mageck> <ScreenR repo>
Rscript k562_screenr_methods.R <ScreenR lib> <ScreenR repo> \
    realdata_work/k562_rra.gene_summary.txt
```

The script that builds the K562 dataset is
`inst/scripts/k562_essential_screen.R`.

## Vignette data

`make_extdata.R` copies the simulation evaluations and the MAGeCK gene results
of the real screens into `inst/extdata/benchmark`, used by the package
vignette "Benchmark against MAGeCK" (MAGeCK cannot run on the Bioconductor
builders).

## Report

`paper/ScreenR_report.Rmd` documents all the changes of ScreenR 1.15.1 –
1.15.10 and the benchmark results; `paper/ScreenR_report.pdf` is the rendered
version. Rebuild it with `Rscript summarise_results.R` and then
`rmarkdown::render("paper/ScreenR_report.Rmd")` (HTML; the PDF was printed
from the HTML with Chromium).
