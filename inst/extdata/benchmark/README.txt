Precomputed results of the benchmark of ScreenR against MAGeCK 0.5.9.5,
used by the vignette 'Benchmark against MAGeCK'. Produced by the scripts
in the benchmark/ folder of https://github.com/EmanuelSoda/ScreenR
(run_benchmark.R, run_realdata.R, screenr_dataset.R, make_extdata.R).

simulation.csv: evaluation of each method on 30 simulated screens
  (6 scenarios x 5 seeds) at FDR 1, 5, 10, 20%: number of genes called,
  false discovery proportion, true positive rate, fraction of null genes
  with p <= alpha, area under the precision-recall curve, run time (s).
simulation_before_1.15.10.csv: the same for the first benchmark
  (ScreenR 1.15.9, 3 seeds, no non-targeting shRNAs).
mageck_k562.csv: MAGeCK RRA and MLE gene p-values and FDR (depletion)
  for k562_count_table (Plasmid vs T14).
mageck_screenr_dataset.csv: the same for count_table (Time4 TRT vs CTRL).
simulation_competitors.csv: as simulation.csv for BAGEL2, gscreend and
  two plain edgeR/limma workflows (benchmark/competitors).
competitors_k562.csv: their gene results on k562_count_table.
crispr_hap1.csv: gene results of every method on the HAP1 TKOv3
  CRISPR-Cas9 screen (Hart et al. 2017, T18 vs T0), restricted to the
  CEGv2/NEGv1 reference genes (benchmark/crispr/hap1_crispr.R).
empirical_null.csv: find_gene_zscore_hit() with the random barcode sets
  and with the empirical null, on the HAP1 CRISPR screen (subset and
  complete TKOv3 library) and the K562 shRNA screen, with the essential
  genes diluted to 5% and 10% of the genes: recall of the core essential
  genes at 5% FDR, held-out non-essential genes called and with
  p <= 0.05, AUPRC (benchmark/realdata/empirical_null.R).
