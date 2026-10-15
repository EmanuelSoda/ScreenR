# Combine the competitor results (BAGEL2, gscreend, edgeR/limma workflows)
# with the ScreenR and MAGeCK results and write tables and figures.
# Usage (from benchmark/): Rscript competitors/summarise_competitors.R
# Reads  results/simulation_v2/*.rds, results/competitors_simulation/*.rds,
#        results/k562_methods.rds, results/realdata_competitors.rds,
#        results/screenr_dataset.rds
# Writes results/tables/competitors_*.csv, results/figures/competitors_*.png/pdf
suppressMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
})
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

labels <- c(
    ScreenR_Zscore_mean = "ScreenR Z-score (mean)",
    ScreenR_CAMERA = "ScreenR CAMERA",
    ScreenR_Zscore_edgeR = "ScreenR Z-score (edgeR)",
    ScreenR_FRY = "ScreenR FRY",
    MAGeCK_RRA = "MAGeCK RRA",
    MAGeCK_MLE = "MAGeCK MLE",
    BAGEL2 = "BAGEL2",
    gscreend = "gscreend",
    edgeR_gene_QL = "edgeR gene-level QL",
    limma_voom_camera = "limma-voom + camera"
)
colours <- c(
    "ScreenR Z-score (mean)" = "#1F5F55", "ScreenR CAMERA" = "#2E8B7A",
    "ScreenR Z-score (edgeR)" = "#93B5A3", "ScreenR FRY" = "#C8DCD0",
    "MAGeCK RRA" = "#E3A894", "MAGeCK MLE" = "#B5523A",
    "BAGEL2" = "#5B4E9E", "gscreend" = "#D4A72C",
    "edgeR gene-level QL" = "#4C78A8", "limma-voom + camera" = "#9ECAE9"
)
scenario_labels <- c(
    S0_null = "No hits", S1_baseline = "Baseline",
    S2_weak_effect = "Weak effect", S3_two_replicates = "2 replicates",
    S4_noisy = "Noisy", S5_off_target = "Off-target"
)
save_plot <- function(plot, name, width, height) {
    for (ext in c("pdf", "png")) {
        ggsave(file.path("results/figures", paste0(name, ".", ext)), plot,
            width = width, height = height, dpi = 200)
    }
}
theme_set(theme_bw(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()))

# ------------------------------------------------------------ simulation ---
read_eval <- function(dir) {
    files <- list.files(dir, "\\.rds$", full.names = TRUE)
    bind_rows(lapply(files, function(f) readRDS(f)$evaluation))
}
evaluation <- bind_rows(read_eval("results/simulation_v2"),
    read_eval("results/competitors_simulation")) %>%
    filter(method %in% names(labels)) %>%
    mutate(Method = factor(labels[method], levels = labels),
        Scenario = factor(scenario_labels[scenario], levels = scenario_labels))

null_table <- evaluation %>%
    filter(scenario == "S0_null", alpha == 0.05, method != "BAGEL2") %>%
    group_by(Method) %>%
    summarise(`Fraction p < 0.05` = mean(null_p_below),
        `Genes called at 5% FDR` = mean(n_called), .groups = "drop")
write.csv(null_table, "results/tables/competitors_null.csv", row.names = FALSE)

with_hits <- filter(evaluation, scenario != "S0_null")
summary_table <- with_hits %>%
    filter(alpha == 0.05) %>%
    group_by(Method) %>%
    summarise(`Mean FDP` = mean(FDP), TPR = mean(TPR), AUPRC = mean(AUPRC),
        `Time (s)` = median(time), .groups = "drop")
worst <- with_hits %>%
    filter(alpha == 0.05) %>%
    group_by(Method, Scenario) %>%
    summarise(FDP = mean(FDP), .groups = "drop") %>%
    group_by(Method) %>%
    summarise(`Worst scenario FDP` = max(FDP), .groups = "drop")
summary_table <- left_join(summary_table, worst, by = "Method") %>%
    relocate(`Worst scenario FDP`, .after = `Mean FDP`)
write.csv(summary_table, "results/tables/competitors_summary.csv",
    row.names = FALSE)

by_scenario <- with_hits %>%
    filter(alpha == 0.05) %>%
    group_by(Method, Scenario) %>%
    summarise(FDP = mean(FDP), TPR = mean(TPR),
        AUPRC_se = sd(AUPRC) / sqrt(n()), AUPRC = mean(AUPRC),
        .groups = "drop")
write.csv(by_scenario, "results/tables/competitors_by_scenario.csv",
    row.names = FALSE)

p <- ggplot(by_scenario, aes(Scenario, AUPRC, colour = Method)) +
    geom_pointrange(aes(ymin = AUPRC - AUPRC_se, ymax = AUPRC + AUPRC_se),
        position = position_dodge(0.7), size = 0.3) +
    scale_colour_manual(values = colours) +
    labs(x = NULL, y = "Area under the precision-recall curve",
        colour = NULL) +
    guides(colour = guide_legend(nrow = 3))
save_plot(p, "competitors_auprc", 9, 4.5)

p <- with_hits %>%
    group_by(Method, Scenario, alpha) %>%
    summarise(FDR = mean(FDP), .groups = "drop") %>%
    ggplot(aes(alpha, FDR, colour = Method)) +
    geom_abline(linetype = "dashed", colour = "grey40") +
    geom_line() + geom_point(size = 0.8) +
    facet_wrap(~Scenario, nrow = 1) +
    scale_colour_manual(values = colours) +
    scale_x_continuous(breaks = c(0.01, 0.05, 0.1, 0.2)) +
    labs(x = "Nominal FDR", y = "Observed FDR (mean FDP)", colour = NULL) +
    guides(colour = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
save_plot(p, "competitors_calibration", 11, 4.5)

p <- ggplot(by_scenario, aes(Scenario, TPR, fill = Method)) +
    geom_col(position = position_dodge(0.9)) +
    scale_fill_manual(values = colours) +
    labs(x = NULL, y = "True positive rate at 5% FDR", fill = NULL) +
    guides(fill = guide_legend(nrow = 3))
save_plot(p, "competitors_power", 10, 4.5)

# ------------------------------------------------------------------ K562 ---
screenr_k562 <- readRDS("results/k562_methods.rds")
competitors_real <- readRDS("results/realdata_competitors.rds")
k562_labels <- c(MAGeCK_RRA = "MAGeCK RRA", MAGeCK_MLE = "MAGeCK MLE",
    CAMERA = "ScreenR CAMERA", FRY = "ScreenR FRY",
    Zscore_mean = "ScreenR Z-score (mean)",
    Zscore_edgeR = "ScreenR Z-score (edgeR)")
k562_eval <- bind_rows(
    screenr_k562$eval %>% filter(method %in% names(k562_labels)) %>%
        mutate(Method = k562_labels[method]),
    competitors_real$k562_eval %>% mutate(Method = labels[method])
) %>%
    filter(alpha == 0.05) %>%
    transmute(Method, `Essential called` = essential_called,
        `Non-essential called` = nonessential_called,
        Recall = recall, `Non-essential fraction` = false_frac, AUPRC,
        `Time (s)` = time) %>%
    arrange(desc(AUPRC))
write.csv(k562_eval, "results/tables/competitors_k562.csv", row.names = FALSE)

load("../data/k562_reference_genes.rda")
k562_results <- c(
    setNames(screenr_k562$res[names(k562_labels)], k562_labels),
    setNames(competitors_real$k562, labels[names(competitors_real$k562)])
)
pr <- bind_rows(lapply(names(k562_results), function(m) {
    merged <- merge(k562_reference_genes, k562_results[[m]], by = "Gene",
        all.x = TRUE)
    merged$PValue[is.na(merged$PValue)] <- 1
    hit <- merged$Class[order(merged$PValue)] == "core essential"
    data.frame(Method = m, recall = cumsum(hit) / sum(hit),
        precision = cumsum(hit) / seq_along(hit))
})) %>% mutate(Method = factor(Method, levels = labels))
p <- ggplot(pr, aes(recall, precision, colour = Method)) +
    geom_line() +
    scale_colour_manual(values = colours) +
    coord_cartesian(ylim = c(0.4, 1)) +
    labs(x = "Recall (core essential genes)", y = "Precision",
        colour = NULL) +
    guides(colour = guide_legend(nrow = 4))
save_plot(p, "competitors_k562_precision_recall", 7, 5.5)

# ------------------------------------------------------ ScreenR dataset ---
screenr_dataset <- readRDS("results/screenr_dataset.rds")
example <- c(screenr_dataset$res, competitors_real$screenr_data)
genes <- Reduce(intersect, lapply(example, function(x) as.character(x$Gene)))
ranks <- sapply(example, function(x) {
    rank(x$PValue[match(genes, as.character(x$Gene))])
})
example_labels <- c(MAGeCK_RRA = "MAGeCK RRA", MAGeCK_MLE = "MAGeCK MLE",
    CAMERA = "ScreenR CAMERA", ROAST = "ScreenR ROAST", FRY = "ScreenR FRY",
    Zscore_mean = "ScreenR Z-score (mean)",
    Zscore_edgeR = "ScreenR Z-score (edgeR)",
    edgeR_gene_QL = "edgeR gene-level QL",
    limma_voom_camera = "limma-voom + camera", gscreend = "gscreend")
colnames(ranks) <- example_labels[colnames(ranks)]
correlation <- cor(ranks, method = "spearman")
write.csv(round(correlation, 3), "results/tables/competitors_screenr_dataset_correlation.csv")
write.csv(data.frame(Method = example_labels[names(example)],
    `Hits at 5% FDR` = vapply(example, function(x) sum(x$FDR <= 0.05),
        integer(1)), check.names = FALSE),
    "results/tables/competitors_screenr_dataset_hits.csv", row.names = FALSE)
p <- as.data.frame(as.table(correlation)) %>%
    ggplot(aes(Var1, Var2, fill = Freq)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.6) +
    scale_fill_gradient(low = "white", high = "#1F5F55", limits = c(0, 1)) +
    scale_y_discrete(limits = rev) +
    labs(x = NULL, y = NULL, fill = "Spearman") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
save_plot(p, "competitors_screenr_dataset_correlation", 7.5, 6)

options(width = 160)
print(summary_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
print(null_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
print(k562_eval %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
