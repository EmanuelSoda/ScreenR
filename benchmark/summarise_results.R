# Tables and figures of the benchmark, from the saved results.
# Usage (from benchmark/): Rscript summarise_results.R
# Reads  results/simulation_v2/*.rds  (run_benchmark.R, ScreenR >= 1.15.10)
#        results/simulation_v1_evaluation.rds (v1_before_calibration)
#        results/k562_methods.rds   (realdata/k562_screenr_methods.R)
#        results/realdata_results.rds (realdata/run_realdata.R, optional)
# Writes results/tables/*.csv and results/figures/*.pdf / *.png
suppressMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
})
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

labels <- c(
    MAGeCK_RRA = "MAGeCK RRA",
    MAGeCK_RRA_controls = "MAGeCK RRA (controls)",
    MAGeCK_MLE = "MAGeCK MLE",
    ScreenR_CAMERA = "ScreenR CAMERA",
    ScreenR_CAMERA_storey = "ScreenR CAMERA (Storey)",
    ScreenR_ROAST = "ScreenR ROAST",
    ScreenR_FRY = "ScreenR FRY",
    ScreenR_Zscore_mean = "ScreenR Z-score (mean)",
    ScreenR_Zscore_median = "ScreenR Z-score (median)",
    ScreenR_Zscore_controls = "ScreenR Z-score (controls)",
    ScreenR_Zscore_edgeR = "ScreenR Z-score (edgeR)",
    ScreenR_legacy_rule = "ScreenR legacy rule"
)
colours <- c(
    "MAGeCK RRA" = "#B2182B", "MAGeCK RRA (controls)" = "#EF8A62",
    "MAGeCK MLE" = "#67001F", "ScreenR CAMERA" = "#2166AC",
    "ScreenR CAMERA (Storey)" = "#67A9CF", "ScreenR ROAST" = "#1B7837",
    "ScreenR FRY" = "#7FBF7B", "ScreenR Z-score (mean)" = "#762A83",
    "ScreenR Z-score (median)" = "#C2A5CF",
    "ScreenR Z-score (controls)" = "#E7D4E8",
    "ScreenR Z-score (edgeR)" = "#8C510A", "ScreenR legacy rule" = "grey50"
)
scenario_labels <- c(
    S0_null = "No hits", S1_baseline = "Baseline",
    S2_weak_effect = "Weak effect", S3_two_replicates = "2 replicates",
    S4_noisy = "Noisy", S5_off_target = "Off-target"
)
label_methods <- function(x) factor(labels[x], levels = labels)
label_scenarios <- function(x) {
    factor(scenario_labels[x], levels = scenario_labels)
}
save_plot <- function(plot, name, width, height) {
    ggsave(file.path("results/figures", paste0(name, ".pdf")), plot,
        width = width, height = height)
    ggsave(file.path("results/figures", paste0(name, ".png")), plot,
        width = width, height = height, dpi = 200)
}
theme_set(theme_bw(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()))

# ------------------------------------------------------------ simulation ---
files <- list.files("results/simulation_v2", "\\.rds$", full.names = TRUE)
evaluation <- bind_rows(lapply(files, function(f) readRDS(f)$evaluation)) %>%
    mutate(Method = label_methods(method),
        Scenario = label_scenarios(scenario))
n_seeds <- n_distinct(evaluation$seed)

# Calibration: mean false discovery proportion vs nominal FDR
calibration <- evaluation %>%
    filter(method != "ScreenR_legacy_rule", scenario != "S0_null") %>%
    group_by(Method, Scenario, alpha) %>%
    summarise(FDR = mean(FDP), se = sd(FDP) / sqrt(n()), .groups = "drop")
p <- ggplot(calibration, aes(alpha, FDR, colour = Method)) +
    geom_abline(linetype = "dashed", colour = "grey40") +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~Scenario, nrow = 1) +
    scale_colour_manual(values = colours) +
    scale_x_continuous(breaks = c(0.01, 0.05, 0.1, 0.2)) +
    labs(x = "Nominal FDR", y = "Observed FDR (mean FDP)", colour = NULL) +
    guides(colour = guide_legend(nrow = 3))
save_plot(p, "simulation_calibration", 11, 4.2)

# Null screens: fraction of p-values below 0.05 (should be 0.05)
null_p <- evaluation %>%
    filter(scenario == "S0_null", method != "ScreenR_legacy_rule") %>%
    group_by(Method, alpha) %>%
    summarise(fraction = mean(null_p_below), se = sd(null_p_below) / sqrt(n()),
        false_calls = mean(n_called), .groups = "drop")
write.csv(null_p, "results/tables/simulation_null.csv", row.names = FALSE)
p <- ggplot(filter(null_p, alpha == 0.05),
    aes(fraction, Method, colour = Method)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", colour = "grey40") +
    geom_pointrange(aes(xmin = fraction - 2 * se, xmax = fraction + 2 * se)) +
    scale_colour_manual(values = colours, guide = "none") +
    scale_y_discrete(limits = rev) +
    labs(x = "Fraction of genes with p < 0.05 (no true hits)", y = NULL)
save_plot(p, "simulation_null", 5.5, 3.8)

# Power at 5% FDR and AUPRC
power <- evaluation %>%
    filter(scenario != "S0_null") %>%
    group_by(Method, Scenario, alpha) %>%
    summarise(TPR_mean = mean(TPR), TPR_se = sd(TPR) / sqrt(n()),
        FDP_mean = mean(FDP), AUPRC_mean = mean(AUPRC),
        AUPRC_se = sd(AUPRC) / sqrt(n()), time = mean(time),
        .groups = "drop")
write.csv(power, "results/tables/simulation_power.csv", row.names = FALSE)
p1 <- ggplot(filter(power, alpha == 0.05),
    aes(Scenario, TPR_mean, fill = Method)) +
    geom_col(position = position_dodge(0.9), width = 0.9) +
    geom_errorbar(aes(ymin = TPR_mean - TPR_se, ymax = TPR_mean + TPR_se),
        position = position_dodge(0.9), width = 0.3, linewidth = 0.3) +
    scale_fill_manual(values = colours) +
    labs(x = NULL, y = "True positive rate at 5% FDR", fill = NULL) +
    guides(fill = guide_legend(nrow = 3))
save_plot(p1, "simulation_power", 10, 4.5)
p2 <- evaluation %>%
    filter(scenario != "S0_null", alpha == 0.05, !is.na(AUPRC)) %>%
    ggplot(aes(Scenario, AUPRC, colour = Method)) +
    geom_boxplot(outlier.size = 0.5, position = position_dodge(0.85),
        width = 0.75) +
    scale_colour_manual(values = colours) +
    labs(x = NULL, y = "Area under the precision-recall curve",
        colour = NULL) +
    guides(colour = guide_legend(nrow = 3))
save_plot(p2, "simulation_auprc", 10, 4.5)

# Summary table at 5% FDR (mean over scenarios S1-S5 and seeds)
summary_table <- evaluation %>%
    filter(scenario != "S0_null", alpha == 0.05) %>%
    group_by(Method) %>%
    summarise(`Mean FDP` = mean(FDP), `Max FDP (scenario mean)` = NA_real_,
        TPR = mean(TPR), AUPRC = mean(AUPRC), `Time (s)` = median(time),
        .groups = "drop")
worst <- calibration %>% filter(alpha == 0.05) %>% group_by(Method) %>%
    summarise(worst = max(FDR))
summary_table$`Max FDP (scenario mean)` <-
    worst$worst[match(summary_table$Method, worst$Method)]
write.csv(summary_table, "results/tables/simulation_summary.csv",
    row.names = FALSE)

# ----------------------------------------- before / after the calibration ---
if (file.exists("results/simulation_v1_evaluation.rds")) {
    v1 <- readRDS("results/simulation_v1_evaluation.rds") %>%
        filter(method %in% c("ScreenR_CAMERA", "ScreenR_ROAST",
            "ScreenR_Zscore", "ScreenR_Zscore_edgeR", "MAGeCK_RRA",
            "MAGeCK_MLE"), scenario != "S0_null") %>%
        mutate(method = recode(method, ScreenR_Zscore = "ScreenR_Zscore_mean"),
            version = "Before (1.15.9)")
    v2 <- evaluation %>%
        filter(method %in% unique(v1$method), scenario != "S0_null",
            alpha %in% unique(v1$alpha)) %>%
        mutate(version = "After (1.15.10)")
    both <- bind_rows(v1, select(v2, all_of(names(v1)))) %>%
        mutate(Method = label_methods(method),
            Scenario = label_scenarios(scenario),
            version = factor(version, c("Before (1.15.9)", "After (1.15.10)")))
    before_after <- both %>% filter(alpha == 0.05) %>%
        group_by(Method, version) %>%
        summarise(FDP = mean(FDP), TPR = mean(TPR), AUPRC = mean(AUPRC),
            .groups = "drop")
    write.csv(before_after, "results/tables/before_after.csv",
        row.names = FALSE)
    p <- before_after %>%
        pivot_longer(c(FDP, TPR), names_to = "metric") %>%
        mutate(metric = recode(metric, FDP = "Observed FDR at 5%",
            TPR = "True positive rate at 5% FDR")) %>%
        ggplot(aes(value, Method, fill = version)) +
        geom_col(position = position_dodge(0.8), width = 0.75) +
        facet_wrap(~metric, scales = "free_x") +
        scale_fill_manual(values = c("grey70", "#2166AC")) +
        scale_y_discrete(limits = rev) +
        labs(x = NULL, y = NULL, fill = NULL)
    save_plot(p, "before_after", 8, 3.5)
}

# ------------------------------------------------------------------ K562 ---
if (file.exists("results/k562_methods.rds")) {
    k562 <- readRDS("results/k562_methods.rds")
    load("../data/k562_reference_genes.rda")
    write.csv(k562$eval, "results/tables/k562_evaluation.csv",
        row.names = FALSE)
    pr <- bind_rows(lapply(names(k562$res), function(m) {
        r <- k562$res[[m]]
        if (all(is.na(r$PValue))) return(NULL)
        merged <- merge(k562_reference_genes, r, by = "Gene", all.x = TRUE)
        merged$PValue[is.na(merged$PValue)] <- 1
        hit <- merged$Class[order(merged$PValue)] == "core essential"
        data.frame(method = m, recall = cumsum(hit) / sum(hit),
            precision = cumsum(hit) / seq_along(hit))
    }))
    k562_labels <- c(MAGeCK_RRA = "MAGeCK RRA", MAGeCK_MLE = "MAGeCK MLE",
        CAMERA = "ScreenR CAMERA", CAMERA_storey = "ScreenR CAMERA (Storey)",
        FRY = "ScreenR FRY", ROAST = "ScreenR ROAST",
        Zscore_mean = "ScreenR Z-score (mean)",
        Zscore_median = "ScreenR Z-score (median)",
        Zscore_controls = "ScreenR Z-score (controls)",
        Zscore_edgeR = "ScreenR Z-score (edgeR)")
    pr$Method <- factor(k562_labels[pr$method], levels = labels)
    p <- ggplot(filter(pr, method != "CAMERA_storey"),
        aes(recall, precision, colour = Method)) +
        geom_line() +
        scale_colour_manual(values = colours) +
        coord_cartesian(ylim = c(0.4, 1)) +
        labs(x = "Recall (core essential genes)",
            y = "Precision (vs non-essential genes)", colour = NULL) +
        guides(colour = guide_legend(nrow = 3))
    save_plot(p, "k562_precision_recall", 6.5, 5)
}
cat("Simulation runs:", length(files), "files,", n_seeds, "seeds\n")
