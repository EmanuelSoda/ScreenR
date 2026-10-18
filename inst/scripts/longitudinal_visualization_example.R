# Run with the current installed ScreenR, or source after pkgload::load_all().
# Figures are written to output_dir (set it before sourcing to choose a path).
library(ScreenR)
library(ggplot2)
library(dplyr)

# Appearance is chosen by the user, once, exactly as in the vignette.
theme_set(theme_light(base_size = 12))

if (!exists("output_dir", inherits = FALSE)) {
    output_dir <- file.path(tempdir(), "screenr-longitudinal-figures")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data(count_table, package = "ScreenR")
data(annotation_table, package = "ScreenR")
counts <- count_table[count_table$Barcode != "*", ]
sample_info <- data.frame(
    Sample = c("T0_Infected", "Puro_Baseline", paste0("T3_TRT_", 1:3),
        paste0("T3_CTRL_", 1:3), paste0("T4_TRT_", 1:3), paste0("T4_CTRL_", 1:3)),
    Day = c("T0", "Puro", rep("T3", 6), rep("T4", 6)),
    Treatment = c("Infected", "Baseline", rep(c("TRT", "CTRL"), each = 3),
        rep(c("TRT", "CTRL"), each = 3)))
sample_info$Group <- ifelse(sample_info$Day %in% c("T0", "Puro"),
    sample_info$Day, paste(sample_info$Day, sample_info$Treatment, sep = "_"))
colnames(counts) <- c("Barcode", sample_info$Sample)
object <- create_screenr_object(counts, annotation_table,
    groups = sample_info$Group, sample_table = sample_info,
    palettes = list(Treatment = c(Infected = "grey75", Baseline = "grey50",
        CTRL = "#1B9E77", TRT = "#D95F02")))
object <- normalize_data(remove_all_zero_row(object), "tmm")
SummarizedExperiment::colData(object)$Treatment <- factor(
    SummarizedExperiment::colData(object)$Treatment,
    levels = c("CTRL", "TRT", "Infected", "Baseline"))
design <- create_design_matrix(object)
t4 <- find_roast_hit(object, design,
    create_contrast(design, "T4_TRT", "T4_CTRL"), method = "fry")
fitness_ctrl <- find_roast_hit(object, design,
    create_contrast(design, "T4_CTRL", "Puro"), method = "fry")
fitness_trt <- find_roast_hit(object, design,
    create_contrast(design, "T4_TRT", "Puro"), method = "fry")
candidates <- head(t4$Gene, 3)
shared <- fitness_ctrl[fitness_ctrl$Gene %in% fitness_trt$Gene &
    !fitness_ctrl$Gene %in% candidates, ]
fitness_gene <- shared$Gene[which.min(shared$Log2FC)]
selected_genes <- c(candidates, fitness_gene)
if (length(selected_genes) != 4L) stop("Expected three candidates and a shared fitness gene.")

trend_plot <- plot_trend(object, selected_genes,
    c("Infected", "Baseline", "CTRL", "TRT"),
    color_by = "Treatment", trend = "mean", ncol = 2,
    reference_groups = c("Infected", "Baseline"), connect_reference = TRUE,
    highlight_difference = TRUE, point_size = 2, line_size = 1,
    scales = "free_y") +
    labs(title = "The complete screen: shared start, separate cultures",
        subtitle = paste0("Three T4 FRY candidates and one shared fitness candidate (", fitness_gene, ")"),
        x = "Screen stage (equally spaced)", y = "Mean normalized counts per shRNA",
        caption = paste0("Grey crosses: shared references, each observed once. Colored points: biological samples and condition means.\n",
            "Shading is the observed control-treatment gap, not uncertainty. Lines are descriptive. Panels use separate y scales."))

# An aligned effect view makes moderate differences visible even when the
# whole-screen abundance range is large. Nothing is inferred before the split.
effect_plot <- plot_treatment_separation(object, selected_genes,
    treatment = "TRT", control = "CTRL", ncol = 2,
    point_size = 3, line_size = 1) +
    labs(title = "How far does treatment separate from control?",
        subtitle = "Labels show the observed percentage difference in mean abundance (TRT vs CTRL)",
        x = "Screen stage (equally spaced)", y = "Log2 treatment / control",
        caption = paste0("Comparisons are defined only after the cultures split (T3 and T4); no effect is assigned to T0 or Puro.\n",
            "Ratios use sample mean normalized counts. They are descriptive, not gene-level model estimates or tests of temporal change."))

# Order each time point with controls first, followed by treated cultures.
shown <- sample_info
shown <- shown[order(match(shown$Day, c("T0", "Puro", "T3", "T4")),
    shown$Treatment, shown$Sample), ]
heatmap_plot <- plot_barcode_heatmap(object, selected_genes,
    samples = shown$Sample, center = "barcode", column_group = "Day") +
    labs(title = "Individual shRNAs across biological samples",
        subtitle = "All stages; within T3 and T4: CTRL 1-3, then TRT 1-3",
        caption = paste("Each row is one shRNA. Colors show log2 normalized abundance minus its mean across displayed samples.",
            "Blue: lower abundance; red: higher abundance. Colors are descriptive, not significance.")) +
    scale_x_discrete(guide = guide_axis(angle = 60))

for (format in c("png", "pdf")) {
    ggsave(file.path(output_dir, paste0("combined_trends.", format)),
        trend_plot, width = 12, height = 8.5, dpi = 220, bg = "white")
    ggsave(file.path(output_dir, paste0("treatment_separation.", format)),
        effect_plot, width = 11, height = 8, dpi = 220, bg = "white")
    ggsave(file.path(output_dir, paste0("barcode_heatmap.", format)),
        heatmap_plot, width = 14, height = 10, dpi = 220, bg = "white")
}
cat("Selected genes:", paste(selected_genes, collapse = ", "), "\n")
cat("Figures:", normalizePath(output_dir), "\n")
