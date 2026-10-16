# Empirical null of find_gene_zscore_hit() (reference = "empirical") against
# the random barcode sets (default and control_genes), on the HAP1 CRISPR
# screen (subset in ScreenR and complete TKOv3 library) and on the K562 shRNA
# screen, evaluated on the core essential and non-essential genes of Hart et
# al. Half of the non-essential genes are the control_genes, the other half is
# kept out to count the false calls. The bundled subsets are enriched in
# essential genes, so they are also diluted to 5% and 10% essential genes.
# Usage: Rscript empirical_null.R <bagel>, where <bagel> is a clone of
# https://github.com/hart-lab/bagel (CEGv2.txt, NEGv1.txt and the HAP1 TKOv3
# example counts), for the complete HAP1 screen.
args <- commandArgs(TRUE)
suppressMessages({ library(ScreenR); library(dplyr) })

evaluate <- function(result, essential, held_out, alpha = 0.05) {
    ess <- intersect(essential, result$Gene)
    ne <- intersect(held_out, result$Gene)
    called <- result$Gene[result$FDR <= alpha]
    ref <- arrange(filter(result, Gene %in% c(ess, ne)), PValue, Gene)
    hit <- ref$Gene %in% ess
    precision <- cumsum(hit) / seq_along(hit)
    tibble(
        Recall = mean(ess %in% called),
        `NE called` = sum(called %in% ne),
        `NE p<=0.05` = mean(result$PValue[result$Gene %in% ne] <= 0.05),
        AUPRC = sum(precision[hit]) / sum(hit)
    )
}

compare <- function(table, essential, nonessential, split_seed = 2017) {
    set.seed(split_seed)
    reference <- sample(nonessential, length(nonessential) %/% 2)
    held_out <- setdiff(nonessential, reference)
    run <- function(...) find_gene_zscore_hit(table, fdr = 1,
        min_barcodes = 1, ...)
    results <- list(
        `Random, all barcodes (default)` = run(n_permutations = 2e4, seed = 1),
        `Random, non-essential controls` = run(control_genes = reference,
            n_permutations = 2e4, seed = 1),
        `Empirical, central` = run(reference = "empirical"),
        `Empirical, one-sided` = run(reference = "empirical",
            empirical_fit = "one-sided")
    )
    bind_rows(lapply(results, evaluate, essential, held_out), .id = "Method")
}

# Removes essential genes at random until they are `share` of the genes
dilute <- function(table, essential, share, seed) {
    set.seed(seed)
    genes <- unique(table$Gene)
    ess <- intersect(essential, genes)
    keep <- min(length(ess),
        round(share * (length(genes) - length(ess)) / (1 - share)))
    filter(table, !Gene %in% setdiff(ess, sample(ess, keep)))
}

data(hart_reference_genes, package = "ScreenR")
essential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "core essential"]
nonessential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "non-essential"]

# HAP1 subset, day 18 vs T0
data(hap1_count_table, package = "ScreenR")
data(hap1_annotation_table, package = "ScreenR")
samples <- data.frame(Sample = colnames(hap1_count_table)[-1], Day = "Screen",
    Treatment = c("T0", "T3", "T3", "T3", "T18", "T18", "T18"))
hap1 <- create_screenr_object(table = hap1_count_table,
    annotation = hap1_annotation_table,
    groups = factor(samples$Treatment, levels = c("T0", "T3", "T18")),
    sample_table = samples)
hap1 <- compute_data_table(normalize_data(remove_all_zero_row(hap1)))
design <- create_design_matrix(hap1)
hap1_stats <- compute_barcode_statistics(hap1, design,
    create_contrast(design, treatment = "T18", control = "T0"))

# K562, day 14 vs plasmid
data(k562_count_table, package = "ScreenR")
data(k562_annotation_table, package = "ScreenR")
data(k562_reference_genes, package = "ScreenR")
k562 <- create_screenr_object(table = k562_count_table,
    annotation = k562_annotation_table,
    groups = factor(c("Plasmid", "T14", "T14")),
    sample_table = data.frame(Sample = c("Plasmid", "T14_A", "T14_B"),
        Day = "Screen", Treatment = c("Plasmid", "T14", "T14")))
k562 <- compute_data_table(normalize_data(k562))
k562_metrics <- compute_metrics(k562, control = "Plasmid", treatment = "T14",
    day = "Screen")
k562_essential <- k562_reference_genes$Gene[
    k562_reference_genes$Class == "core essential"]
k562_nonessential <- k562_reference_genes$Gene[
    k562_reference_genes$Class != "core essential"]

results <- list()
for (share in c(0.05, 0.10)) for (seed in 1:3) {
    results[[length(results) + 1]] <- mutate(compare(
        dilute(hap1_stats, essential, share, seed), essential, nonessential),
        Data = "HAP1 subset", Essential = share, Seed = seed)
    results[[length(results) + 1]] <- mutate(compare(
        dilute(k562_metrics, k562_essential, share, seed), k562_essential,
        k562_nonessential), Data = "K562", Essential = share, Seed = seed)
}
results[[length(results) + 1]] <- mutate(compare(hap1_stats, essential,
    nonessential), Data = "HAP1 subset", Essential = 0.19, Seed = 0)
results[[length(results) + 1]] <- mutate(compare(k562_metrics, k562_essential,
    k562_nonessential), Data = "K562", Essential = 0.26, Seed = 0)

# Complete HAP1 TKOv3 screen, T18 vs T0
if (length(args) > 0) {
    raw <- read.delim(file.path(args[1],
        "pipeline-script-example/HAP1-TKOv3-EXAMPLE.txt"), check.names = FALSE)
    genome_essential <- sub("\t.*", "",
        readLines(file.path(args[1], "CEGv2.txt"))[-1])
    genome_nonessential <- sub("\t.*", "",
        readLines(file.path(args[1], "NEGv1.txt"))[-1])
    counts <- raw[, c("GENE_CLONE", "T0_-", "T18_A", "T18_B", "T18_C")]
    colnames(counts) <- c("Barcode", "T0", "T18_A", "T18_B", "T18_C")
    genome <- create_screenr_object(table = counts,
        annotation = data.frame(Barcode = raw$GENE_CLONE, Gene = raw$GENE),
        groups = factor(c("T0", "T18", "T18", "T18")),
        sample_table = data.frame(Sample = colnames(counts)[-1],
            Day = "Screen", Treatment = c("T0", "T18", "T18", "T18")))
    genome <- compute_data_table(normalize_data(remove_all_zero_row(genome)))
    design <- create_design_matrix(genome)
    genome_stats <- compute_barcode_statistics(genome, design,
        create_contrast(design, treatment = "T18", control = "T0"))
    for (seed in 1:3) {
        results[[length(results) + 1]] <- mutate(compare(genome_stats,
            genome_essential, genome_nonessential, split_seed = seed),
            Data = "HAP1 genome-wide", Essential = round(mean(
                unique(genome_stats$Gene) %in% genome_essential), 3),
            Seed = seed)
    }
}

summary <- bind_rows(results) %>%
    group_by(Data, Essential, Method) %>%
    summarise(across(c(Recall, `NE called`, `NE p<=0.05`, AUPRC), mean),
        .groups = "drop")
print(as.data.frame(summary), digits = 3, row.names = FALSE)
dir.create("../results", showWarnings = FALSE)
saveRDS(summary, "../results/empirical_null.rds")
