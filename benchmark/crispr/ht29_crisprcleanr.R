# Effect of correct_crispr_bias() (CRISPRcleanR) on the HT-29 screen
# distributed with CRISPRcleanR (KY library, Plasmid vs 3 replicates).
# Writes the summary shown in the CRISPR tutorial when CRISPRcleanR is not
# installed: inst/extdata/crisprcleanr_ht29.csv.
# Usage (from the repository root): Rscript benchmark/crispr/ht29_crisprcleanr.R
suppressMessages({
    library(ScreenR)
    library(dplyr)
})
data(hart_reference_genes)
essential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "core essential"]
nonessential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "non-essential"]
ccr <- "CRISPRcleanR"
library_annotation <- get(utils::data("KY_Library_v1.0", package = ccr))
counts <- read.delim(system.file("extdata", "HT-29_counts.tsv", package = ccr))
colnames(counts) <- c("Barcode", "Gene", "Plasmid", "HT29_R1", "HT29_R2",
    "HT29_R3")
samples <- data.frame(Sample = colnames(counts)[3:6], Day = "Screen",
    Treatment = c("Plasmid", "Late", "Late", "Late"))
object <- create_screenr_object(table = counts[, -2],
    annotation = counts[, c("Barcode", "Gene")],
    groups = factor(samples$Treatment, levels = c("Plasmid", "Late")),
    sample_table = samples)
corrected <- correct_crispr_bias(object, library_annotation,
    control = "Plasmid", treatment = c("HT29_R1", "HT29_R2", "HT29_R3"))

fold_changes <- function(o) {
    o <- compute_data_table(normalize_data(o))
    compute_metrics(o, control = "Plasmid", treatment = "Late", day = "Screen")
}
position <- data.frame(Barcode = rownames(library_annotation),
    Chromosome = library_annotation$CHRM,
    Start = library_annotation$STARTpos)
summarise_screen <- function(o, label) {
    m <- fold_changes(o)
    quality <- compute_screen_quality(m, essential, nonessential)
    # The MYC amplicon of HT-29 (chromosome 8q24): genes without known
    # essentiality there should not be depleted
    amplicon <- m %>%
        left_join(position, by = "Barcode") %>%
        filter(!Gene %in% essential, Chromosome == "8", Start > 110e6)
    data.frame(Counts = label, NNMD = quality$NNMD,
        CohenD = quality$CohenD, AUC = quality$AUC,
        MeanLog2FC_8q24 = mean(amplicon$Log2FC))
}
summary <- rbind(summarise_screen(object, "Raw"),
    summarise_screen(corrected, "Corrected (CRISPRcleanR)"))
write.csv(summary, "inst/extdata/crisprcleanr_ht29.csv", row.names = FALSE)
print(summary)
