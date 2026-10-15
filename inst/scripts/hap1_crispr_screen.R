# Build the hap1_count_table and hap1_annotation_table datasets: a subset of
# the genome-wide CRISPR-Cas9 knockout screen of HAP1 cells with the TKOv3
# library (Hart et al. 2017, G3 7:2719-2727), distributed as example data of
# BAGEL2 (https://github.com/hart-lab/bagel, MIT license, see
# inst/LICENSES_data.txt), with the sgRNA positions of the TKOv3 library
# annotation for CRISPRcleanR from the same repository.
#
# The subset keeps
# - all the sgRNAs of the core essential (CEGv2) and non-essential (NEGv1)
#   reference genes, to check the screen and evaluate the hits;
# - all the sgRNAs on chromosome 15 (HAP1 cells carry an extra copy of a
#   region of chromosome 15), to show the copy-number correction;
# - the control sgRNAs (EGFP, LacZ, luciferase);
# - all the sgRNAs of 1500 other randomly chosen genes, so that the essential
#   genes are not over-represented (the competitive tests compare each gene
#   with the others).
# Samples: T0 and three replicates after 3 and 18 days.
#
# Usage: Rscript inst/scripts/hap1_crispr_screen.R <bagel dir>
args <- commandArgs(TRUE)
bagel <- args[1]
example <- file.path(bagel, "pipeline-script-example")
raw <- read.delim(file.path(example, "HAP1-TKOv3-EXAMPLE.txt"),
    check.names = FALSE)
library <- read.delim(file.path(example,
    "TKOv3_library_forCRISPRcleanR_REAGENT_ID.txt"), row.names = 1)
ceg <- read.delim(file.path(bagel, "CEGv2.txt"))$GENE
neg <- read.delim(file.path(bagel, "NEGv1.txt"))$GENE

chromosome <- library[raw$GENE_CLONE, "CHRM"]
controls <- c("EGFP", "LacZ", "luciferase")
set.seed(2017)
others <- setdiff(unique(raw$GENE), c(ceg, neg, controls,
    unique(raw$GENE[chromosome %in% "15"])))
random_genes <- sample(others, 1500)
keep <- raw$GENE %in% c(ceg, neg, random_genes) | chromosome %in% "15" |
    raw$GENE %in% controls
raw <- raw[keep, ]

hap1_count_table <- data.frame(
    Barcode = raw$GENE_CLONE,
    T0 = raw[["T0_-"]],
    T3_A = raw$T3_A, T3_B = raw$T3_B, T3_C = raw$T3_C,
    T18_A = raw$T18_A, T18_B = raw$T18_B, T18_C = raw$T18_C
)
position <- library[raw$GENE_CLONE, ]
hap1_annotation_table <- data.frame(
    Barcode = raw$GENE_CLONE,
    Gene = raw$GENE,
    Chromosome = position$CHRM,
    Strand = position$STRAND,
    Start = as.integer(position$STARTpos),
    End = as.integer(position$ENDpos),
    Class = ifelse(raw$GENE %in% ceg, "core essential",
        ifelse(raw$GENE %in% neg, "non-essential",
            ifelse(raw$GENE %in% controls, "control", "other")))
)
rownames(hap1_count_table) <- NULL
rownames(hap1_annotation_table) <- NULL
save(hap1_count_table, file = "data/hap1_count_table.rda", compress = "xz")
save(hap1_annotation_table, file = "data/hap1_annotation_table.rda",
    compress = "xz")
