# Build the k562_* datasets of ScreenR: a subset of a published genome-wide
# shRNA essentiality screen in K562 cells, with reference essential and
# non-essential genes.
#
# Sources
# - shRNA counts: test data of casTLE (Morgens DW, Deans RM, Li A, Bassik MC.
#   Systematic comparison of CRISPR/Cas9 and RNAi screens for essential genes.
#   Nat Biotechnol 2016; 34:634-636. doi:10.1038/nbt.3567).
#   Downloaded from https://bitbucket.org/dmorgens/castle/downloads
#   (dmorgens-castle.tar.gz), files Data/Test/plasmid_shRNA_counts.csv,
#   Data/Test/T14_shRNA_repA_counts.csv, Data/Test/T14_shRNA_repB_counts.csv
#   and GenRef/Homo_sapiens.gene_info. casTLE is distributed under the
#   BSD-2-Clause license, Copyright (c) 2016, David W Morgens.
# - Reference genes: CEGv2.txt (core essential genes) and NEGv1.txt
#   (non-essential genes) from https://github.com/hart-lab/bagel (MIT
#   license, Copyright (c) 2020 Hart Lab). Hart T et al. Mol Syst Biol 2014;
#   10:733 and Hart T et al. G3 2017; 7:2719-2727.
#
# Usage (from the package source directory):
# Rscript inst/scripts/k562_essential_screen.R <castle dir> <bagel dir>
args <- commandArgs(TRUE)
castle <- args[1]
bagel <- args[2]

read_counts <- function(file) {
    counts <- read.delim(file, header = FALSE,
        colClasses = c("character", "integer"))
    stats::setNames(counts, c("Barcode", "Count"))
}
plasmid <- read_counts(file.path(castle, "Data/Test/plasmid_shRNA_counts.csv"))
t14_a <- read_counts(file.path(castle, "Data/Test/T14_shRNA_repA_counts.csv"))
t14_b <- read_counts(file.path(castle, "Data/Test/T14_shRNA_repB_counts.csv"))

barcodes <- sort(Reduce(union, list(plasmid$Barcode, t14_a$Barcode,
    t14_b$Barcode)))
count_of <- function(table) {
    x <- table$Count[match(barcodes, table$Barcode)]
    x[is.na(x)] <- 0L
    x
}
counts <- data.frame(
    Barcode = barcodes,
    Plasmid = count_of(plasmid),
    T14_A = count_of(t14_a),
    T14_B = count_of(t14_b)
)
# The barcode identifier starts with the NCBI (Entrez) gene ID; 0 marks the
# non-targeting controls
gene_id <- sub("_.*", "", barcodes)

ceg <- read.delim(file.path(bagel, "CEGv2.txt"))
neg <- read.delim(file.path(bagel, "NEGv1.txt"))
# NCBI gene_info: the first line is a header in another format, column 2 is
# the GeneID and column 3 the Symbol
gene_info <- read.delim(file.path(castle, "GenRef/Homo_sapiens.gene_info"),
    header = FALSE, skip = 1, quote = "", comment.char = "",
    colClasses = "character")
symbol <- gene_info[[3]][match(gene_id, gene_info[[2]])]

# Subset: all the reference genes, 1000 other genes and 1000 non-targeting
# controls, to keep the package small
set.seed(2016)
essential <- intersect(as.character(ceg$ENTREZ_ID), gene_id)
non_essential <- intersect(as.character(neg$ENTREZ_ID), gene_id)
others <- setdiff(unique(gene_id[gene_id != "0"]), c(essential,
    non_essential))
others <- sample(others, 1000)
controls <- sample(barcodes[gene_id == "0"], 1000)
keep <- gene_id %in% c(essential, non_essential, others) |
    barcodes %in% controls

class <- ifelse(gene_id %in% essential, "core essential",
    ifelse(gene_id %in% non_essential, "non-essential",
        ifelse(gene_id == "0", "non-targeting", "other")))
gene <- ifelse(gene_id == "0", "non-targeting",
    ifelse(is.na(symbol), paste0("GeneID_", gene_id), symbol))

k562_count_table <- counts[keep, ]
rownames(k562_count_table) <- NULL
k562_annotation_table <- data.frame(
    Barcode = barcodes[keep],
    Gene = gene[keep],
    Gene_ID = ifelse(gene_id[keep] == "0", NA_character_, gene_id[keep]),
    Class = class[keep]
)
k562_reference_genes <- unique(data.frame(
    Gene = gene, Gene_ID = gene_id, Class = class
)[keep & class %in% c("core essential", "non-essential"), ])
rownames(k562_reference_genes) <- NULL

save(k562_count_table, file = "data/k562_count_table.rda",
    compress = "xz")
save(k562_annotation_table, file = "data/k562_annotation_table.rda",
    compress = "xz")
save(k562_reference_genes, file = "data/k562_reference_genes.rda",
    compress = "xz")
