# Build the hart_reference_genes dataset: the core essential genes (CEGv2)
# and non-essential genes (NEGv1) of Hart et al., from the BAGEL2 repository
# (https://github.com/hart-lab/bagel, MIT license, see
# inst/LICENSES_data.txt).
# Usage: Rscript inst/scripts/hart_reference_genes.R <bagel dir>
args <- commandArgs(TRUE)
bagel <- args[1]
ceg <- read.delim(file.path(bagel, "CEGv2.txt"))
neg <- read.delim(file.path(bagel, "NEGv1.txt"))
hart_reference_genes <- rbind(
    data.frame(Gene = ceg$GENE, HGNC_ID = ceg$HGNC_ID,
        Entrez_ID = as.integer(ceg$ENTREZ_ID), Class = "core essential"),
    data.frame(Gene = neg$GENE, HGNC_ID = neg$HGNC_ID,
        Entrez_ID = as.integer(neg$ENTREZ_ID), Class = "non-essential")
)
# NEGv1.txt lists SLC34A1 twice
hart_reference_genes <- hart_reference_genes[
    !duplicated(hart_reference_genes$Gene), ]
rownames(hart_reference_genes) <- NULL
save(hart_reference_genes, file = "data/hart_reference_genes.rda",
    compress = "xz")
