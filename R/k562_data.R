#' K562 shRNA essentiality screen: count table
#'
#' Read counts of a published genome-wide shRNA essentiality screen in K562
#' cells (Morgens et al. 2016), with reference essential and non-essential
#' genes (\code{\link{k562_reference_genes}}). In an essentiality screen the
#' cells are grown for some time after the infection with the library: the
#' barcodes of the genes needed for growth (essential genes) are depleted.
#' Since the essential genes are known, the dataset can be used to evaluate
#' how well a method finds the true hits.
#'
#' To keep the package small, the dataset is a subset of the original
#' library (about 500,000 shRNAs): all the shRNAs of the 681 core essential
#' and 904 non-essential reference genes, of 1000 other randomly chosen
#' genes and 1000 randomly chosen non-targeting controls. The script that
#' builds the subset is in the scripts folder of the installed package
#' (\code{system.file("scripts", package = "ScreenR")}); the licenses of
#' the original data are in
#' \code{system.file("LICENSES_data.txt", package = "ScreenR")}.
#'
#' The counts come from the test data of the casTLE software
#' (\url{https://bitbucket.org/dmorgens/castle}), distributed under the
#' BSD-2-Clause license, Copyright (c) 2016, David W Morgens.
#'
#' @format A data frame with 67358 rows (shRNAs) and 4 columns:
#' \describe{
#'   \item{Barcode}{The shRNA identifier. It starts with the NCBI (Entrez)
#'                  gene ID of the targeted gene (0 for the non-targeting
#'                  controls).}
#'   \item{Plasmid}{The counts in the plasmid library, before the screen.}
#'   \item{T14_A, T14_B}{The counts of the two replicates after 14 days of
#'                       growth.}
#' }
#' @source Morgens DW, Deans RM, Li A, Bassik MC (2016). Systematic
#'         comparison of CRISPR/Cas9 and RNAi screens for essential genes.
#'         Nat Biotechnol 34:634-636. \doi{10.1038/nbt.3567}
#' @seealso \code{\link{k562_annotation_table}},
#'          \code{\link{k562_reference_genes}}
#' @docType data
#' @usage data(k562_count_table)
#' @keywords datasets
#' @concept data
#' @examples
#' data(k562_count_table)
#' data(k562_annotation_table)
#' head(k562_count_table)
#'
#' # The sample names do not follow the Day_Treatment_Replicate convention,
#' # so the samples are described with a sample table
#' samples <- data.frame(
#'     Sample = c("Plasmid", "T14_A", "T14_B"),
#'     Day = "Screen",
#'     Treatment = c("Plasmid", "T14", "T14")
#' )
#' object <- create_screenr_object(
#'     table = k562_count_table, annotation = k562_annotation_table,
#'     groups = factor(c("Plasmid", "T14", "T14")),
#'     sample_table = samples
#' )
"k562_count_table"

#' K562 shRNA essentiality screen: annotation table
#'
#' Annotation of the shRNAs of \code{\link{k562_count_table}}.
#'
#' @format A data frame with 67358 rows (shRNAs) and 4 columns:
#' \describe{
#'   \item{Barcode}{The shRNA identifier, as in
#'                  \code{\link{k562_count_table}}.}
#'   \item{Gene}{The symbol of the targeted gene ("non-targeting" for the
#'               controls).}
#'   \item{Gene_ID}{The NCBI (Entrez) gene ID (NA for the controls).}
#'   \item{Class}{"core essential" or "non-essential" for the reference
#'                genes (see \code{\link{k562_reference_genes}}), "other"
#'                for the other genes and "non-targeting" for the controls.}
#' }
#' @source Morgens DW et al. (2016) Nat Biotechnol 34:634-636.
#'         \doi{10.1038/nbt.3567}. Gene symbols from the NCBI gene_info file
#'         distributed with casTLE.
#' @seealso \code{\link{k562_count_table}}
#' @docType data
#' @usage data(k562_annotation_table)
#' @keywords datasets
#' @concept data
#' @examples
#' data(k562_annotation_table)
#' table(k562_annotation_table$Class)
"k562_annotation_table"

#' K562 shRNA essentiality screen: reference essential and non-essential genes
#'
#' The core essential genes (CEGv2) and the non-essential genes (NEGv1)
#' defined by Hart et al. from many genome-wide screens, restricted to the
#' genes of \code{\link{k562_count_table}}. They are the standard reference
#' to evaluate essentiality screens: the core essential genes should be
#' depleted and the non-essential genes not.
#'
#' The lists come from the BAGEL repository
#' (\url{https://github.com/hart-lab/bagel}), distributed under the MIT
#' license, Copyright (c) 2020 Hart Lab.
#'
#' @format A data frame with 1585 rows (genes) and 3 columns:
#' \describe{
#'   \item{Gene}{The gene symbol.}
#'   \item{Gene_ID}{The NCBI (Entrez) gene ID.}
#'   \item{Class}{"core essential" (681 genes) or "non-essential"
#'                (904 genes).}
#' }
#' @source Hart T, Brown KR, Sircoulomb F, Rottapel R, Moffat J (2014).
#'         Measuring error rates in genomic perturbation screens: gold
#'         standards for human functional genomics. Mol Syst Biol 10:733.
#'         \doi{10.15252/msb.20145216}.
#'         Hart T, Tong AHY, Chan K, Van Leeuwen J, Seetharaman A, Aregger M,
#'         et al. (2017). Evaluation and design of genome-wide CRISPR/SpCas9
#'         knockout screens. G3 7:2719-2727. \doi{10.1534/g3.117.041277}
#' @seealso \code{\link{k562_count_table}}
#' @docType data
#' @usage data(k562_reference_genes)
#' @keywords datasets
#' @concept data
#' @examples
#' data(k562_reference_genes)
#' table(k562_reference_genes$Class)
"k562_reference_genes"
