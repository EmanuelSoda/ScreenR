#' @title Example count table
#' @description Barcode counts of an example loss-of-function shRNA screen
#'     (a chemical synthetic lethality screen), with one row for each barcode
#'     and one column for each sample. The shRNAs of a pooled
#'     \href{https://cellecta.com/}{Cellecta} library were sequenced, the reads
#'     were aligned to the reference library with
#'     \href{http://bowtie-bio.sourceforge.net/bowtie2/index.shtml}{bowtie2}
#'     and sorted with \href{https://github.com/samtools/samtools}{samtools}.
#'     The barcodes of the shRNAs that knock down a gene needed to survive the
#'     treatment are expected to have fewer reads in the treated samples than
#'     in the controls.
#' @docType data
#' @usage data(count_table)
#' @keywords datasets
#' @concept data
#' @format A data frame with 5323 rows and 15 columns. Besides the 5320
#'     barcodes of \code{\link{annotation_table}}, it contains three rows
#'     with the barcode "*" (unmapped reads, all zero), which should be
#'     removed before the edgeR based analyses.
#' \describe{
#'   \item{Barcode}{The barcode identifier, unique for each shRNA. It links
#'                  the count table to the annotation table. Since several
#'                  shRNAs target each gene, each with its own efficacy, every
#'                  shRNA is tracked with its own barcode.}
#'   \item{Time_1}{The counts at the first time point, before the infection
#'                 and the treatment.}
#'   \item{Time_2}{The counts after the infection and the puromycin
#'                 selection (Cellecta protocol).}
#'   \item{Time_3_TRT_rep1, Time_3_TRT_rep2, Time_3_TRT_rep3}{The counts of
#'                 the three replicates of the treated samples at the first
#'                 time point after the selection (usually 7 days after the
#'                 puromycin wash).}
#'   \item{Time_3_rep1, Time_3_rep2, Time_3_rep3}{The counts of the three
#'                 replicates of the control samples at the same time point.}
#'   \item{Time_4_TRT_rep1, Time_4_TRT_rep2, Time_4_TRT_rep3}{The counts of
#'                 the three replicates of the treated samples at the second
#'                 time point (usually 14 days after the puromycin wash).}
#'   \item{Time_4_rep1, Time_4_rep2, Time_4_rep3}{The counts of the three
#'                 replicates of the control samples at the same time point.}
#' }
#' @seealso \code{\link{annotation_table}}, \code{\link{create_screenr_object}}
#' @examples
#' data(count_table)
#' head(count_table)
"count_table"
