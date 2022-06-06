#' Table of the count table
#' @docType data
#' @usage data(count_table)
#' @keywords datasets
#' @concept  data
#'
#' @format A data frame with 5323 rows and 15 variables obtained from
#'         barcode alignment to the reference library.
#'         It is generated from a [Cellecta](https://cellecta.com/) protocol.
#'         The samples generated are then sequenced using an RNA-seq protocol.
#'         Due to the fact that different shRNAs are sequenced for a gene each
#'         barcode has its associated reads. This reads were aligned to the
#'         reference library using [bowtie2](http://bowtie-bio.sourceforge.net/bowtie2/index.shtml)
#'         and then sorted with [samtools](https://github.com/samtools/samtools).
#'         Since this dataset comes from a Chemical Synthetic Lethality
#'         experiments the samples treated and combined with the shRNAs
#'         knockdown should present a decreased number of reads compared to
#'         the controls.
#'
#' \describe{
#'   \item{Barcode}{It contains an ID that identify each barcode. It can be use
#'   to marge the annotation table with the count table. A Barcode is a unique
#'   identifier of an shRNA. In a genetic screening multiple slightly different
#'   shRNAs perform a knockout of a gene each with its efficacy. For this reason
#'   it is important to keep track of each shRNA using a unique barcode.}
#'
#'  \item{Time_1}{It contains the counts at time zero.
#'                This is the first time point at which cells are not treated
#'                and not infected.}
#'
#'   \item{Time_2}{It contains the counts after the cell were washed.
#'                At this time point the cells are  infected and following the
#'                Cellecta protocol are washed with the puromycin.}
#'
#'   \item{Time_3_TRT_rep1}{It contains the counts for the first replicate
#'                 of the treated at the first time point.
#'                 Usually the first time  point is 7 day after the puromycin
#'                 wash.}
#'
#'   \item{Time_3_TRT_rep2}{It contains the counts for the second replicate
#'                 of the treated at the first time point. Usually the first
#'                 time point is 7 day after the puromycin wash.}
#'
#'   \item{Time_3_TRT_rep3}{It contains the counts for the third replicate
#'                 of the treated at the first time point. Usually the first
#'                 time point is 7 day after the puromycin wash.}
#'
#'   \item{Time_3_rep1}{It contains the counts for the first replicate of the
#'                 control at the first time point. Usually the first
#'                 time point is 7 day after the puromycin wash.}
#'
#'   \item{Time_3_rep2}{It contains the counts for the second replicate of the
#'                 control at the first time point. Usually the first
#'                 time point is 7 day after the puromycin wash.}
#'
#'   \item{Time_3_rep3}{It contains the counts for the third replicate of the
#'                 control at the first time point. Usually the first
#'                 time point is 7 day after the puromycin  wash.}
#'
#'   \item{Time_4_TRT_rep1}{It contains the counts for the first replicate
#'                 of the treated at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#'
#'   \item{Time_4_TRT_rep2}{It contains the counts for the second replicate
#'                 of the treated at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#'
#'   \item{Time_4_TRT_rep3}{It contains the counts for the third replicate
#'                 of the treated at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#'
#'   \item{Time_4_rep1}{It contains the counts for the first replicate of the
#'                 control at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#'
#'   \item{Time_4_rep2}{It contains the counts for the second replicate of the
#'                 control at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#'
#'   \item{Time_4_rep3}{It contains the counts for the third replicate of the
#'                 control at the second time point. Usually the first
#'                 time point is 14 day after the puromycin wash.}
#' }
#'
"count_table"
