#' Table of the count table
#' @docType data
#' @usage data(count_table)
#' @keywords datasets
#' @concept  data
#'
#' @format A data frame with 5323 rows and 15 variables obtained from
#'         barcode alignment to the reference genome/library.
#' \describe{
#'   \item{Barcode}{It contains an ID that identify each barcode. It can be use
#'   to marge the annotation table with the count table. A Barcode is a unique
#'   identifier of an shRNA. In a genetic screening multiple slightly different
#'   shRNAs perform a knockout a gene each with its efficacy. For this reason
#'   it is importat to keep track of each shRNA using a unique barcode.}
#'
#'  \item{Time_1}{It contains the counts at time zero}
#'
#'   \item{Time_2}{It contains the counts after the cell were washed}
#'   \item{Time_3_TRT_rep1}{It contains the counts for the first replicate
#'                 of the treated at the first time point}
#'
#'   \item{Time_3_TRT_rep2}{It contains the counts for the second replicate
#'                 of the treated at the first time point}
#'
#'   \item{Time_3_TRT_rep3}{It contains the counts for the third replicate
#'                 of the treated at the first time point}
#'
#'   \item{Time_3_rep1}{It contains the counts for the first replicate of the
#'                 control at the first time point}
#'
#'   \item{Time_3_rep2}{It contains the counts for the second replicate of the
#'                 control at the first time point}
#'
#'   \item{Time_3_rep3}{It contains the counts for the third replicate of the
#'                 control at the first time point}
#'
#'   \item{Time_4_TRT_rep1}{It contains the counts for the first replicate
#'                 of the treated at the second time point}
#'
#'   \item{Time_4_TRT_rep2}{It contains the counts for the second replicate
#'                 of the treated at the second time point}
#'
#'   \item{Time_4_TRT_rep3}{It contains the counts for the third replicate
#'                 of the treated at the second time point}
#'
#'   \item{Time_4_rep1}{It contains the counts for the first replicate of the
#'                 control at the second time point}
#'
#'   \item{Time_4_rep2}{It contains the counts for the second replicate of the
#'                 control at the second time point}
#'
#'   \item{Time_4_rep3}{It contains the counts for the third replicate of the
#'                 control at the second time point}
#' }
#'
"count_table"
