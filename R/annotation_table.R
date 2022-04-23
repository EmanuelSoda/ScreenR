#' Table for the annotation of Barcode
#' @docType data
#' @usage data(annotation_table)
#' @keywords datasets
#' @concept data
#'
#' @format A data frame with 5320 rows and 2 columns:
#' \describe{
#'   \item{Gene}{It Contains the gene name}
#'   \item{Barcode}{It contains an ID that identify each barcode, it can be use
#'   to merge the annotation table with the count table}
#'
#'   \item{Gene_ID}{It Contains a unique Gene ID}
#'
#'   \item{Sequence}{It contains the cDNA sequence of the Barcode}
#'
#'   \item{Library}{It contains the library from which the Barcode comes from}
#' }
#'
"annotation_table"
