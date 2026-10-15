#' @title Example annotation table
#' @description Annotation of the barcodes of \code{\link{count_table}}, an
#'     example loss-of-function shRNA screen. Each row describes one shRNA
#'     (barcode) and the gene it targets. Use it together with
#'     \code{\link{count_table}} to create a ScreenR object with
#'     \code{\link{create_screenr_object}}.
#' @docType data
#' @usage data(annotation_table)
#' @keywords datasets
#' @concept data
#' @format A data frame with 5320 rows (barcodes) and 5 columns:
#' \describe{
#'   \item{Gene}{The name of the gene targeted by the shRNA.}
#'   \item{Barcode}{The barcode identifier, unique for each shRNA. It links
#'                  the annotation table to the count table.}
#'   \item{Gene_ID}{A gene identifier (NA in this example).}
#'   \item{Sequence}{The sequence of the shRNA (NA in this example).}
#'   \item{Library}{The library of the shRNA, a pooled library from
#'                  \href{https://cellecta.com/}{Cellecta} (NA in this
#'                  example).}
#' }
#' @seealso \code{\link{count_table}}, \code{\link{create_screenr_object}}
#' @examples
#' data(annotation_table)
#' head(annotation_table)
"annotation_table"
