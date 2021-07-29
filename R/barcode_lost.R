#' @title Count Barcode Lost
#' @description This fuction count the number of Barcode lost during the
#'              sequenceing. A barcode is lost if it has zero mapped read
#'
#'
#'
#'
#' @param screenR_object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @importFrom magrittr %>%
#' @return return a tibble containing the number of barcode lost for sample
#' @export

barcode_lost <- function(screenR_Object){
  table <- count_mapped_reads(screenR_Object)
  table <- table %>%
    group_by(Sample) %>%
    filter(Mapped == 0) %>%
    summarise(LostBarcode = n())

  return(table)
}
