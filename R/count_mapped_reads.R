#' @title Count the number of mapped read
#' @description This function counts the number of reads for Barcode
#'              in each sample
#'
#'
#'
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#'
#' @return return a tibble containing the number of mapped read for sample

count_mapped_reads <- function(screenR_Object){
  # Get only the numeric column (so the sample)
  numericColumn <-
    screenR_Object@count_table %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()

  table <- screenR_Object@count_table %>%
    tidyr::gather(Sample, Mapped, numericColumn) %>%
    dplyr::mutate(Sample = factor(Sample, levels = numericColumn))
  return(table)
}
