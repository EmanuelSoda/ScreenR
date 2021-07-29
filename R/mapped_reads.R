#' @title Mapped Reads
#' @description This function perform plot the number of reads mapped for each
#'              sample.
#'
#'
#'
#'
#' @param screenR_object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @importFrom magrittr %>%
#' @return return a tibble containing the number of mapped read for sample
#' @export


mapped_reads <- function(screenR_Object){
  # Get only the numeric column (so the sample)
  numericColumn <-
    screenR_Object@count_table %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()


  table <- screenR_Object@count_table %>%
    tidyr::gather(Sample, Mapped, numericColumn) %>%
    dplyr::select(Sample, Mapped) %>%
    dplyr::group_by(Sample) %>%
    dplyr::mutate(Mapped = sum(Mapped))
  return(table)
}
