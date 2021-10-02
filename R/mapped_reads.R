#' @title Mapped Reads
#' @description This function perform plot the number of reads mapped for each
#'              sample.
#'
#'
#'
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @importFrom rlang .data
#'
#' @return return a tibble containing the number of mapped read for sample
#' @export


mapped_reads <- function(screenR_Object){
  table <- count_mapped_reads(screenR_Object)

  table <- table %>%
    dplyr::select(.data$Sample, .data$Mapped) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Mapped = sum(.data$Mapped))

  return(table)
}
