#' Normalize data
#' @title Normalize data
#' @description This function perform a normalization on the data considering
#'              the fact that Barcode has a defined length so they will not
#'              influence the data. So, basically is computed the sum for
#'              each row and then multiply by 1e6. At the end we will obtain a
#'              sort of transcript per milion.
#'
#'
#'
#'
#' @param screenR_object The ScreenR object obtained using the
#'                       \code{\link{create_ScreenR_object}}
#'
#'
#' @return An object containing all the information for the analysis.
normalize_data <- function(screenR_Object) {
  library(tidyverse)
  screenR_Object$count_table %>%
    mutate_if(is.numeric, funs(./sum(.)*numberNorm))

}
