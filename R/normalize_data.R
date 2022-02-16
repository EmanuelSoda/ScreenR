#' @title Normalize data
#' @description This function perform a normalization on the data considering
#'              the fact that Barcode has a defined length so they will not
#'              influence the data. So, basically is computed the sum for
#'              each row and then multiply by 1e6. At the end we will obtain a
#'              sort of transcript per million.

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @importFrom magrittr %>%
#' @return return the screenR object with the normalize data
#' @export
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#' normalize_data(obj)
normalize_data <- function(screenR_Object) {
    screenR_Object@normalized_count_table <- screenR_Object@count_table %>%
        # divede each cell for the sum of the column and than multiply 1e6
    purrr::map_if(is.numeric, ~./sum(.) * 1e+06) %>%
        dplyr::as_tibble()

    return(screenR_Object)
}
