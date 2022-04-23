#' @title Normalize data
#' @description This function perform a normalization on the data considering
#'              the fact that Barcodes have a defined length so this will not
#'              influence the data. Basically is computed the sum for
#'              each row and then multiply by 1e6. At the end we will obtain a
#'              sort of transcript per million.

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom purrr map_if
#' @return Return the screenR object with the normalize data
#' @export
#' @concept  compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- normalize_data(object)
#'
#' slot(object, "normalized_count_table")
normalize_data <- function(screenR_Object) {
    screenR_Object@normalized_count_table <- screenR_Object@count_table %>%
        # it divedes each cell for the sum of the column and than multiply 1e6
        purrr::map_if(is.numeric, ~ . / sum(.) * 1e+06) %>%
        dplyr::as_tibble()

    return(screenR_Object)
}
