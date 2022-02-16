#' @title Count the number of mapped read
#' @description This function counts the number of reads for Barcode
#'              in each sample
#'
#'
#'
#' @importFrom rlang .data
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#'
#' @return return a tibble containing the number of mapped read for sample
#' @export
#' @examples
#' obj <- get0("obj", envir = asNamespace("ScreenR"))
#' count_mapped_reads(obj)
#'

count_mapped_reads <- function(screenR_Object) {
    # Get only the numeric column (so the sample)
    numericColumn <- screenR_Object@count_table %>%
        dplyr::select_if(is.numeric) %>%
        colnames()

    table <- screenR_Object@count_table %>%
        tidyr::gather("Sample", "Mapped", all_of(numericColumn)) %>%
        dplyr::mutate(Sample = factor(.data$Sample, levels = numericColumn))
    return(table)
}
