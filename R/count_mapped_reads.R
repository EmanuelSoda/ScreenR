#' @title Count the number of mapped read
#' @description This function counts the number of reads for each barcode
#'              in each sample. It is a quality control function (QC) to see if
#'              the biological protocol went as planned.
#'              If a sample has very low mapped compared to the other means
#'              that is has a lower quality.
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#'
#' @return Return a tibble containing the number of mapped read for sample
#' @export
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' head(count_mapped_reads(object))
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
