#' @title Mapped Reads
#' @description This function returns the number of mapped reads inside the
#'              screenR object
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @importFrom rlang .data
#' @importFrom dplyr select
#' @return Return a tibble containing the number of mapped read for sample
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' mapped_reads(object)
#'
mapped_reads <- function(screenR_Object) {
    table <- count_mapped_reads(screenR_Object)

    table <- table %>%
        dplyr::select(.data$Sample, .data$Mapped) %>%
        dplyr::group_by(.data$Sample) %>%
        dplyr::mutate(Mapped = sum(.data$Mapped)) %>%
        summarise(Sample = unique(.data$Sample), Mapped = unique(.data$Mapped))

    return(table)
}
