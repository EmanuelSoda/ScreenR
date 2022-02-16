#' Title Find robust Z-score Hit
#'
#' @param table_treate_vs_control table computet with the function
#'                                \code{compute_data_table}
#' @param number_barcode Number of barcode to have under the median
#'
#' @return return a tibble containing the hit for the robust Z-score
#' @export
#' @examples
#' obj <- get0("obj", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(obj, control = "Met", treatment = "DMSO",
#'                          day = "Day3")
#' find_robust_zscore_hit(table, number_barcode = 6)
#'
find_robust_zscore_hit <- function(table_treate_vs_control, number_barcode) {
    hit_table <- table_treate_vs_control %>%
        dplyr::filter(.data$ZscoreRobust < median(.data$ZscoreRobust)) %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(numberOfBarcode = n()) %>%
        dplyr::group_by(.data$numberOfBarcode) %>%
        dplyr::filter(.data$numberOfBarcode > number_barcode)

    return(hit_table)
}
