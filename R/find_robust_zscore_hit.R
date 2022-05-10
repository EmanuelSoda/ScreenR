#' Title Find robust Z-score Hit
#' @param table_treate_vs_control A table computed with the function
#'                                \code{compute_data_table}.
#'                                It contain for each barcode the associated
#'                                Gene the counts in the treated and control
#'                                and the value for the Log2FC, Zscore,
#'                                ZscoreRobust in each day.
#' @param number_barcode Number of barcode that as to be differentially
#'                       expressed (DE)in order to consider the gene associated
#'                       DE. Example a gene is associated
#'                       with 10 shRNA we consider a gene DE if it has at least
#'                       number_barcode = 5 shRNA DE.
#' @importFrom dplyr n
#' @return Return a tibble containing the hit for the robust Z-score
#' @export
#' @concept find
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3",
#'     day = "Time3"
#' )
#' result <- find_robust_zscore_hit(table, number_barcode = 6)
#' head(result)
find_robust_zscore_hit <- function(table_treate_vs_control, number_barcode) {
    hit_table <- table_treate_vs_control %>%
        dplyr::filter(.data$ZscoreRobust < median(.data$ZscoreRobust)) %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(numberOfBarcode = n()) %>%
        dplyr::group_by(.data$numberOfBarcode) %>%
        dplyr::filter(.data$numberOfBarcode > number_barcode)

    return(hit_table)
}
