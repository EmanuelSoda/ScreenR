#' Title Find Z-score Hit
#' @param table_treate_vs_control table computed with the function
#'                                \code{compute_data_table}
#' @param number_barcode Number of barcode to have under the median or the mean
#' @param metric A string containing the metric too use
#' @return return a tibble containing the hit for the Z-score
#' @export
#' @examples
#' obj <- get0("obj", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(obj, control = "Met", treatment = "DMSO",
#'                          day = "Day3")
#'
#' # For the the median
#' find_zscore_hit(table, number_barcode = 6)
#'
#' # For the mean
#' find_zscore_hit(table, number_barcode = 6, metric = "mean")

find_zscore_hit <- function(table_treate_vs_control, number_barcode = 6,
    metric = "median") {
    if (metric == "median") {
        hit_table <- table_treate_vs_control %>%
            # Filtering of the gene that have the Z-score under the median of
            # the Z-scores
        dplyr::filter(.data$Zscore < median(.data$Zscore)) %>%
            dplyr::group_by(.data$Gene) %>%
            dplyr::summarise(numberOfBarcode = n()) %>%
            # Take only the gene that have 6 barcode under the median of the
            # Z-scores
        dplyr::filter(.data$numberOfBarcode > number_barcode)
    } else if (metric == "mean") {
        hit_table <- table_treate_vs_control %>%
            # Filtering of the gene that have the Z-score under the median of
            # the Z-scores
        dplyr::filter(.data$Zscore < mean(.data$Zscore)) %>%
            dplyr::group_by(.data$Gene) %>%
            dplyr::mutate(numberOfBarcode = n()) %>%
            dplyr::summarise(.data$numberOfBarcode) %>%
            # Take only the gene that have 6 barcode under the median of the
            # Z-scores
        dplyr::filter(.data$numberOfBarcode > number_barcode)
    }
    hit_table <- dplyr::ungroup(hit_table)
    return(hit_table)
}
