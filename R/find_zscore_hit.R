#' Title Find Z-score Hit
#'
#' @param table_treate_vs_control table computet with the function
#'                                \code{compute_data_table}
#' @param number_barcode Number of barcode to have under the median
#'
#' @return return a tibble containing the hit for the Z-score
#' @export
#'
#' @examples
find_zscore_hit <- function(table_treate_vs_control, number_barcode){
  hit_table <-
    table_treate_vs_control %>%
    dplyr::filter(Zscore < median(Zscore)) %>%
    dplyr::group_by(Gene) %>%
    dplyr::summarise(numberOfBarcode = n()) %>%
    dplyr::group_by(numberOfBarcode) %>%
    dplyr::filter(numberOfBarcode > number_barcode) %>%
    column_to_rownames(., var = "Gene")

  return(hit_table)
}
