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
#' @param metric A string containing the metric to use. The value allowed are
#'               "median" or "mean".
#' @param group_var The name of the column which contain the grouping variable
#'                  usually the Gene column.
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
#'
#' # For the the median
#' result <- find_robust_zscore_hit(table, number_barcode = 6)
#' head(result)
#'
#' # For the mean
#' result <- find_robust_zscore_hit(table, number_barcode = 6, metric = "mean")
#' head(result)
find_robust_zscore_hit <- function(table_treate_vs_control,
                                   number_barcode = 6,
                                   metric = "median",
                                   group_var = "Gene") {

    if (sum(colnames(table_treate_vs_control) == group_var) == 0 ) {
        message("The group_var specified is not a column of the dataset")
    }
    else {
        hit_table <- table_treate_vs_control
        if (metric == "median") {
            hit_table <- hit_table %>%
                # Filtering of the gene that have the robust Z-score under
                # the median of the robust Z-scores
                dplyr::filter(.data$ZscoreRobust < median(.data$ZscoreRobust))
        } else if (metric == "mean") {
            hit_table <- hit_table %>%
                # Filtering of the gene that have the robust Z-score under
                # the mean of the robust Z-scores
                dplyr::filter(.data$ZscoreRobust < mean(.data$ZscoreRobust))
        }
        hit_table <-
            hit_table %>%
            dplyr::group_by(.data[[group_var]]) %>%
            dplyr::summarise(numberOfBarcode = n()) %>%
            dplyr::filter(.data$numberOfBarcode > number_barcode)

    return(hit_table)
    }
}
