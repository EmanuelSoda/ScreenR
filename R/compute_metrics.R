#' @title Compute Metrics
#' @description This function computes the  metrics that will be then used
#'              to compute the Z-score using the function
#'              \code{\link{find_zscore_hit}} starting from the screenR object
#'              for a given treatment in a given day
#'

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @param control The control Samples.
#'
#' @param treatment The treatment Samples
#'
#' @param day The day of the treatment

#' @importFrom rlang .data
#' @importFrom tidyr spread pivot_wider
#' @importFrom dplyr mutate filter summarise if_else
#' @importFrom stats sd median
#' @return Return a tibble  with all the measure computed.
#' @export
#' @examples
#' object <- get0('object', envir = asNamespace('ScreenR'))
#' metrics <- compute_metrics(object, control = 'TRT',
#'                           treatment = 'Time3', day = 'Time3')
#' head(metrics)

compute_metrics <- function(screenR_Object, control,
    treatment, day) {

    control <- screenR_Object@data_table %>%
        filter(.data$Treatment %in% control) %>%
        pull(.data$Sample) %>%
        unique() %>%
        as.character()

    treated <- screenR_Object@data_table %>%
        filter(.data$Treatment %in% treatment) %>%
        pull(.data$Sample) %>%
        unique() %>%
        as.character()

    data_trt <- screenR_Object@data_table %>%
        filter(.data$Sample %in% c(control, treated)) %>%
        mutate(Group = factor(if_else(condition = .data$Sample %in%
            treated, true = "Treated", false = "Control"),
            levels = c("Treated", "Control")))

    data_trt <- data_trt %>%
        dplyr::filter(.data$Day %in% day) %>%
        dplyr::group_by(.data$Group, .data$Barcode) %>%
        dplyr::mutate(Mean = mean(.data$Frequency)) %>%
        dplyr::summarise(Barcode = unique(.data$Barcode),
            Mean = unique(.data$Mean), Gene = unique(.data$Gene),
            .groups = "drop") %>%
        tidyr::pivot_wider(names_from = .data$Group,
            values_from = .data$Mean) %>%
        dplyr::filter(.data$Control > 0) %>%
        dplyr::mutate(Log2FC = log2(.data$Treated/.data$Control +
            1e-07)) %>%
        dplyr::mutate(Zscore = (.data$Log2FC -
            mean(.data$Log2FC))/sd(.data$Log2FC)) %>%
        dplyr::mutate(ZscoreRobust = 1.4826 * (.data$Log2FC -
            mean(.data$Log2FC))/median(abs(.data$Log2FC -
            median(.data$Log2FC)))) %>%
        dplyr::mutate(Day = day, Treatment = treatment)

    return(data_trt)
}
