#' @title Compute Metrics
#' @description This function computes the metrics that will be then used
#'              to compute the z-score using the function
#'              \code{\link{find_zscore_hit}} starting from the screenr object
#'              for a given treatment in a given day. More information about the
#'              z-score and other metrics used in genetic screening can be found
#'              at this paper
#'              \href{https://pubmed.ncbi.nlm.nih.gov/21515799/}{z-score}
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param control A string specifying the sample that as to be used as
#'                control in the analysis.
#'                This string has to be equal to the interested sample in the
#'                Treatment column of the data_table slot
#' @param treatment A string specifying the sample that as to be used as
#'                  treatment in the analysis.
#'                  This string has to be equal to the interested sample in the
#'                  Treatment column of the data_table slot.
#' @param day A string containing the day (time point) to consider in the
#'            metrics computation.
#'            This string has to be equal to the interested sample in the
#'            Day column of the data_table slot.
#' @importFrom rlang .data
#' @importFrom tidyr spread pivot_wider
#' @importFrom dplyr mutate filter summarise if_else pull
#' @importFrom stats sd median
#' @return Return a tibble with all the measure computed.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' metrics <- compute_metrics(object,
#'     control = "TRT",
#'     treatment = "Time3", day = "Time3"
#' )
#' head(metrics)
compute_metrics <- function(screenR_Object, control,
    treatment, day) {
    control <- screenR_Object@data_table %>%
        filter(.data$Treatment %in% control) %>%
        dplyr::pull(.data$Sample) %>%
        unique() %>%
        as.character()

    treated <- screenR_Object@data_table %>%
        filter(.data$Treatment %in% treatment) %>%
        dplyr::pull(.data$Sample) %>%
        unique() %>%
        as.character()

    data_trt <- screenR_Object@data_table %>%
        dplyr::filter(.data$Sample %in% c(control, treated)) %>%
        dplyr::mutate(Group = factor(if_else(condition = .data$Sample %in%
            treated, true = "Treated", false = "Control"),
        levels = c("Treated", "Control")
        ))

    data_trt <- data_trt %>%
        dplyr::filter(.data$Day %in% day) %>%
        dplyr::group_by(.data$Group, .data$Barcode) %>%
        dplyr::mutate(Mean = mean(.data$Frequency)) %>%
        dplyr::summarise(
            Barcode = unique(.data$Barcode),
            Mean = unique(.data$Mean), Gene = unique(.data$Gene),
            .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
            names_from = .data$Group,
            values_from = .data$Mean
        ) %>%
        dplyr::filter(.data$Control > 0) %>%
        dplyr::mutate(Log2FC = log2(.data$Treated / .data$Control +
            1e-07)) %>%
        dplyr::mutate(Zscore = (.data$Log2FC -
            mean(.data$Log2FC)) / sd(.data$Log2FC)) %>%
        dplyr::mutate(ZscoreRobust = 1.4826 * (.data$Log2FC -
            mean(.data$Log2FC)) / median(abs(.data$Log2FC -
            median(.data$Log2FC)))) %>%
        dplyr::mutate(Day = day, Treatment = treatment)

    return(data_trt)
}
