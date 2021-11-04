#' @title Create the Z-score table
#' @description This function compute the  Z-score tablestarting from
#' the screenR object for a given treatment in a given day
#'

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @param control
#'
#' @param treatment
#'
#' @param day

#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @importFrom dplyr mutate filter summarise
#' @importFrom dplyr mutate
#' @importFrom stats sd median
#'
#' @return return a tibble  with all the measure computed
#' @export

compute_metrics <- function(screenR_Object, control, treatment, day){

  control <- screenR_Object@data_table %>%
    filter(Treatment %in% control) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()

  treated <- screenR_Object@data_table %>%
    filter(Treatment %in% treatment) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()

  data_trt <-
    screenR_Object@data_table %>%
    filter(Sample %in% c(control, treated)) %>%
    mutate(Group = as.factor(if_else(condition = Sample %in% treated,
                                     true = "Treated",
                                     false = "Control")))



  data_trt <- data_trt %>%
    filter(Day %in% day) %>%
    group_by(Group, Barcode) %>%
    mutate(Mean = mean(Frequency)) %>%
    summarise(Barcode = unique(Barcode), Mean = unique(Mean),
              Gene = unique(Gene), .groups = "drop") %>%
    spread(Group, Mean) %>%
    filter(Control > 0) %>%
    mutate(Log2FC = log2(Treated/Control + 1e-07))  %>%
    mutate(Zscore = (Log2FC - mean(Log2FC)) / sd(Log2FC))  %>%
    mutate(ZscoreRobust = 1.4826 * (Log2FC - mean(Log2FC)) /
             median(abs(Log2FC- median(Log2FC)))) %>%
    mutate(Day = day, Treatment = treatment) %>%


  return(data_trt)
}


