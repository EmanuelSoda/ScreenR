#' @title Filter using the slope filter
#' @description This function is used to improve the quality of the hits.
#'              It computes a regression line in the different samples ad uses
#'              the slope of this line to see the trend
#' @importFrom stats lm
#' @param screenR_Object The ScreenR object obtained using the
#'                       create_screenR_object
#'
#' @param genes The genes for which the slope as to be computed. Those genes are
#'              the result of the three statistical methods selection
#'
#' @param slope_treatment The treatment slope
#' @param slope_control The control slope
#' @param group_var_treatment The variable to use as X for the linear model
#'                            for the Treatment
#' @param group_var_control The variable to use as X for the linear model
#'                            for the the control
#' @return A data frame with the slope for the treatment and the control
#'         for each gene
#' @export

filter_by_slope <- function(screenR_Object, genes, group_var_treatment,
                            group_var_control, slope_control = NULL, slope_treatment) {

  # Compute the slope of the hits in the treatment Samples
  slope_treatment <- compute_slope(screenR_Object, genes, group_var_treatment)

  # Compute the slope of the hits in the control Samples
  slope_DMSO <- compute_slope(screenR_Object, genes, group_var_control)

  data <- screenR_Object@data_table

  data <- dplyr::left_join(data, slope_treatment, by = "Gene")
  data <- dplyr::rename(data, slope_treatment = .data$Slope)

  data <- dplyr::left_join(data, slope_DMSO, by = "Gene")
  data <- dplyr::rename(data, slope_control = .data$Slope)

  if (!is.null(slope_control)) {
    data <- dplyr::filter(data, .data$slope_control <= slope_control)
  }

  data <- dplyr::filter(data, .data$slope_treatment <= slope_treatment)

  return(data)
}

#' @title Compute Slope for Gene
#' @description This function is used to compute the slope of the genes passed
#'              as input
#' @importFrom rlang .data
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       create_screenR_object
#' @param genes The genes for which the slope as to be computed. Those genes are
#'              the result of the three statistical methods selection
#' @param group_var The variable to use as X for the linear model
#' @return A tibble containing in each row the gene and the corresponding Slope
#' @export

compute_slope <- function(screenR_Object, genes, group_var) {
  data <- screenR_Object@data_table
  data <- dplyr::filter(data, .data$Gene %in% genes)
  data <- dplyr::filter(data, .data$Treatment %in% group_var)

  # in order to perform the linear regression as samples are a
  # time series the ordered Sample are encoded as number
  data <- dplyr::group_by(data, .data$Gene)
  data <- dplyr::mutate(data, encode = 1:n())
  data <- dplyr::ungroup(data)

  data <- dplyr::mutate(dplyr::nest_by(data, .data$Gene),
                        Slope = lm(Frequency ~ encode, data = data)$coefficients["encode"])
  slope_tibble <- select(data, .data$Gene, .data$Slope)

  return(slope_tibble)
}


#' @title Filter using the variance filter
#' @description This function is used to improve the quality of the hits.
#'              It compute the variance of the hits and filter the one with a
#'              value greater than the treashold set
#' @param screenR_Object The ScreenR object obtained using the
#'                       create_screenR_object
#'
#' @param genes The genes for which the slope as to be computed. Those genes are
#'              the result of the three statistical methods selection
#'
#' @param variance The maximum value of variance accepted
#' @param contrast The variable to use as X for the linear model
#'                            for the Treatment
#' @param matrix_model a matrix created using model.matrix
#' @return A data frame with the variance for the treatment and the control
#'         for each gene
#' @export

filter_by_variance <- function(screenR_Object,
                               genes,
                               matrix_model,
                               variance = 0.5,
                               contrast) {


  # Save the data_table in a temporary variable
  data <- screenR_Object@data_table

  # Create the edgeR object and computing the LogFC
  DGEList <- create_edgeR_obj(screenR_Object)
  xglm <- edgeR::estimateDisp(DGEList, matrix_model)
  fit <- edgeR::glmFit(xglm, matrix_model)
  lrt <- edgeR::glmLRT(fit, contrast = contrast)

  lrt <- data.frame(lrt$table)
  lrt$Gene <- fit$genes$Gene
  lrt <- dplyr::filter(lrt, .data$Gene %in% genes)
  lrt <- dplyr::group_by(lrt, .data$Gene)
  lrt <- dplyr::mutate(lrt, variance = stats::var(.data$logFC))
  lrt <- dplyr::summarise(lrt, Gene = unique(.data$Gene),
                          variance = mean(.data$variance),
                          .groups = "drop")

  # Bind the temporary data_table  to the table with the fold change
  data <-
    dplyr::left_join(data, lrt, by="Gene")

  data <- dplyr::filter(data, .data$variance <= variance)
  data <- dplyr::rename(data, Variance = .data$variance)

  return(data)

}

