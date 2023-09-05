#' @title Filter using the slope filter
#' @description This function is used to improve the quality of the hits found.
#'              It computes a regression line in the different samples ad uses
#'              the slope of this line to see the trend
#' @importFrom stats lm
#' @importFrom dplyr rename
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param genes The genes for which the slope as to be computed. Those genes
#'              are the result of the three statistical methods selection
#' @param slope_treatment A value used as threshold for the treatment slope
#' @param slope_control A value used as threshold for the control slope
#' @param group_var_treatment The variable to use as independent variable (x)
#'                            for the linear model of the treatment
#' @param group_var_control The variable to use as independent variable (x)
#'                          for the linear model of the the control
#' @return A data frame with the slope for the treatment and the control
#'         for each gene
#' @concept filter
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' filter_by_slope(
#'     screenR_Object = object, genes = c("Gene_1", "Gene_2"),
#'     group_var_treatment = c("T1", "T2", "TRT"),
#'     group_var_control = c("T1", "T2", "Time3", "Time4"),
#'     slope_control = 0.5, slope_treatment = 1
#' )
#'
filter_by_slope <- function(screenR_Object, genes, group_var_treatment,
    group_var_control, slope_control = NULL, slope_treatment = NULL) {

    # Compute the slope of the hits in the treatment Samples
    slope_trt <- ScreenR::compute_slope(screenR_Object, genes,
        group_var = group_var_treatment
    )

    
    # Compute the slope of the hits in the control Samples
    slope_contr <- ScreenR::compute_slope(screenR_Object, genes,
        group_var = group_var_control
    )

    data <- screenR_Object@data_table

    data <- dplyr::left_join(data, slope_trt, by = "Gene")
    data <- dplyr::rename(data, slope_treatment = .data$Slope)

    data <- dplyr::left_join(data, slope_contr, by = "Gene")
    data <- dplyr::rename(data, slope_control = .data$Slope)

    if(!is.null(slope_control)){
        print(unique(data$slope_control))
        data <- dplyr::filter(data, abs(.data$slope_control)  <=
                                  abs(slope_control))
    }
    
    if(!is.null(slope_treatment)){
        data <- dplyr::filter(data, abs(.data$slope_treatment) >=
                                  abs(slope_treatment))
    }

    # The treatment has more effect than the control 
    data <- dplyr::filter(data, abs(.data$slope_control) <= 
                              abs(.data$slope_treatment))
    
    data <- dplyr::distinct(data, .data$Gene, 
                            .data$slope_control, .data$slope_treatment)
    return(data)
}

#' @title Compute Slope of a Gene
#' @description This function is used to compute the slope of the gene passed
#'              as input
#' @importFrom rlang .data
#' @importFrom dplyr ungroup
#' @param group_var The variable to use  as independent variable (x)
#'                  for the linear model
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param genes The genes for which the slope as to be computed. Those genes
#'              are the result of the three statistical methods selection
#' @return A tibble containing in each row the gene and the corresponding Slope
#' @export
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_slope(object,
#'     genes = c("Gene_42", "Gene_24"),
#'     group_var = c("T1", "T2", "TRT")
#' )
compute_slope <- function(screenR_Object, genes, group_var) {
    data <- screenR_Object@data_table
    data <- dplyr::filter(data, .data$Gene %in% genes)
    data <- dplyr::filter(data, .data$Treatment %in% group_var)

    # in order to perform the linear regression as samples are a time series
    # the ordered Sample are encoded as number
    data <- dplyr::group_by(data, .data$Gene)
    data <- dplyr::mutate(data, encode = seq(1, n(), 1))
    data <- dplyr::ungroup(data)

    data <- dplyr::mutate(dplyr::nest_by(data, .data$Gene),
        Slope = lm(Frequency ~ encode, data = data)$coefficients["encode"]
    )
    slope_tibble <- select(data, .data$Gene, .data$Slope)

    return(slope_tibble)
}


#' @title Filter using the variance filter
#' @description This function is used to improve the quality of the hits.
#'              It compute the variance among the hits and filter the one with
#'              a value greater than the threshold set
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param genes The genes for which the variance as to be computed.
#'              Those genes are the result of the three statistical
#'              methods selection
#' @param variance The maximum value of variance accepted
#' @param contrast The variable to use as X for the linear model
#'                 for the Treatment
#' @param matrix_model a matrix created using \code{\link[stats]{model.matrix}}
#' @return A data frame with the variance for the treatment and the control
#'         for each gene
#' @export
#' @concept filter
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- model.matrix(~ slot(object, "groups"))
#' colnames(matrix_model) <- c("Control", "T1_T2", "Treated")
#' contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)
#'
#' data <- filter_by_variance(
#'     screenR_Object = object, genes = c("Gene_42"),
#'     matrix_model = matrix_model, contrast = contrast
#' )
#' head(data)
#'
filter_by_variance <- function(screenR_Object, genes, matrix_model,
    variance = 0.5, contrast) {
    # Save the data_table in a temporary variable
    data <- screenR_Object@data_table

    # Create the edgeR object and computing the LogFC
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)
    fit <- edgeR::glmFit(xglm, matrix_model)
    lrt <- edgeR::glmLRT(fit, contrast = contrast)

    lrt <- data.frame(lrt$table)
    lrt$Gene <- fit$genes$Gene
    lrt <- dplyr::filter(lrt, .data$Gene %in% genes)
    lrt <- dplyr::group_by(lrt, .data$Gene)
    lrt <- dplyr::mutate(lrt, variance = stats::var(.data$logFC))
    lrt <- dplyr::summarise(lrt,
        Gene = unique(.data$Gene),
        variance = mean(.data$variance), .groups = "drop"
    )

    # Bind the temporary data_table to the table with the fold change
    data <- dplyr::left_join(data, lrt, by = "Gene")

    data <- dplyr::filter(data, .data$variance <= variance)
    data <- dplyr::rename(data, Variance = .data$variance)
    return(data)
}



#' @title Remove rows that have zero count in all samples
#' @description This function removes the rows that have zero count in
#'              all samples. It takes care of updating both count_table and
#'              annotation_table.
#'              Very_Important: It has to be performed before the
#'                              data normalization.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @importFrom dplyr if_all
#' @importFrom tidyselect vars_select_helpers
#' @return The ScreenR object with the count_table and the annotation_table
#'         filtered.
#' @export
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- get_count_table(object)
#' nrow(counts)
#' object <- remove_all_zero_row(object)
#' counts <- get_count_table(object)
#' nrow(counts)
remove_all_zero_row <- function(screenR_Object) {
    counts <- screenR_Object@count_table

    # First of all all the row with zero counts are founded
    counts_zeros <- filter(
        counts,
        if_all(
            vars_select_helpers$where(is.numeric),
            ~ .x == 0
        )
    )

    # Then there are filtered
    counts <- filter(counts, !.data$Barcode %in% counts_zeros$Barcode)

    screenR_Object@count_table <- counts


    # The same procedure as to be done for the annotation_table table
    # in order to keep thigs working

    anno_table <- screenR_Object@annotation_table

    anno_table <-
        filter(
            anno_table,
            !.data$Barcode %in% counts_zeros$Barcode
        )

    screenR_Object@annotation_table <- anno_table

    return(screenR_Object)
}
