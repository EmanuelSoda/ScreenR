#' @title Filter hits by the slope of their trend
#' @description Refines a list of candidate hits (for example found by the
#'     statistical methods) by their trend over time. For each gene it fits a
#'     regression line of the normalized counts of its barcodes over the time
#'     points, separately in the treated and in the control samples (see
#'     \code{\link{compute_slope}}), and keeps the genes that decrease more in
#'     the treated samples than in the controls.
#' @details A gene is kept when both slopes are negative and the treatment
#'     slope is lower than the control slope, or when the control slope is
#'     zero or positive and the treatment slope negative. Before
#'     this, the optional thresholds remove the genes whose control slope is
#'     larger in absolute value than slope_control and those whose treatment
#'     slope is smaller in absolute value than slope_treatment.
#' @importFrom stats lm
#' @importFrom dplyr rename
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}} (its normalized counts are used).
#' @param genes The genes to filter, for example the hits found by
#'     \code{\link{find_gene_zscore_hit}}, \code{\link{find_camera_hit}} or
#'     \code{\link{find_roast_hit}}.
#' @param slope_treatment An optional threshold: genes whose treatment slope
#'     is smaller than this in absolute value are removed. Default NULL (no
#'     threshold).
#' @param slope_control An optional threshold: genes whose control slope is
#'     larger than this in absolute value are removed. Default NULL (no
#'     threshold).
#' @param group_var_treatment The values of the Treatment column of the data
#'     table (see \code{\link{get_sample_table}}) of the samples used for the
#'     treatment trend, for example the early time points and the treated
#'     samples.
#' @param group_var_control The values of the Treatment column of the data
#'     table of the samples used for the control trend.
#' @param time_column The column of the data table with the time point of
#'     each sample: "Day" (default) or, for example, "Treatment" when the
#'     time points are described there. See \code{\link{compute_slope}}.
#' @param time An optional named numeric vector with the time of each time
#'     point, for example c(T0 = 0, T3 = 3, T18 = 18), so that the slopes
#'     are per unit of time. By default the time points are numbered 1, 2,
#'     ... in the order of the samples. See \code{\link{compute_slope}}.
#' @return A tibble with the columns Gene, slope_control and slope_treatment,
#'     with one row for each gene that passes the filter.
#' @seealso \code{\link{compute_slope}}, \code{\link{filter_by_variance}},
#'     \code{\link{plot_trend}}
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
filter_by_slope <- function(screenR_Object, genes, group_var_treatment,
    group_var_control, slope_control = NULL, slope_treatment = NULL,
    time_column = "Day", time = NULL) {

    # Compute the slope of the hits in the treatment Samples
    slope_trt <- ScreenR::compute_slope(screenR_Object, genes,
        group_var = group_var_treatment, time_column = time_column,
        time = time
    )

    
    # Compute the slope of the hits in the control Samples
    slope_contr <- ScreenR::compute_slope(screenR_Object, genes,
        group_var = group_var_control, time_column = time_column,
        time = time
    )

    data <- data_table_of(update_screenr_object(screenR_Object),
        "filter_by_slope()")

    data <- dplyr::left_join(data, slope_trt, by = "Gene")
    data <- dplyr::rename(data, slope_treatment = .data$Slope)

    data <- dplyr::left_join(data, slope_contr, by = "Gene")
    data <- dplyr::rename(data, slope_control = .data$Slope)

    if(!is.null(slope_control)){
        data <- dplyr::filter(data, abs(.data$slope_control)  <=
                                  abs(slope_control))
    }
    
    if(!is.null(slope_treatment)){
        data <- dplyr::filter(data, abs(.data$slope_treatment) >=
                                  abs(slope_treatment))
    }
    
    data <- dplyr::distinct(data, .data$Gene, 
                            .data$slope_control, .data$slope_treatment)
    # The treatment has more effect than the control 

    
    
    data <- dplyr::mutate(data, keep = dplyr::case_when(
        .data$slope_control <= 0 & .data$slope_treatment <= 0 & 
            .data$slope_treatment < .data$slope_control  ~ TRUE,
        
        .data$slope_control >= 0 & .data$slope_treatment >= 0 & 
            .data$slope_treatment >= .data$slope_control ~ FALSE,
        
        .data$slope_control >= 0 & .data$slope_treatment <= 0 ~ TRUE,

        TRUE ~ FALSE)
        )
    
    data <- dplyr::filter(data, .data$keep)
    data$keep <- NULL
    return(data)
}

#' @title Compute the slope of the trend of genes
#' @description Computes, for each gene, the slope of a regression line of the
#'     normalized counts (Frequency column of the data table) of its barcodes
#'     over time. Only the samples whose Treatment is in group_var are used.
#'     It is used by \code{\link{filter_by_slope}}, and it is the slope of
#'     the line drawn by \code{\link{plot_trend}}.
#' @details The time of each sample is taken from its time point, in the
#'     time_column column of the data table, so replicates of the same time
#'     point share the same time. By default the time points are numbered 1,
#'     2, ... in the order of the samples, so they are treated as equally
#'     spaced; give time to use the real times (for example days 0, 3 and
#'     18), and the slope is then per unit of time.
#' @importFrom rlang .data
#' @importFrom dplyr ungroup
#' @param group_var The values of the Treatment column of the data table (see
#'     \code{\link{get_sample_table}}) of the samples to use.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}} (its normalized counts are used).
#' @param genes The genes for which the slope is computed.
#' @param time_column The column of the data table with the time point of
#'     each sample: "Day" (default) or, for example, "Treatment" when the
#'     time points are described there.
#' @param time An optional named numeric vector with the time of each time
#'     point (the names are the values of time_column), for example
#'     c(T0 = 0, T3 = 3, T18 = 18). By default the time points are numbered
#'     1, 2, ... in the order of the samples.
#' @return A row-wise tibble with the columns Gene and Slope, with one row for
#'     each gene.
#' @seealso \code{\link{filter_by_slope}}, \code{\link{plot_trend}}
#' @export
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_slope(object,
#'     genes = c("Gene_42", "Gene_24"),
#'     group_var = c("T1", "T2", "TRT")
#' )
#'
#' # With the real times of the time points
#' compute_slope(object,
#'     genes = c("Gene_42", "Gene_24"),
#'     group_var = c("T1", "T2", "TRT"),
#'     time = c(T1 = 0, T2 = 2, Time3 = 7, Time4 = 14)
#' )
compute_slope <- function(screenR_Object, genes, group_var,
    time_column = "Day", time = NULL) {
    data <- data_table_of(update_screenr_object(screenR_Object),
        "compute_slope()")
    data <- dplyr::filter(data, .data$Gene %in% genes)
    data <- dplyr::filter(data, .data$Treatment %in% group_var)
    data$Time <- encode_time(data, time_column, time)

    data <- dplyr::mutate(dplyr::nest_by(data, .data$Gene),
        Slope = lm(Frequency ~ Time, data = data)$coefficients["Time"]
    )
    slope_tibble <- select(data, "Gene", "Slope")

    return(slope_tibble)
}

#' @title Encode the time of the samples
#' @description Internal function that gives each row of the data table the
#'     time of its sample, used by \code{\link{compute_slope}} and
#'     \code{\link{plot_trend}}. Replicates of the same time point share the
#'     same time.
#' @param data The data table, or a part of it, with the columns Sample and
#'     time_column.
#' @param time_column The column with the time point of each sample.
#' @param time NULL to number the time points 1, 2, ... in the order of the
#'     samples, or a named numeric vector with the time of each time point.
#' @return A numeric vector with one value for each row of data, with the
#'     attribute "labels" (the time point of each distinct time, in order)
#'     when time is NULL.
#' @keywords internal
encode_time <- function(data, time_column = "Day", time = NULL) {
    if (!time_column %in% colnames(data)) {
        stop("time_column must be a column of the data table: ",
            paste(colnames(data), collapse = ", "), ".")
    }
    time_point <- as.character(data[[time_column]])
    if (is.null(time)) {
        # The time points in the order of the samples
        sample_rank <- if (is.factor(data$Sample)) {
            as.integer(data$Sample)
        } else {
            match(data$Sample, unique(data$Sample))
        }
        levels <- unique(time_point[order(sample_rank)])
        encoded <- match(time_point, levels)
        attr(encoded, "labels") <- levels
        return(encoded)
    }
    if (!is.numeric(time) || is.null(names(time))) {
        stop("time must be a named numeric vector, for example ",
            "c(T0 = 0, T3 = 3, T18 = 18).")
    }
    missing_time <- setdiff(unique(time_point), names(time))
    if (length(missing_time) > 0) {
        stop("time has no value for the time point(s): ",
            paste(missing_time, collapse = ", "), ".")
    }
    unname(time[time_point])
}


#' @title Filter hits by the variance of their barcodes
#' @description Refines a list of candidate hits by the consistency of their
#'     barcodes. For each gene it computes the variance of the log2 fold
#'     changes of its barcodes (estimated with edgeR for the given contrast)
#'     and keeps the genes whose variance is at most the given threshold:
#'     a gene whose barcodes disagree is a less reliable hit.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}}. The model uses its raw counts with its
#'     effective library sizes as offsets, the same normalization as the plots
#'     and the data table (see \code{\link{create_edger_obj}}).
#' @param genes The genes to filter, for example the hits found by
#'     \code{\link{find_gene_zscore_hit}}, \code{\link{find_camera_hit}} or
#'     \code{\link{find_roast_hit}}.
#' @param variance The maximum variance of the barcode log2 fold changes
#'     accepted (default 0.5).
#' @param contrast The comparison used to compute the log2 fold changes: the
#'     name or index of a column of matrix_model, a numeric contrast vector
#'     (for example created with \code{\link{create_contrast}}) or a
#'     one-column matrix created with \code{\link[limma]{makeContrasts}}.
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @return The rows of the data table (see \code{\link{get_data_table}}) of
#'     the genes that pass the filter, with an additional Variance column (the
#'     variance of the barcode log2 fold changes of the gene).
#' @seealso \code{\link{filter_by_slope}},
#'     \code{\link{find_discordant_barcodes}}
#' @export
#' @concept filter
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- create_design_matrix(object)
#' contrast <- create_contrast(matrix_model,
#'     treatment = "Treated", control = "Control"
#' )
#'
#' data <- filter_by_variance(
#'     screenR_Object = object, genes = c("Gene_42"),
#'     matrix_model = matrix_model, contrast = contrast
#' )
#' head(data)
filter_by_variance <- function(screenR_Object, genes, matrix_model,
    variance = 0.5, contrast) {
    data <- data_table_of(update_screenr_object(screenR_Object),
        "filter_by_variance()")

    # Create the edgeR object and computing the LogFC
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)

    lrt <- data.frame(logFC = barcode_log2fc(xglm, matrix_model, contrast))
    lrt$Gene <- DGEList$genes$Gene
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



#' @title Remove the barcodes with zero counts in all samples
#' @description Removes the barcodes (rows) with zero counts in all the
#'     samples, with their annotation. Run it before
#'     \code{\link{normalize_data}}. On a normalized object the
#'     normalization is kept (barcodes without reads do not change the
#'     library sizes; see \code{\link{screenr_object-class}}).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return The ScreenR object without the barcodes with zero counts in all
#'     samples.
#' @seealso \code{\link{normalize_data}}, \code{\link{barcode_lost}}
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
    object <- update_screenr_object(screenR_Object)
    counts <- SummarizedExperiment::assay(object, "counts",
        withDimnames = FALSE)
    object[rowSums(counts) > 0, ]
}
