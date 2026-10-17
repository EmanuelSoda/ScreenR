#' @title Compute the barcode metrics
#' @description Computes, for each barcode, the log2 fold change between the
#'     treated and the control samples of a given time point, and its Z-score
#'     and robust Z-score. The result is the input of the Z-score methods
#'     (\code{\link{find_gene_zscore_hit}}, \code{\link{find_zscore_hit}}) and
#'     of several plots. More information about the Z-score and other metrics
#'     used in genetic screens can be found in this
#'     \href{https://pubmed.ncbi.nlm.nih.gov/21515799/}{paper}.
#' @details The function uses the normalized counts (see
#'     \code{\link{get_data_table}}), so run \code{\link{normalize_data}}
#'     first. For each barcode the normalized counts of the treated and of the
#'     control samples of the given day are averaged, then
#'     Log2FC = log2((Treated + pseudocount) / (Control + pseudocount)),
#'     Zscore = (Log2FC - mean(Log2FC)) / sd(Log2FC) and
#'     ZscoreRobust = (Log2FC - median(Log2FC)) / mad(Log2FC), where the mean,
#'     standard deviation, median and median absolute deviation are computed
#'     over all the barcodes. Since the replicates are averaged, their
#'     variability is not used: see \code{\link{compute_barcode_statistics}}
#'     for a replicate-aware alternative.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param control The value(s) of the Treatment column of the data table (see
#'     \code{\link{get_sample_table}}) that identify the control samples.
#' @param treatment The value(s) of the Treatment column of the data table
#'     that identify the treated samples.
#' @param day The day (time point) to use, as in the Day column of the data
#'     table.
#' @param pseudocount A small value (in counts per million) added to the
#'     treated and control mean frequencies before computing the log2 fold
#'     change, so that barcodes with zero reads get a finite fold change:
#'     Log2FC = log2((Treated + pseudocount) / (Control + pseudocount)).
#'     Default 0.5. Barcodes with zero reads in both treated and control are
#'     removed. The value is on the scale of the normalized counts: per
#'     million reads of the effective library size of
#'     \code{\link{normalize_data}} (all the reads for "cpm", all the reads
#'     times the TMM factor for "tmm", the reads of the control barcodes for
#'     "control"). It is a display transformation that keeps the fold changes
#'     finite; it does not change the normalization.
#' @importFrom rlang .data
#' @importFrom tidyr spread pivot_wider
#' @importFrom dplyr mutate filter summarise if_else pull
#' @importFrom stats sd median mad
#' @return A tibble with one row for each barcode and the columns Barcode,
#'     Gene, Treated and Control (the mean normalized counts), Log2FC, Zscore,
#'     ZscoreRobust, Day and Treatment.
#' @seealso \code{\link{find_gene_zscore_hit}},
#'     \code{\link{compute_barcode_statistics}},
#'     \code{\link{find_discordant_barcodes}},
#'     \code{\link{plot_zscore_distribution}},
#'     \code{\link{plot_barcode_trend}}
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' metrics <- compute_metrics(object,
#'     control = "Time3",
#'     treatment = "TRT", day = "Time3"
#' )
#' head(metrics)
compute_metrics <- function(screenR_Object, control,
    treatment, day, pseudocount = 0.5) {
    if (!is.numeric(pseudocount) || length(pseudocount) != 1 ||
        pseudocount <= 0) {
        stop("pseudocount must be a single positive number.")
    }
    data_table <- data_table_of(update_screenr_object(screenR_Object),
        "compute_metrics()")
    control <- data_table %>%
        filter(.data$Treatment %in% control) %>%
        dplyr::pull(.data$Sample) %>%
        unique() %>%
        as.character()

    treated <- data_table %>%
        filter(.data$Treatment %in% treatment) %>%
        dplyr::pull(.data$Sample) %>%
        unique() %>%
        as.character()

    data_trt <- data_table %>%
        dplyr::filter(.data$Sample %in% c(control, treated)) %>%
        dplyr::mutate(Group = factor(if_else(condition = .data$Sample %in%
            treated, true = "Treated", false = "Control"),
        levels = c("Treated", "Control")
        ))

    data_trt <- data_trt %>%
        dplyr::filter(.data$Day %in% day) %>%
        dplyr::group_by(.data$Group, .data$Barcode) %>%
        dplyr::summarise(
            Mean = mean(.data$Frequency), Gene = dplyr::first(.data$Gene),
            .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
            names_from = "Group",
            values_from = "Mean"
        ) %>%
        dplyr::filter(.data$Control > 0 | .data$Treated > 0) %>%
        dplyr::mutate(Log2FC = log2((.data$Treated + pseudocount) /
            (.data$Control + pseudocount))) %>%
        dplyr::mutate(Zscore = (.data$Log2FC -
            mean(.data$Log2FC)) / sd(.data$Log2FC)) %>%
        # Robust Z-score: (x - median) / MAD, where stats::mad() already
        # includes the 1.4826 consistency constant
        dplyr::mutate(ZscoreRobust = (.data$Log2FC -
            median(.data$Log2FC)) / stats::mad(.data$Log2FC)) %>%
        dplyr::mutate(Day = day, Treatment = treatment)

    if (nrow(data_trt) > 0 && isTRUE(stats::mad(data_trt$Log2FC) == 0)) {
        warning("The median absolute deviation of the Log2FC is 0: the ",
            "robust Z-score cannot be computed.", call. = FALSE)
    }
    return(data_trt)
}
