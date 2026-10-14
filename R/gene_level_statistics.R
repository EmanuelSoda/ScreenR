#' @title Find Gene-level Z-score Hit
#' @description Gene-level test for the Z-score method, with a p-value and a
#'              False Discovery Rate (FDR) for each gene.
#'              For each gene it counts how many of its barcodes have a
#'              score beyond the threshold (below -zscore_threshold for
#'              direction "Down", above zscore_threshold for direction
#'              "Up"). Under the null hypothesis that the gene has no effect
#'              its barcodes behave like all the other barcodes, so the
#'              number of barcodes beyond the threshold follows a binomial
#'              distribution with the proportion observed over all the
#'              barcodes. The one-sided binomial p-values are corrected
#'              with the Benjamini-Hochberg method.
#'              This replaces the rule of \code{\link{find_zscore_hit}}
#'              ("more than number_barcode barcodes below the median"),
#'              which flags about 17\% of the genes without any effect when
#'              genes have 10 barcodes and number_barcode = 6.
#' @param table_treate_vs_control A table with one row for each barcode and
#'                                at least the columns Gene, Log2FC and the
#'                                score column, as returned by
#'                                \code{\link{compute_metrics}} or
#'                                \code{\link{compute_barcode_statistics}}.
#' @param score The column to use as score: "Zscore" (default) or
#'              "ZscoreRobust".
#' @param zscore_threshold A positive number: a barcode counts as depleted
#'                         (enriched) when its score is below (above)
#'                         -zscore_threshold (zscore_threshold).
#' @param direction "Down" (default) for depleted genes or "Up" for
#'                  enriched genes.
#' @param min_barcodes Genes with fewer barcodes than this are not tested.
#' @param fdr The False Discovery Rate threshold: only genes with an FDR
#'            lower or equal to this value are returned. Use fdr = 1 to
#'            return all the tested genes.
#' @importFrom stats pbinom p.adjust median
#' @return A tibble with one row for each hit and the columns Gene,
#'         NBarcodes (number of barcodes), NBarcodesHit (number of barcodes
#'         beyond the threshold), Log2FC (median log2 fold change of the
#'         barcodes), PValue and FDR, ordered by p-value.
#' @concept find
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3",
#'     day = "Time3"
#' )
#' find_gene_zscore_hit(table, fdr = 1)
find_gene_zscore_hit <- function(table_treate_vs_control,
    score = c("Zscore", "ZscoreRobust"), zscore_threshold = 2,
    direction = c("Down", "Up"), min_barcodes = 3, fdr = 0.05) {
    score <- match.arg(score)
    direction <- match.arg(direction)
    table <- table_treate_vs_control
    missing_columns <- setdiff(c("Gene", "Log2FC", score), colnames(table))
    if (length(missing_columns) > 0) {
        stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
    }
    if (!is.numeric(zscore_threshold) || length(zscore_threshold) != 1 ||
        zscore_threshold <= 0) {
        stop("zscore_threshold must be a single positive number.")
    }

    table <- dplyr::ungroup(table)
    table <- dplyr::filter(table, !is.na(.data$Gene), !is.na(.data[[score]]))
    values <- table[[score]]
    if (direction == "Down") {
        table$is_hit <- values < -zscore_threshold
    } else {
        table$is_hit <- values > zscore_threshold
    }
    # The proportion of barcodes beyond the threshold over all the barcodes
    # is the probability of a barcode being beyond it under the null
    proportion <- mean(table$is_hit)

    hit_table <- table %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(
            NBarcodes = dplyr::n(),
            NBarcodesHit = sum(.data$is_hit),
            Log2FC = stats::median(.data$Log2FC),
            .groups = "drop"
        ) %>%
        dplyr::filter(.data$NBarcodes >= min_barcodes) %>%
        dplyr::mutate(
            PValue = stats::pbinom(.data$NBarcodesHit - 1, .data$NBarcodes,
                proportion,
                lower.tail = FALSE
            ),
            FDR = stats::p.adjust(.data$PValue, method = "BH")
        ) %>%
        dplyr::arrange(.data$PValue) %>%
        dplyr::filter(.data$FDR <= fdr)

    return(hit_table)
}

#' @title Compute Barcode Statistics
#' @description Computes for each barcode a replicate-aware test of the
#'              given contrast with the edgeR quasi-likelihood F-test
#'              (\code{\link[edgeR]{glmQLFit}},
#'              \code{\link[edgeR]{glmQLFTest}}). Unlike
#'              \code{\link{compute_metrics}}, which averages the replicates
#'              before computing the fold change, this uses the variability
#'              between replicates: barcodes that are noisy across
#'              replicates get a smaller score.
#'              The score is reported as a signed Z-score
#'              (sign(Log2FC) * qnorm(1 - PValue / 2)) so that the table can
#'              be used with \code{\link{find_gene_zscore_hit}}.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param matrix_model The design matrix, for example created with
#'                     \code{\link{create_design_matrix}}.
#' @param contrast The comparison to test: the name or index of a column of
#'                 matrix_model, a numeric contrast vector (for example
#'                 created with \code{\link{create_contrast}}) or a
#'                 one-column matrix created with
#'                 \code{\link[limma]{makeContrasts}}.
#' @importFrom edgeR estimateDisp glmQLFit glmQLFTest
#' @importFrom stats qnorm p.adjust
#' @return A tibble with one row for each barcode and the columns Barcode,
#'         Gene, Log2FC, logCPM, F, PValue, FDR and Zscore.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' design <- create_design_matrix(object)
#' contrast <- create_contrast(design,
#'     treatment = "Treated",
#'     control = "Control"
#' )
#' statistics <- compute_barcode_statistics(object, design, contrast)
#' head(statistics)
compute_barcode_statistics <- function(screenR_Object, matrix_model,
    contrast) {
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)
    fit <- edgeR::glmQLFit(xglm, matrix_model)
    if (length(contrast) == 1) {
        test <- edgeR::glmQLFTest(fit, coef = contrast)
    } else {
        test <- edgeR::glmQLFTest(fit, contrast = contrast)
    }
    result <- test$table

    statistics <- tibble::tibble(
        Barcode = as.character(DGEList$genes$Barcode),
        Gene = as.character(DGEList$genes$Gene),
        Log2FC = result$logFC,
        logCPM = result$logCPM,
        F = result$F,
        PValue = result$PValue,
        FDR = stats::p.adjust(result$PValue, method = "BH"),
        Zscore = sign(result$logFC) *
            stats::qnorm(result$PValue / 2, lower.tail = FALSE)
    )
    return(statistics)
}

#' @title Find Time Hit
#' @description Finds the genes whose depletion (or enrichment) caused by
#'              the treatment grows over time, using a single model for the
#'              whole time course instead of a separate analysis for each
#'              time point.
#'              The barcode counts are modelled with edgeR as
#'              log(abundance) = a + b * time + c * time * treated,
#'              where time is the index of the time point (0 for the first
#'              element of days, 1 for the second, ...) and treated is 1 for
#'              the treated samples and 0 for the control and baseline
#'              samples. The coefficient c is the extra change per time
#'              point in the treated samples compared with the control
#'              samples. The barcodes of each gene are then tested together
#'              with \code{\link[limma]{camera}}.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param treatment The value(s) of the Treatment column of
#'                  \code{\link{get_sample_table}} for the treated samples.
#' @param control The value(s) of the Treatment column of
#'                \code{\link{get_sample_table}} for the control samples.
#' @param days The time points to use, in temporal order, as in the Day
#'             column of \code{\link{get_sample_table}}.
#' @param baseline Optional value(s) of the Treatment column of samples
#'                 taken before the treatment (for example the first time
#'                 points), which are used as untreated samples.
#' @param number_barcode Only genes with more than number_barcode barcodes
#'                       are tested.
#' @param direction "Down" (default) for genes depleted over time in the
#'                  treated samples or "Up" for enriched genes.
#' @param fdr The False Discovery Rate threshold: only genes with an FDR
#'            lower or equal to this value are returned. Use fdr = 1 to
#'            return all the genes with the given direction.
#' @importFrom edgeR estimateDisp
#' @importFrom limma camera
#' @return A tibble with the columns of \code{\link[limma]{camera}} and
#'         Log2FC, the median over the barcodes of the gene of the extra
#'         log2 fold change per time point in the treated samples.
#' @concept find
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' get_sample_table(object)
#' find_time_hit(object,
#'     treatment = "TRT", control = c("Time3", "Time4"),
#'     days = c("Time3", "Time4"), fdr = 1
#' )
find_time_hit <- function(screenR_Object, treatment, control, days,
    baseline = NULL, number_barcode = 3, direction = c("Down", "Up"),
    fdr = 0.05) {
    direction <- match.arg(direction)
    if (length(days) < 2) {
        stop("At least two time points are needed.")
    }
    info <- get_sample_table(screenR_Object)
    use <- info$Day %in% days &
        info$Treatment %in% c(treatment, control, baseline)
    info <- info[use, ]
    treated <- as.numeric(info$Treatment %in% treatment)
    if (sum(treated) == 0) {
        stop("No treated samples found for the given treatment and days.")
    }
    if (sum(treated == 0) == 0) {
        stop("No control or baseline samples found for the given days.")
    }
    time <- match(info$Day, days) - 1
    if (length(unique(time[treated == 1])) < 2) {
        stop("The treated samples must cover at least two time points.")
    }
    design <- cbind(Intercept = 1, Time = time, TimeTreated = time * treated)

    DGEList <- create_edger_obj(screenR_Object)
    DGEList <- DGEList[, info$Sample]
    xglm <- edgeR::estimateDisp(DGEList, design)
    genesymbols <- DGEList$genes$Gene
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

    time_hit <- limma::camera(xglm,
        index = genesymbollist, design = design,
        contrast = "TimeTreated"
    )
    effect <- gene_log2fc(
        barcode_log2fc(xglm, design, "TimeTreated"), genesymbols
    )
    time_hit <- time_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::left_join(effect, by = "Gene") %>%
        dplyr::filter(.data$Direction == direction) %>%
        dplyr::filter(.data$FDR <= fdr)

    return(time_hit)
}

#' @title Summarise Hits
#' @description Combines the results of several hit-finding methods in one
#'              table. For each gene it reports the FDR of each method and
#'              the largest of them (MaxFDR). A gene with MaxFDR <= fdr is
#'              significant at that FDR level in every method: this is an
#'              intersection-union test, which is valid whatever the
#'              dependence between the methods, so MaxFDR can be used as a
#'              single combined measure to rank and select the genes found
#'              by all the methods.
#'              To get a complete picture, compute the tables with fdr = 1
#'              so that they contain all the tested genes. A gene missing
#'              from a table gets an NA FDR for that method and a NA MaxFDR.
#' @param hit_list A named list of tables, each with the columns Gene and
#'                 FDR (for example from \code{\link{find_gene_zscore_hit}},
#'                 \code{\link{find_camera_hit}},
#'                 \code{\link{find_roast_hit}} or
#'                 \code{\link{find_time_hit}}).
#' @param fdr The FDR threshold used for the columns NMethods and Hit.
#' @return A tibble with one row for each gene and the columns Gene,
#'         FDR_<method> for each method, NMethods (number of methods in
#'         which the gene has FDR <= fdr), MaxFDR and Hit
#'         (MaxFDR <= fdr), ordered by MaxFDR.
#' @concept find
#' @export
#' @examples
#' zscore <- data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.2, 0.03))
#' camera <- data.frame(Gene = c("A", "B", "C"), FDR = c(0.02, 0.01, 0.5))
#' summarise_hits(list(zscore = zscore, camera = camera))
summarise_hits <- function(hit_list, fdr = 0.05) {
    if (!is.list(hit_list) || is.data.frame(hit_list) ||
        is.null(names(hit_list)) || any(names(hit_list) == "")) {
        stop("hit_list must be a named list of tables.")
    }
    tables <- lapply(names(hit_list), function(method) {
        table <- hit_list[[method]]
        if (!all(c("Gene", "FDR") %in% colnames(table))) {
            stop("The table '", method, "' must have the columns Gene and FDR.")
        }
        table <- dplyr::distinct(
            tibble::tibble(
                Gene = as.character(table$Gene),
                FDR = table$FDR
            ),
            .data$Gene,
            .keep_all = TRUE
        )
        colnames(table)[2] <- paste0("FDR_", method)
        table
    })
    summary <- Reduce(function(x, y) {
        dplyr::full_join(x, y, by = "Gene")
    }, tables)
    fdr_matrix <- as.matrix(summary[, -1, drop = FALSE])
    summary$NMethods <- rowSums(fdr_matrix <= fdr, na.rm = TRUE)
    summary$MaxFDR <- apply(fdr_matrix, 1, max)
    summary$Hit <- !is.na(summary$MaxFDR) & summary$MaxFDR <= fdr
    summary <- dplyr::arrange(summary, .data$MaxFDR)
    return(summary)
}
