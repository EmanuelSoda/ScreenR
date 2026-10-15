#' @title Find hits with a gene-level Z-score test
#' @description Gene-level test for the Z-score method, with a p-value and a
#'     False Discovery Rate (FDR) for each gene. Each gene gets a score, the
#'     mean (or the median) of the Z-scores of its barcodes, which is compared
#'     with the scores of random sets of barcodes of the same size. Use it on
#'     the table returned by \code{\link{compute_metrics}} or
#'     \code{\link{compute_barcode_statistics}}; it replaces
#'     \code{\link{find_zscore_hit}}.
#' @details Under the null hypothesis that the gene has no effect, its
#'     barcodes behave like barcodes taken at random, so the p-value is
#'     computed by permutation: the gene score is compared with the scores of
#'     n_permutations random sets of barcodes of the same size. The p-values
#'     are continuous (the smallest is 1 / (n_permutations + 1)) and do not
#'     depend on the distribution of the barcode scores.
#'
#'     The random barcodes are drawn from all the barcodes or, when
#'     control_genes is given, only from the barcodes of the control genes
#'     (for example the non-targeting shRNAs). With all the barcodes (the
#'     default), the barcodes of the true hits are part of the reference,
#'     which makes the test conservative when many genes have an effect.
#'     Control genes avoid this, but only if their barcodes behave like the
#'     barcodes of genes without effect. This is often not true for
#'     non-targeting shRNAs: targeting shRNAs are usually somewhat depleted
#'     compared with them (off-target effects, toxicity of the RNAi
#'     machinery), so every gene looks depleted. In the K562 essentiality
#'     screen (\code{\link{k562_count_table}}) the non-targeting reference
#'     called hundreds of non-essential genes, while the default reference did
#'     not. Before using control genes, check that their scores match those
#'     of genes known to have no effect (for example the non-essential
#'     genes); otherwise use the default.
#'
#'     This replaces the rule of \code{\link{find_zscore_hit}} ("more than
#'     number_barcode barcodes below the median"), which flags about 17\% of
#'     the genes without any effect when genes have 10 barcodes and
#'     number_barcode = 6.
#' @param table_treate_vs_control A table with one row for each barcode and
#'     at least the columns Gene, Log2FC and the score column, as returned by
#'     \code{\link{compute_metrics}} or
#'     \code{\link{compute_barcode_statistics}}.
#' @param score The column to use as score: "Zscore" (default) or
#'     "ZscoreRobust".
#' @param direction "Down" (default) for depleted genes or "Up" for enriched
#'     genes.
#' @param statistic The gene score: "mean" (default) or "median" of the
#'     barcode scores. The median is less affected by single barcodes with a
#'     strong effect (for example off-target effects) but has less power.
#' @param control_genes Optional names of control genes (as in the Gene
#'     column), for example "non-targeting". When given, the random barcodes
#'     of the permutations are drawn from their barcodes, and they are not
#'     tested. Use only controls that behave like genes without effect (see
#'     Details); the default (NULL) uses all the barcodes.
#' @param n_permutations The number of random barcode sets used to compute
#'     the p-values (default 1e5, at least 100).
#' @param min_barcodes Genes with fewer barcodes than this are not tested
#'     (default 3).
#' @param fdr The FDR threshold: only genes with an FDR lower than or equal to
#'     this value are returned (default 0.05). Use fdr = 1 to return all the
#'     tested genes.
#' @param fdr_method How the FDR is computed: "BH" (Benjamini-Hochberg, the
#'     default) or "storey" (Benjamini-Hochberg multiplied by the estimated
#'     proportion of genes without effect, less conservative when many genes
#'     have an effect).
#' @param seed An optional random seed, to make the permutations
#'     reproducible. The random number state of the session is restored
#'     afterwards.
#' @importFrom stats median
#' @return A tibble with one row for each hit and the columns Gene, NBarcodes
#'     (number of barcodes), Score (mean or median score of the barcodes),
#'     Log2FC (median log2 fold change of the barcodes), PValue and FDR,
#'     ordered by p-value.
#' @seealso \code{\link{compute_metrics}},
#'     \code{\link{compute_barcode_statistics}},
#'     \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{summarise_hits}}, \code{\link{find_discordant_barcodes}}
#' @concept find
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "Time3", treatment = "TRT",
#'     day = "Time3"
#' )
#' find_gene_zscore_hit(table, fdr = 1, n_permutations = 1000, seed = 1)
find_gene_zscore_hit <- function(table_treate_vs_control,
    score = c("Zscore", "ZscoreRobust"), direction = c("Down", "Up"),
    statistic = c("mean", "median"), control_genes = NULL,
    n_permutations = 1e5, min_barcodes = 3, fdr = 0.05,
    fdr_method = c("BH", "storey"), seed = NULL) {
    score <- match.arg(score)
    direction <- match.arg(direction)
    statistic <- match.arg(statistic)
    fdr_method <- match.arg(fdr_method)
    table <- table_treate_vs_control
    missing_columns <- setdiff(c("Gene", "Log2FC", score), colnames(table))
    if (length(missing_columns) > 0) {
        stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
    }
    if (!is.numeric(n_permutations) || length(n_permutations) != 1 ||
        n_permutations < 100) {
        stop("n_permutations must be a single number of at least 100.")
    }

    table <- dplyr::ungroup(table)
    table <- dplyr::filter(table, !is.na(.data$Gene), !is.na(.data[[score]]))
    table$Gene <- as.character(table$Gene)
    if (is.null(control_genes)) {
        reference <- table[[score]]
    } else {
        is_control <- table$Gene %in% control_genes
        reference <- table[[score]][is_control]
        if (length(reference) < 20) {
            stop("The control genes have fewer than 20 barcodes.")
        }
        table <- table[!is_control, ]
    }

    summarise_score <- if (statistic == "mean") mean else stats::median
    gene_table <- table %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(
            NBarcodes = dplyr::n(),
            Score = summarise_score(.data[[score]]),
            Log2FC = stats::median(.data$Log2FC),
            .groups = "drop"
        ) %>%
        dplyr::filter(.data$NBarcodes >= min_barcodes)

    if (!is.null(seed)) {
        restore_random_state()
        set.seed(seed)
    }
    gene_table$PValue <- NA_real_
    for (size in unique(gene_table$NBarcodes)) {
        null_scores <- sort(random_set_scores(reference, size,
            n_permutations, statistic))
        genes <- gene_table$NBarcodes == size
        scores <- gene_table$Score[genes]
        if (direction == "Down") {
            # number of random sets with a score lower or equal
            extreme <- findInterval(scores, null_scores)
        } else {
            # number of random sets with a score higher or equal
            extreme <- n_permutations -
                findInterval(scores, null_scores, left.open = TRUE)
        }
        gene_table$PValue[genes] <- (extreme + 1) / (n_permutations + 1)
    }
    gene_table$FDR <- adjust_p(gene_table$PValue, fdr_method)

    gene_table %>%
        dplyr::arrange(.data$PValue) %>%
        dplyr::filter(.data$FDR <= fdr)
}

#' @title Scores of Random Barcode Sets
#' @description Internal function that computes the mean or the median of
#'              n random sets of size barcodes drawn from the reference
#'              scores.
#' @param reference The scores of the reference barcodes.
#' @param size The number of barcodes of each set.
#' @param n The number of random sets.
#' @param statistic "mean" or "median".
#' @return A numeric vector of length n.
#' @keywords internal
random_set_scores <- function(reference, size, n, statistic) {
    draws <- matrix(sample(reference, size * n, replace = TRUE), nrow = n)
    if (statistic == "mean") {
        return(rowMeans(draws))
    }
    # Row medians without a loop: sort the values of each row together
    values <- as.vector(draws)
    sorted <- matrix(values[order(rep(seq_len(n), times = size), values)],
        nrow = size)
    (sorted[floor((size + 1) / 2), ] + sorted[ceiling((size + 1) / 2), ]) / 2
}

#' @title Compute replicate-aware barcode statistics
#' @description Tests, for each barcode, the comparison given by the contrast
#'     with the edgeR quasi-likelihood F-test (\code{\link[edgeR]{glmQLFit}},
#'     \code{\link[edgeR]{glmQLFTest}}). Unlike \code{\link{compute_metrics}},
#'     which averages the replicates before computing the log2 fold change,
#'     this uses the variability between replicates: barcodes that are noisy
#'     across replicates get a smaller score. The score is reported as a
#'     signed Z-score (sign(Log2FC) * qnorm(1 - PValue / 2)), so that the table
#'     can be used with \code{\link{find_gene_zscore_hit}} and
#'     \code{\link{find_discordant_barcodes}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @param contrast The comparison to test: the name or index of a column of
#'     matrix_model, a numeric contrast vector (for example created with
#'     \code{\link{create_contrast}}) or a one-column matrix created with
#'     \code{\link[limma]{makeContrasts}}.
#' @importFrom edgeR estimateDisp glmQLFit glmQLFTest
#' @importFrom stats qnorm p.adjust
#' @return A tibble with one row for each barcode and the columns Barcode,
#'     Gene, Log2FC, logCPM, F, PValue, FDR (Benjamini-Hochberg, over all the
#'     barcodes) and Zscore.
#' @seealso \code{\link{compute_metrics}}, \code{\link{find_gene_zscore_hit}},
#'     \code{\link{create_design_matrix}}, \code{\link{create_contrast}}
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

#' @title Find hits with a time-course model
#' @description Finds the genes whose depletion (or enrichment) caused by the
#'     treatment grows over time, using a single model for the whole time
#'     course instead of a separate analysis for each time point. Both the
#'     treated and the untreated samples must cover at least two time points.
#' @details The barcode counts are modelled with edgeR as
#'     log(abundance) = a + b * time + d * treated + c * time * treated,
#'     where time is the index of the time point (0 for the first element of
#'     days, 1 for the second, ...; the time points are treated as equally
#'     spaced) and treated is 1 for the treated samples and 0 for the control
#'     and baseline samples. The untreated samples and the treated samples
#'     each follow their own line; the coefficient c is the difference between
#'     the slopes, i.e. how much the treatment effect changes per time point.
#'     A constant treatment effect gives c = 0. The barcodes of each gene are
#'     then tested together with \code{\link[limma]{camera}}, as in
#'     \code{\link{find_camera_hit}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param treatment The value(s) of the Treatment column of
#'     \code{\link{get_sample_table}} for the treated samples.
#' @param control The value(s) of the Treatment column of
#'     \code{\link{get_sample_table}} for the control samples.
#' @param days The time points to use, in temporal order, as in the Day column
#'     of \code{\link{get_sample_table}}.
#' @param baseline Optional value(s) of the Treatment column of samples taken
#'     before the treatment (for example the first time points), which are
#'     used as untreated samples.
#' @param number_barcode Only genes with more than number_barcode barcodes
#'     are tested (default 3, so a gene needs at least 4 barcodes). The
#'     barcodes of a gene are tested together as a set, so this is not a
#'     number of barcodes that must change.
#' @param direction "Down" (default) for genes depleted over time in the
#'     treated samples or "Up" for enriched genes.
#' @param fdr The FDR threshold: only genes with an FDR lower than or equal
#'     to this value are returned (default 0.05). Use fdr = 1 to return all
#'     the genes with the given direction.
#' @param fdr_method How the FDR is computed: "BH" (Benjamini-Hochberg, the
#'     default) or "storey" (Benjamini-Hochberg multiplied by the estimated
#'     proportion of genes without effect).
#' @importFrom edgeR estimateDisp
#' @importFrom limma camera
#' @return A tibble with one row for each hit in the requested direction and
#'     the columns of \code{\link[limma]{camera}} (Gene, NGenes, Direction,
#'     PValue, FDR), where PValue is the one-sided p-value for the requested
#'     direction and FDR is computed from it, plus Log2FC: the median over the
#'     barcodes of the gene of c, the change of the treatment log2 fold change
#'     per time point.
#' @seealso \code{\link{find_camera_hit}}, \code{\link{summarise_hits}},
#'     \code{\link{plot_barcode_trend}}, \code{\link{get_sample_table}}
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
    fdr = 0.05, fdr_method = c("BH", "storey")) {
    direction <- match.arg(direction)
    fdr_method <- match.arg(fdr_method)
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
    if (length(unique(time[treated == 0])) < 2) {
        stop("The control and baseline samples must cover at least two ",
            "time points.")
    }
    design <- cbind(
        Intercept = 1, Time = time, Treated = treated,
        TimeTreated = time * treated
    )

    DGEList <- create_edger_obj(screenR_Object)
    DGEList <- DGEList[, info$Sample]
    xglm <- edgeR::estimateDisp(DGEList, design)
    genesymbols <- DGEList$genes$Gene
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

    time_hit <- run_camera(xglm, genesymbollist, design, "TimeTreated")
    effect <- gene_log2fc(
        barcode_log2fc(xglm, design, "TimeTreated"), genesymbols
    )
    time_hit <- time_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::left_join(effect, by = "Gene") %>%
        dplyr::mutate(Direction = factor(.data$Direction)) %>%
        one_direction(direction, fdr_method) %>%
        dplyr::filter(.data$FDR <= fdr)

    return(time_hit)
}

#' @title Summarise the hits of several methods
#' @description Combines the results of several hit-finding methods in one
#'     table. For each gene it reports the FDR of each method and the largest
#'     of them (MaxFDR), a conservative score to rank the genes supported by
#'     all the methods. To get a complete picture, compute the tables with
#'     fdr = 1 so that they contain all the tested genes.
#' @details Since an FDR (Benjamini-Hochberg adjusted p-value) is never
#'     smaller than the p-value, MaxFDR <= alpha implies that the gene has a
#'     p-value <= alpha in every method: for each single gene this is a
#'     level-alpha intersection-union test of the hypothesis "the gene has no
#'     effect in at least one method", valid whatever the dependence between
#'     the methods. Note however that the list of genes with MaxFDR <= alpha
#'     does not have a guaranteed False Discovery Rate: the false discoveries
#'     of the single methods can concentrate in their intersection.
#'
#'     A gene missing from a table gets an NA FDR for that method and an NA
#'     MaxFDR.
#' @param hit_list A named list of tables, each with the columns Gene and FDR
#'     (for example from \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}} or
#'     \code{\link{find_time_hit}}). The names are used in the column names
#'     of the result.
#' @param fdr The FDR threshold used for the columns NMethods and Hit
#'     (default 0.05).
#' @return A tibble with one row for each gene and the columns Gene,
#'     FDR_<method> for each method, NMethods (number of methods in which the
#'     gene has FDR <= fdr), MaxFDR and Hit (MaxFDR <= fdr), ordered by
#'     MaxFDR.
#' @seealso \code{\link{find_gene_zscore_hit}}, \code{\link{find_camera_hit}},
#'     \code{\link{find_roast_hit}}, \code{\link{find_time_hit}},
#'     \code{\link{find_common_hit}}
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
