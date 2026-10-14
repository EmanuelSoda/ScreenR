#' @title Create the Design Matrix
#' @description Creates the design matrix used by the edgeR/limma based
#'              methods (\code{\link{find_camera_hit}},
#'              \code{\link{find_roast_hit}},
#'              \code{\link{compute_barcode_statistics}}). The matrix has no
#'              intercept: it has one column for each group of the ScreenR
#'              object, so that each coefficient is the mean of a group and
#'              a comparison between groups is expressed with a contrast
#'              created by \code{\link{create_contrast}}.
#'              The column names are syntactically valid versions of the
#'              group names (for example "T1/T2" becomes "T1.T2").
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @importFrom stats model.matrix
#' @return A design matrix with one row for each sample and one column for
#'         each group.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' create_design_matrix(object)
create_design_matrix <- function(screenR_Object) {
    groups <- groups_of(screenR_Object)
    if (length(groups) == 0) {
        stop("The ScreenR object has no groups.")
    }
    groups <- droplevels(as.factor(groups))
    design <- stats::model.matrix(~ 0 + groups)
    colnames(design) <- make.names(levels(groups))
    return(design)
}

#' @title Create a Contrast
#' @description Creates the contrast that compares the treatment group(s)
#'              with the control group(s) of a design matrix created with
#'              \code{\link{create_design_matrix}}. When more than one group
#'              is given on one side, their average is used.
#' @param design A design matrix created with
#'               \code{\link{create_design_matrix}}.
#' @param treatment The name(s) of the treatment group(s), as in the groups
#'                  of the ScreenR object or as in the column names of the
#'                  design matrix.
#' @param control The name(s) of the control group(s), as in the groups
#'                of the ScreenR object or as in the column names of the
#'                design matrix.
#' @return A named numeric vector with one value for each column of the
#'         design matrix.
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' design <- create_design_matrix(object)
#' create_contrast(design, treatment = "Treated", control = "Control")
create_contrast <- function(design, treatment, control) {
    treatment <- make.names(treatment)
    control <- make.names(control)
    not_found <- setdiff(c(treatment, control), colnames(design))
    if (length(not_found) > 0) {
        stop(
            "Group(s) not found in the design matrix: ",
            paste(not_found, collapse = ", "), ". Available groups: ",
            paste(colnames(design), collapse = ", ")
        )
    }
    if (length(intersect(treatment, control)) > 0) {
        stop("The same group cannot be both treatment and control.")
    }
    contrast <- stats::setNames(numeric(ncol(design)), colnames(design))
    contrast[treatment] <- 1 / length(treatment)
    contrast[control] <- -1 / length(control)
    return(contrast)
}

#' @title Check a Contrast
#' @description Internal function that converts a one-column contrast
#'              matrix (as returned by \code{\link[limma]{makeContrasts}})
#'              to a vector, and checks the contrast against the design.
#' @param contrast A column name or index of the design, a numeric vector,
#'                 or a one-column contrast matrix.
#' @param design The design matrix.
#' @return The contrast as a single name/index or a numeric vector.
#' @keywords internal
as_contrast <- function(contrast, design) {
    if (is.matrix(contrast)) {
        if (ncol(contrast) != 1) {
            stop("Only one contrast at a time is supported.")
        }
        contrast <- contrast[, 1]
    }
    if (length(contrast) == 1) {
        if (is.character(contrast) && !contrast %in% colnames(design)) {
            stop(
                "Contrast '", contrast, "' is not a column of the design ",
                "matrix. Available columns: ",
                paste(colnames(design), collapse = ", ")
            )
        }
    } else if (length(contrast) != ncol(design)) {
        stop(
            "The contrast must have one value for each column of the ",
            "design matrix (", ncol(design), ")."
        )
    }
    return(contrast)
}

#' @title Barcode Log2 Fold Change
#' @description Internal function that fits the edgeR negative binomial
#'              model and returns the log2 fold change of each barcode for
#'              the given contrast.
#' @param xglm A DGEList with the dispersion estimated.
#' @param design The design matrix.
#' @param contrast A contrast checked with \code{as_contrast}.
#' @importFrom edgeR glmFit glmLRT
#' @return A numeric vector with the log2 fold change of each barcode.
#' @keywords internal
barcode_log2fc <- function(xglm, design, contrast) {
    fit <- edgeR::glmFit(xglm, design)
    if (length(contrast) == 1) {
        lrt <- edgeR::glmLRT(fit, coef = contrast)
    } else {
        lrt <- edgeR::glmLRT(fit, contrast = contrast)
    }
    return(lrt$table$logFC)
}

#' @title Run CAMERA
#' @description Internal function that runs \code{\link[limma]{camera}} and
#'              always returns an FDR column (camera omits it when only one
#'              gene is tested), with a clear error when no gene can be
#'              tested.
#' @param xglm A DGEList with the dispersion estimated.
#' @param index The list of barcode indices of each gene.
#' @param design The design matrix.
#' @param contrast A contrast checked with \code{as_contrast}.
#' @importFrom limma camera
#' @return The data frame returned by camera, with an FDR column.
#' @keywords internal
run_camera <- function(xglm, index, design, contrast) {
    if (length(index) == 0) {
        stop("No gene has more than number_barcode barcodes: lower ",
            "number_barcode.", call. = FALSE)
    }
    result <- limma::camera(xglm,
        index = index, design = design, contrast = contrast
    )
    if (!"FDR" %in% colnames(result)) {
        result$FDR <- result$PValue
    }
    return(result)
}

#' @title Gene Log2 Fold Change
#' @description Internal function that summarises the barcode log2 fold
#'              changes to a gene log2 fold change using the median.
#' @param log2fc The log2 fold change of each barcode.
#' @param genes The gene of each barcode.
#' @return A tibble with the columns Gene and Log2FC.
#' @keywords internal
gene_log2fc <- function(log2fc, genes) {
    genes <- as.character(genes)
    keep <- !is.na(genes)
    medians <- tapply(log2fc[keep], genes[keep], stats::median)
    tibble::tibble(Gene = names(medians), Log2FC = as.numeric(medians))
}

#' @title One-sided P-values
#' @description Internal function that converts the two-sided p-values of
#'              \code{\link[limma]{camera}}, \code{\link[limma]{mroast}} and
#'              \code{\link[limma]{fry}} (which come with the direction of
#'              the change) into one-sided p-values for the requested
#'              direction: p / 2 for the genes changing in that direction
#'              and 1 - p / 2 for the others.
#' @param p The two-sided p-values.
#' @param observed The direction of each gene ("Up" or "Down").
#' @param direction The requested direction.
#' @return The one-sided p-values.
#' @keywords internal
one_sided_p <- function(p, observed, direction) {
    ifelse(as.character(observed) == direction, p / 2, 1 - p / 2)
}

#' @title Adjust P-values
#' @description Internal function that computes the False Discovery Rate
#'              from the p-values, with the Benjamini-Hochberg method or with
#'              the Storey method, which multiplies the Benjamini-Hochberg
#'              values by the estimated proportion of genes without effect
#'              (pi0, estimated as the proportion of p-values above 0.5
#'              divided by 0.5). The Storey method is less conservative when
#'              many genes have an effect.
#' @param p The p-values.
#' @param method "BH" or "storey".
#' @return The adjusted p-values.
#' @keywords internal
adjust_p <- function(p, method = c("BH", "storey")) {
    method <- match.arg(method)
    adjusted <- stats::p.adjust(p, method = "BH")
    if (method == "storey") {
        pi0 <- min(1, mean(p > 0.5, na.rm = TRUE) / 0.5)
        adjusted <- pmin(1, pi0 * adjusted)
    }
    adjusted
}

#' @title Gene Set Test Result in One Direction
#' @description Internal function that turns the result of a gene set test
#'              (camera, mroast or fry) into one-sided p-values for the
#'              requested direction, computes the FDR over all the genes and
#'              keeps the genes changing in that direction.
#' @param result The data frame of the test, with the columns Gene,
#'               Direction and PValue.
#' @param direction The requested direction ("Down" or "Up").
#' @param fdr_method "BH" or "storey".
#' @return The result with one-sided PValue and FDR, only for the genes in
#'         the requested direction.
#' @keywords internal
one_direction <- function(result, direction, fdr_method) {
    result$PValue <- one_sided_p(result$PValue, result$Direction, direction)
    result$FDR <- adjust_p(result$PValue, fdr_method)
    result[as.character(result$Direction) == direction, ]
}
