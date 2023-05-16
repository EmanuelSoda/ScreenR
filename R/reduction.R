#' @title Compute MDS
#' @description Compute MDS reduction 
#' @param screenR_Object The Object of the package
#'                       \code{\link{create_screenr_object}}
#' @param groups The vector that has to be used to fill the plot if NULL the
#'               function will use the default groups slot in the object passed
#'               as input.
#' @param dimensions The number of MDS dimensions to compute. By default is 2,
#' @importFrom limma plotMDS
#' @return The screenR_Object with the MDS reduciton computed
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_mds(object)
#' 
compute_mds <- function(screenR_Object, groups = NULL, dimensions = 2) {
    if (!"MDS" %in% names(screenR_Object@reduction)) {
        # We have to convert the screenR obj into an edgeR obj
        DGEList <- create_edger_obj(screenR_Object)
        
        # The Standard plotMDS
        plotMDS <-
            limma::plotMDS(DGEList, plot = FALSE, ndim = dimensions)
        
        # Create the Updated plot MDS
        PLTdata <- data.frame(
            Sample = rownames(plotMDS$distance.matrix.squared),
            MDS1 = plotMDS$x,
            MDS2 = plotMDS$y
        )
        
        if (is.null(groups)) {
            PLTdata$group <- screenR_Object@groups
        } else {
            PLTdata$group <- groups
        }
        screenR_Object@reduction <- c(screenR_Object@reduction,
                                      list("MDS" = PLTdata))
    }
    return(screenR_Object)
}


#' @title Compute PCA
#' @description Compute PCA reduction 
#' @param screenR_Object The Object of the package
#'                       \code{\link{create_screenr_object}}
#' @param groups The vector that has to be used to fill the plot if NULL the
#'               function will use the default groups slot in the object passed
#'               as input.
#' @param dimensions The number of MDS dimensions to compute. By default is 2.
#' @param center A logical value indicating whether the variables should be 
#'               shifted to be zero centered. Alternately, a vector of length 
#'               equal the number of columns of x can be supplied.
#'               The default is TRUE. 
#' @param scale a logical value indicating whether the variables should be 
#'              scaled to have unit variance before the analysis takes place. 
#'              The default is FALSE.
#' @param log A logical value indicating if the data should be log1p
#' @return The screenR_Object with the MDS reduciton computed
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_mds(object)
#' 
compute_pca <- function(screenR_Object, groups = NULL, dimensions = 2,
                        center = TRUE, scale = FALSE, log = TRUE) {
    if (!"PCA" %in% names(screenR_Object@reduction)) {
        # extract the normalized counts
        counts <- screenR_Object@normalized_count_table
        
        counts <-
            as.matrix(dplyr::select(counts, tidyselect::where(is.numeric)))
        
        counts <- t(counts)
        if (log) {
            counts <- log1p(counts)
        }
        # The Standard plotMDS
        pca <- stats::prcomp(counts, center = center, scale. = scale)
        embedding <- as.data.frame(pca$x[, seq(1, dimensions, 1)])
        embedding$Sample <- rownames(embedding)
        rownames(embedding) <- NULL
        if (is.null(groups)) {
            embedding$group <- screenR_Object@groups
        } else {
            embedding$group <- groups
        }
        embedding <- dplyr::select(embedding, Sample,
                                   tidyselect::starts_with("PC"),
                                   group)
        screenR_Object@reduction <- c(screenR_Object@reduction,
                                      list("PCA" = embedding))
    } 
    return(screenR_Object)
}