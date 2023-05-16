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
        embedding <- data.frame(
            Sample = unique(screenR_Object@data_table$Sample),
            MDS1 = plotMDS$x,
            MDS2 = plotMDS$y
        )
       
        if (is.null(groups)) {
            embedding$group <- screenR_Object@groups
        } else {
            embedding$group <- groups
        }
        
        mds_result <- list(embedding = embedding)
        screenR_Object@reduction <- c(screenR_Object@reduction,  
                                      list("MDS" = mds_result))
    }
    return(screenR_Object)
}


#' @title Multidimensional Scaling Plot
#' @description Plot samples on a two-dimensional scatterplot so that
#'              distances on the plot approximate the typical log2 fold
#'              changes between the samples.
#' @param screenR_Object The Object of the package
#'                       \code{\link{create_screenr_object}}
#' @param groups The vector that has to be used to fill the plot if NULL the
#'               function will use the default groups slot in the object passed
#'               as input.
#' @param alpha The opacity of the labels.
#'              Possible value are in a range from 0 to 1.
#' @param size The dimension of the labels. The default value is 2.5
#' @param color The color of the labels. The default value is black
#' @importFrom ggplot2 geom_label labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom limma plotMDS
#' @return The MDS Plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_mds(object)
plot_mds <- function(screenR_Object, groups = NULL, alpha = 0.8, size = 5,
                     color = "black") {
    
    screenR_Object <- compute_mds(screenR_Object, groups)
    PLTdata <- screenR_Object@reduction$MDS$embedding
    
    plot <- ggplot2::ggplot(PLTdata, aes(
        x = .data$MDS1, y = .data$MDS2,
        fill = .data$group,
        label = .data$Sample
    )) +
        ggplot2::geom_label(color = color, size = size, alpha = alpha
        ) 
    
    return(plot)
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
        
        # Compute PCA
        pca <- stats::prcomp(counts, center = center, scale. = scale)
        embedding <- as.data.frame(pca$x[, seq(1, dimensions, 1)])
        embedding$Sample <- unique(screenR_Object@data_table$Sample)
        rownames(embedding) <- NULL
        if (is.null(groups)) {
            embedding$group <- screenR_Object@groups
        } else {
            embedding$group <- groups
        }
        embedding <- dplyr::select(embedding, Sample,
                                   tidyselect::starts_with("PC"),
                                   group)
        
        
        # Extract the importance ad create a data.frame
        explained_variance <- data.frame(summary(pca)$importance)
        
        explained_variance <-
            tibble::rownames_to_column(explained_variance, var = "Name")
        
        explained_variance <- dplyr::mutate(explained_variance, Name = factor(
            x = .data$Name,
            levels = unique(.data$Name)
        ))
        
        
        pca_result <- list(embedding = embedding,
                           explained_variance = explained_variance
                           )
        
        screenR_Object@reduction <- c(screenR_Object@reduction,
                                      list("PCA" = pca_result))
    } 
    return(screenR_Object)
}


#' @title Plot PCA embedding
#' @description This function plot the PCA embeddings.
#' @param screenR_Object The Object of the package
#'                       \code{\link{create_screenr_object}}
#' @param groups The vector that has to be used to fill the plot if NULL the
#'               function will use the default groups slot in the object passed
#'               as input.
#' @param alpha The opacity of the labels.
#'              Possible value are in a range from 0 to 1.
#' @param size The dimension of the labels. The default value is 2.5
#' @param color The color of the labels. The default value is black
#' @importFrom ggplot2 geom_label labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom limma plotMDS
#' @return The MDS Plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))

plot_pca <- function(screenR_Object, groups = NULL, alpha = 0.8, size = 5,
                     color = "black") {
    
    screenR_Object <- compute_pca(screenR_Object, groups)
    pca_embedding <- screenR_Object@reduction$PCA$embedding
    
    plot <- ggplot2::ggplot(pca_embedding, aes(
        x = .data$PC1, y = .data$PC2,
        fill = .data$group,
        label = .data$Sample
    )) +
        ggplot2::geom_label(color = color, size = size, alpha = alpha
        ) 
    
    return(plot)
}







#' @title Plot the explained variance by the PC
#' @description This function plot the explained variance by the
#'              Principal Component analysis.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param cumulative A boolean value which indicates whether or not to plot
#'                   the cumulative variance. The default value is FALSE.
#' @param color The color to fill the barplot the default value is steelblue
#' @importFrom  scales percent
#' @importFrom forcats fct_reorder 
#' @return The explained variance plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_explained_variance(object)
#'
#' # For the cumulative plot
#' plot_explained_variance(object, cumulative = TRUE)
plot_explained_variance <- function(screenR_Object, cumulative = FALSE,
                                    color = "steelblue") {
    if (!"PCA" %in% names(screenR_Object@reduction)) {
        stop("You have to first compute the PCA using compute_pca()")
    } else{
        plot <- NULL
        
        pca_variance <- screenR_Object@reduction$PCA$explained_variance
        
        if (cumulative) {
            # Select only the Cumulative Proportion
            pca_variance <-
                dplyr::filter(pca_variance, .data$Name == "Cumulative Proportion")
            pca_variance <- pivot_longer(pca_variance, -Name)
            pca_variance$name <- 
                forcats::fct_reorder(pca_variance$name, 
                                     pca_variance$value)
            plot <-
                ggplot2::ggplot(pca_variance, aes(x = .data$name,
                                                  y = .data$value)) +
                geom_bar(stat = "identity",
                         fill = color,
                         col = "black") +
                geom_point() +
                geom_line(aes(group = .data$Name)) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = NULL,
                     y = "Cumulative Expressed Variance (%)")
        } else {
            # Select only the Proportion of Variance
            pca_variance <-
                dplyr::filter(pca_variance, 
                              .data$Name == "Proportion of Variance")
            pca_variance <- pivot_longer(pca_variance, -Name)
            pca_variance$name <- 
                forcats::fct_reorder(pca_variance$name, 
                                     -pca_variance$value)
            plot <-
                ggplot2::ggplot(pca_variance, 
                                aes(x = .data$name, y = .data$value)) +
                geom_bar(stat = "identity",
                         fill = color,
                         col = "black") +
                geom_point() +
                geom_line(aes(group = .data$Name)) +
                scale_y_continuous(labels = percent) +
                labs(x = NULL, y = "Expressed Variance (%)")
        }
    }
    return(plot)
}

#' @title Compute explained variance
#' @description This  is an internal function  used to compute
#'              the explained variance by each of the Principal Components.
#'              `r lifecycle::badge("deprecated")`
#' @importFrom stats  prcomp
#' @param screenR_Object The Object of the package
#' @return A data.frame containing all the information of the variance
#'         expressed by the components
#' @keywords internal
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_explained_variance(object)
compute_explained_variance <- function(screenR_Object) {
    lifecycle::deprecate_warn("2.0.0", 
                              "compute_explained_variance()", 
                              "compute_pca()",)
    
    data <- screenR_Object@count_table
    # Get only the numeric columns
    numeric_col <- unlist(lapply(data, is.numeric))
    
    # The data for the PC corresponds only on the numeric column
    data_PC <- data[, numeric_col]
    
    # To compute the PC the features has to be columns
    data_PC <- t(data_PC)
    
    # Rename the columns
    colnames(data_PC) <- data$Barcode
    
    # Computing the PCS
    PC <- prcomp(data_PC)
    
    # Extract the importance ad create a data.frame
    PC <- data.frame(summary(PC)$importance)
    
    PC <- tibble::rownames_to_column(PC, var = "Name")
    
    PC <- dplyr::mutate(PC, Name = factor(
        x = .data$Name,
        levels = unique(.data$Name)
    ))
    
    return(PC)
}




