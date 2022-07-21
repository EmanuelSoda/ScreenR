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
plot_mds <- function(screenR_Object, groups = NULL, alpha = 0.8, size = 2.5,
    color = "black") {

    if (is.null(screenR_Object@normalized_count_table)) {
        stop("The normaliza count table cannot be NULL")
    }

    # We have to convert the screenR obj into an edgeR obj
    DGEList <- create_edger_obj(screenR_Object)

    # The Standard plotMDS
    plotMDS <- limma::plotMDS(DGEList, plot = FALSE, ndim = 2)

    # Create the Updated plot MDS
    PLTdata <- data.frame(
        Sample = rownames(plotMDS$distance.matrix.squared),
        x = plotMDS$x, y = plotMDS$y
    )

    if (is.null(groups)) {
        message(paste0("You did not set the the group variable ",
                       "the defaul group variable will be used"))
        PLTdata$group <- DGEList$samples$group
    } else {
        PLTdata$group <- groups
    }

    plot <- ggplot2::ggplot(PLTdata, aes(
        x = .data$x, y = .data$y,
        fill = .data$group
    )) +
        ggplot2::geom_label(aes(label = .data$Sample),
            color = color, size = size, alpha = alpha
        ) +
        ggplot2::labs(x = "First Dimension", y = "Second Dimension")

    return(plot)
}

#' @title Plot the explained variance by the PC
#' @description This function plot the explained variance by the
#'              Principal Component analysis.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @parm variable  The variable percent or cumulative.
#' @param color The color to fill the barplot the default value is steelblue
#' @importFrom  scales percent
#' @return The explained variance plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_explained_variance(object)
#'
#' # For the cumulative plot
#' plot_explained_variance(object, variable = "cumulative")
plot_explained_variance <- function(screenR_Object,
                                    variable = "percent",
                                    color = "steelblue") {
    variable <- tolower(variable)

    if (!variable %in% c("percent", "cumulative")) {
        stop("The accepted value are percent or cumulative")
    }


    PC <- compute_explained_variance(screenR_Object)

    PC <- PC %>%  mutate(PC = paste0("PC", .data$PC)) %>%
        mutate(PC = fct_reorder(.data$PC, - .data$percent))


    p <- ggplot2::ggplot(PC, aes(x = .data$PC, y = .data[[variable]])) +
        geom_bar(stat = "identity", fill = color, col = "black") +
        geom_point() +
        geom_line(aes(group = .data[[variable]]))  +
        scale_y_continuous(labels = scales::percent)

    if(variable == "percent"){
        p <- p + labs(x = NULL, y = "Expressed Variance (%)")
    } else {
        p <- p + labs(x = NULL, y = "Cumulative Expressed Variance (%)")
    }

    return(p)
}

#' @title Compute explained variance
#' @description This  is an internal function  used to compute
#'              the explained variance by each of the Principal Components.
#' @importFrom stats  prcomp
#' @importFrom purrr  map_lgl
#' @importFrom broom  tidy
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
    if (is.null(screenR_Object@count_table)) {
        stop("The count_table cannot be NULL")
    }
    data <- screenR_Object@count_table
    # Get only the numeric columns
    numeric_col <- purrr::map_lgl(data, is.numeric)

    # The data for the PC corresponds only on the numeric column
    data_PC <- data[, numeric_col]

    # To compute the PC the features has to be columns
    data_PC <- t(data_PC)

    # Rename the columns
    colnames(data_PC) <- data$Barcode

    # Computing the PCS
    PC <- prcomp(data_PC)

    PC <- broom::tidy(PC, "pcs")

    return(PC)
}




