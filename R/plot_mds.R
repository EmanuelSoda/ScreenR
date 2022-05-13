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
#' @param cumulative A boolean value which indicates whether or not to plot
#'                   the cumulative variance. The default value is FALSE.
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
#' plot_explained_variance(object, cumulative = TRUE)
plot_explained_variance <- function(screenR_Object, cumulative = FALSE,
    color = "steelblue") {
    PC <- compute_explained_variance(screenR_Object)
    # Remove the Standard deviation row
    PC <- filter(PC, .data$Name != "Standard deviation")

    # Get only the numeric columns which corresponds to the PCs
    numeric_col <- colnames(PC[, unlist(lapply(PC, is.numeric))])

    # Transform the data in a longer format
    PC <- tidyr::pivot_longer(
        data = PC, cols = all_of(numeric_col),
        names_to = "name"
    )
    PC <- dplyr::mutate(PC, name = factor(
        x = .data$name,
        levels = unique(.data$name)
    ))
    plot <- NULL


    if (cumulative) {
        # Select only the Cumulative Proportion
        PC <- dplyr::filter(PC, .data$Name == "Cumulative Proportion")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() +
            geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = scales::percent) +
            labs(
                x = NULL,
                y = "Cumulative Expressed Variance (%)"
            )
    } else {
        # Select only the Proportion of Variance
        PC <- dplyr::filter(PC, .data$Name == "Proportion of Variance")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() +
            geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = percent) +
            labs(x = NULL, y = "Expressed Variance (%)")
    }

    return(plot)
}

#' @title Compute explained variance
#' @description This  is an internal function  used to compute
#'              the explained variance by each of the Principal Components.
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




