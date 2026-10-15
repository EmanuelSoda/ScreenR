#' @title Find the hits in common between methods
#' @description Returns the genes found as hits by at least common_in of the
#'     three methods (Z-score, CAMERA and ROAST). Only the gene names are
#'     used, so the tables can come from any method. To combine the methods
#'     using their FDR, see \code{\link{summarise_hits}}.
#' @param hit_zscore The hits of the Z-score method, a table with a Gene
#'     column (for example from \code{\link{find_gene_zscore_hit}} or
#'     \code{\link{find_zscore_hit}}).
#' @param hit_camera The hits of \code{\link{find_camera_hit}}, a table with a
#'     Gene column.
#' @param hit_roast The hits of \code{\link{find_roast_hit}}, a table with a
#'     Gene column.
#' @param common_in The minimum number of methods in which a gene has to be
#'     found to be returned. The default, 3, returns the genes found by all
#'     three methods.
#' @return A character vector with the names of the common hits.
#' @importFrom tibble tibble
#' @seealso \code{\link{plot_common_hit}}, \code{\link{summarise_hits}}
#' @export
#' @concept find
#' @examples
#' hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
#' hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
#' hit_roast <- data.frame(Gene = c("A", "L", "N"))
#'
#' # Common to all the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast)
#'
#' # Common to at least two of the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2)
find_common_hit <- function(hit_zscore, hit_camera, hit_roast, common_in = 3) {
    # First we gets all the Gene name of the different table
    hit_zscore_Gene <- as.vector(hit_zscore$Gene)
    hit_camera_Gene <- as.vector(hit_camera$Gene)
    hit_roast_Gene <- as.vector(hit_roast$Gene)

    name <- unique(c(hit_zscore_Gene, hit_camera_Gene, hit_roast_Gene))

    hit_common <-
        tibble::tibble(Gene = name) %>%
        dplyr::mutate(hit_zscore = ifelse(test = .data$Gene %in%
            hit_zscore_Gene, yes = 1, no = 0)) %>%
        dplyr::mutate(hit_camera = ifelse(test = .data$Gene %in%
            hit_camera_Gene, yes = 1, no = 0)) %>%
        dplyr::mutate(hit_roast = ifelse(test = .data$Gene %in%
            hit_roast_Gene, yes = 1, no = 0)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(common = sum(
            .data$hit_zscore, .data$hit_camera,
            .data$hit_roast
        )) %>%
        dplyr::filter(.data$common >= common_in) %>%
        dplyr::pull(.data$Gene)

    return(hit_common)
}

#' @title Plot the hits in common between methods
#' @description Draws a Venn diagram of the hits of the three methods
#'     (Z-score, CAMERA and ROAST), to see how many hits they have in common.
#'     It is a wrapper around \code{\link[ggvenn]{ggvenn}}.
#' @param hit_zscore The hits of the Z-score method, a table with a Gene
#'     column (for example from \code{\link{find_gene_zscore_hit}}).
#' @param hit_camera The hits of \code{\link{find_camera_hit}}, a table with a
#'     Gene column.
#' @param hit_roast The hits of \code{\link{find_roast_hit}}, a table with a
#'     Gene column.
#' @param alpha The opacity of the circles, between 0 and 1. Default 0.5.
#' @param stroke_size The width of the circle borders.
#' @param set_name_size The text size of the set names.
#' @param text_size The text size of the contents of the intersections.
#' @param text_color The text color of the contents of the intersections.
#' @param show_percentage Whether to show the percentage of each set (used
#'     when show_elements is FALSE).
#' @param show_elements Whether to show the gene names instead of the
#'     counts (default TRUE).
#' @param title The title displayed above the plot.
#' @param color The three colors used to fill the circles.
#' @return A ggplot object.
#' @importFrom ggplot2 ggtitle
#' @seealso \code{\link{find_common_hit}}, \code{\link{summarise_hits}}
#' @export
#' @examples
#' hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
#' hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
#' hit_roast <- data.frame(Gene = c("A", "L", "N"))
#' plot_common_hit(hit_zscore, hit_camera, hit_roast)
#'
#' # Number of genes instead of their names
#' plot_common_hit(hit_zscore, hit_camera, hit_roast, show_elements = FALSE)
plot_common_hit <- function(hit_zscore, hit_camera,
    hit_roast, alpha = 0.5, stroke_size = 0.5, set_name_size = 4,
    text_color = "black", text_size = 4, show_percentage = TRUE,
    title = "", color = c("#1B9E77", "#D95F02", "#7570B3"),
    show_elements = TRUE) {
    hit_list <- list(
        `Z-score Hits` = unique(hit_zscore$Gene),
        `Camera Hits` = unique(hit_camera$Gene),
        `ROAST Hits` = unique(hit_roast$Gene)
    )

    ggvenn::ggvenn(
        data = hit_list, fill_alpha = alpha,
        columns = c("Z-score Hits", "Camera Hits", "ROAST Hits"),
        fill_color = color, stroke_size = stroke_size,
        text_size = text_size, text_color = text_color,
        show_elements = show_elements, set_name_size = set_name_size,
        show_percentage = show_percentage
    ) +
        ggplot2::ggtitle(title)
}
