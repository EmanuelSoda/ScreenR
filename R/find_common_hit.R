#' @title Find common hit
#' @description This method find the hit in common between the three methods
#' @param hit_zscore The matrix obtained by the \code{\link{find_zscore_hit}}
#'                   method
#' @param hit_camera The matrix obtained by the \code{\link{find_camera_hit}}
#'                   method
#' @param hit_roast The matrix obtained by the \code{\link{find_roast_hit}}
#'                  method
#' @param common_in Number of method in which the hit should be part at
#'                  the same time
#' @return A vector containing the common hit
#' @export
#' @examples
#' hit_zscore <- tibble(Gene = c('A', 'B', 'C', 'D', 'E'))
#' hit_camera <- tibble(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
#' hit_roast <- tibble(Gene = c('A', 'L', 'N'))
#'
#' # common among all the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast)
#'
#' # common among at least twoo of the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2)

find_common_hit <- function(hit_zscore, hit_camera, hit_roast, common_in = 3) {
    # First we gets all the Gene name of the different table
    hit_zscore_Gene <- as.vector(hit_zscore$Gene)
    hit_camera_Gene <- as.vector(hit_camera$Gene)
    hit_roast_Gene <- as.vector(hit_roast$Gene)

    hit_common <- c("")
    if (common_in == 3) {
        # We then create a list of those vector
        hit_list <- list(hit_zscore_Gene, hit_camera_Gene, hit_roast_Gene)

        # We use the function Reduce to apply the intersect to the list and
        # obtain a vector
        hit_common <- Reduce(intersect, hit_list)

    } else if (common_in == 2) {
        name <- unique(c(hit_zscore_Gene, hit_camera_Gene, hit_roast_Gene))

        hit_common <- tibble(Gene = name) %>%
            dplyr::mutate(hit_zscore = ifelse(test = .data$Gene %in%
                hit_zscore_Gene, yes = 1, no = 0)) %>%
            dplyr::mutate(hit_camera = ifelse(test = .data$Gene %in%
                hit_camera_Gene, yes = 1, no = 0)) %>%
            dplyr::mutate(hit_roast = ifelse(test = .data$Gene %in%
                hit_roast_Gene, yes = 1, no = 0)) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(common = sum(.data$hit_zscore, .data$hit_camera,
                .data$hit_roast)) %>%
            dplyr::filter(.data$common > 1) %>%
            dplyr::pull(.data$Gene)
    }
    return(hit_common)
}

#' @title Plot common hit
#' @description This method plot the hit in common between the three methods
#' @param hit_zscore The list of hits of the Z-score method
#' @param hit_camera The list of hits of the CAMERA method
#' @param hit_roast The list of hits of the ROAST method
#' @param alpha The opacity of the color
#' @param stroke_size Stroke size for drawing circles
#' @param set_name_size Text size for set names
#' @param text_size Text size for intersect contents
#' @param text_color Text color for intersect contents
#' @param show_percentage Show percentage for each set
#' @param title The title to display above the plot
#' @return A vector containing the common hit
#' @param color The three vector color for the veen
#' @export
#' @examples
#' hit_zscore <- tibble(Gene = c('A', 'B', 'C', 'D', 'E'))
#' hit_camera <- tibble(Gene = c('A', 'B', 'C', 'F', 'H', 'G'))
#' hit_roast <- tibble(Gene = c('A', 'L', 'N'))
#' plot_common_hit(hit_zscore, hit_camera, hit_roast)
plot_common_hit <- function(hit_zscore, hit_camera,
    hit_roast, alpha = 0.5, stroke_size = 0.5, set_name_size = 4,
    text_color = "black", text_size = 4, show_percentage = FALSE,
    title = "", color = c("#1B9E77", "#D95F02", "#7570B3")) {
    hit_list <- list(`Z-score Hits` = unique(hit_zscore$Gene),
        `Camera Hits` = unique(hit_camera$Gene),
        `ROAST Hits` = unique(hit_roast$Gene))

    ggvenn::ggvenn(data = hit_list, fill_alpha = alpha,
        fill_color = color, stroke_size = stroke_size,
        text_size = text_size, text_color = text_color,
        set_name_size = 4, show_percentage = show_percentage) +
        ggtitle(title)
}




