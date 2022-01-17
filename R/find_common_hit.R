#' @title Find common hit
#' @description This method find the hit in common between the three metod
#' @param hit_zscore The matrix obtained by the \code{\link{find_zscore_hit}}
#'                   method
#' @param hit_camera The matrix obtained by the \code{\link{find_camera_hit}}
#'                   method
#' @param hit_roast The matrix obtained by the \code{\link{find_roast_hit}}
#'                  method
#' @param common_in Number of method in which the hit should be part at the same
#'                  time
#' @return A vector containing the common hit
#' @export

find_common_hit <- function(hit_zscore, hit_camera, hit_roast,
                            common_in = 3){
  # First we gets all the Gene name of the different table
  hit_zscore_Gene <- as.vector(hit_zscore$Gene)
  hit_camera_Gene<- as.vector(hit_camera$Gene)
  hit_roast_Gene <- as.vector(hit_roast$Gene)

  hit_common <- c("")
  if (common_in == 3) {
    # We then create a list of those vector
    hit_list <- list(hit_zscore_Gene, hit_camera_Gene, hit_roast_Gene)

    # We use the function Reduce to apply the intersect to the
    # list and obtain a vector
    hit_common <-  Reduce(intersect, hit_list)

  }  else  if (common_in == 2) {
    name <- unique(c(hit_zscore_Gene, hit_camera_Gene, hit_roast_Gene))

    hit_common <-
      tibble(Gene = name) %>%
      dplyr::mutate(hit_zscore =
                      ifelse(test = .data$Gene %in% rownames(hit_zscore),
                             yes = 1, no = 0)) %>%
      dplyr::mutate(hit_camera =
                      ifelse(test = .data$Gene %in% hit_camera$Gene,
                             yes = 1, no = 0)) %>%
      dplyr::mutate(hit_roast =
                      ifelse(test = .data$Gene  %in% hit_roast$Gene,
                             yes = 1, no = 0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(common = sum(.data$hit_zscore, .data$hit_camera,
                                 .data$hit_roast)) %>%
      dplyr::filter(.data$common > 1) %>%
      dplyr::pull(.data$Gene)
  }
  return(hit_common)
}

