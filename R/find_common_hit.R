#' @title Find common hit
#' @description This method find the hit in common between the three metod
#' @param hit_zscore The matrix obtained by the \code{\link{find_zscore_hit}}
#'                   method
#' @param hit_camera The matrix obtained by the \code{\link{find_camera_hit}}
#'                   method
#' @param hit_roast The matrix obtained by the \code{\link{find_roast_hit}}
#'                  method
#'
#' @return A vector containing the common hit
#' @export

find_common_hit <- function(hit_zscore, hit_camera, hit_roast,
                            common_in = 3){
  # First we gets all the Gene name of the different table
  hit_zscore_Gene <- as.vector(hit_zscore$Gene)
  hit_camera_Gene<- as.vector(camera_hit$Gene)
  hit_roast_Gene <- as.vector(roast_hit$Gene)

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
      dplyr::mutate(zscore_hit =
                      ifelse(test = .data$Gene %in% rownames(zscore_hit),
                             yes = 1, no = 0)) %>%
      dplyr::mutate(camera_hit =
                      ifelse(test = .data$Gene %in% camera_hit$Gene,
                             yes = 1, no = 0)) %>%
      dplyr::mutate(roast_hit =
                      ifelse(test = .data$Gene  %in% roast_hit$Gene,
                             yes = 1, no = 0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(common = sum(.data$zscore_hit, .data$camera_hit,
                                 .data$roast_hit)) %>%
      dplyr::filter(.data$common > 1) %>%
      dplyr::pull(Gene)
  }
  return(hit_common)
}

