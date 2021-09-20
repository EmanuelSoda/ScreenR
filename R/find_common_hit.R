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

find_common_hit <- function(hit_zscore, hit_camera, hit_roast){
  # First we gets all the row names of the different table
  hit_zscore <- rownames(hit_zscore)
  hit_camera <- rownames(hit_camera)
  hit_roast <- rownames(hit_roast)

  # We then create a list of those vector
  hit_list <- list(hit_zscore, hit_camera, hit_roast)

  # We use the function Reduce to apply the intersect to the
  # list and obtain a vector
  hit_common <-  Reduce(intersect, hit_list)
  return(hit_common)
}

