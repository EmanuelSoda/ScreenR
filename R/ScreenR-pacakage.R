#' Tools for analyzing shRNAs screening data
#'
#'
#' \strong{ScreenR} is an easy and effective package to perform hits
#' identification in loss of function High Throughput Biological
#' Screening performed with shRNAs library. ScreenR combines the
#' power of software like edgeR with the simplicity of the Tydiverse
#' metapackage. ScreenR executes a pipeline able to find candidate
#' hits from barcode counts data and integrates a wide range of
#' visualization for each step of the analysis.
#'
#' \strong{ScreenR} takes the a count table as input and create the
#' screenr_object to perform the analysis. Throught the pipeline
#' \strong{ScreenR} anable the user to perform quality control, visual
#' inspection, dimensionality reduction of the data. Using three statistical
#' methods:
#'
#' \itemize{
#'   \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896}{ROAST}
#'   \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/}{CAMERA}
#'   \item \href{https://pubmed.ncbi.nlm.nih.gov/21515799/}{Z-score}
#'   }
#' it is able to find new candidate hits. Moreover in order to improve the
#' quality of the hit found it is also possible to further filter the list of
#' hit using other filter like the variance and the slope filters.
#'
#' @author Emanuel Michele Soda \email{emanuelsoda@@gmail.com}
#' @name ScreenR-package
#' @docType package
#' @aliases ScreenR ScreenR-package
#' @keywords internal
NULL
