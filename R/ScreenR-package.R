#' ScreenR: analysis of pooled shRNA and CRISPR-Cas9 screens
#'
#' \strong{ScreenR} finds candidate hits in loss-of-function pooled shRNA and
#' CRISPR-Cas9 screens, comparing a treatment with a control or the time
#' points of a dropout screen. It combines the statistical models of edgeR and limma with the
#' simplicity of the tidyverse, and provides plots for each step of the
#' analysis.
#'
#' The analysis starts from a barcode count table and an annotation table,
#' which are stored in a ScreenR object (\code{\link{create_screenr_object}}).
#' The counts are then normalized (\code{\link{normalize_data}}), checked
#' with quality control plots, and the hits are found with several
#' statistical methods:
#'
#' \itemize{
#'   \item the Z-score of the barcode log2 fold changes with a gene-level
#'         permutation test (\code{\link{find_gene_zscore_hit}},
#'         \href{https://pubmed.ncbi.nlm.nih.gov/21515799/}{Z-score});
#'   \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/}{CAMERA}
#'         (\code{\link{find_camera_hit}});
#'   \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896}{ROAST}
#'         and FRY (\code{\link{find_roast_hit}});
#'   \item a time-course model (\code{\link{find_time_hit}}) and a test
#'         for an effect at every time point
#'         (\code{\link{find_consistent_hit}}).
#' }
#' CRISPR-Cas9 screens can be checked with essential and non-essential
#' reference genes (\code{\link{compute_screen_quality}}) and corrected for
#' the copy-number bias with CRISPRcleanR
#' (\code{\link{correct_crispr_bias}}).
#' The results of the methods can be combined with
#' \code{\link{summarise_hits}} and further filtered with the variance and
#' slope filters (\code{\link{filter_by_variance}},
#' \code{\link{filter_by_slope}}).
#'
#' @author Emanuel Michele Soda \email{emanuelsoda@@gmail.com}
#' @aliases ScreenR ScreenR-package
#' @keywords internal
#' @importFrom sn dsn psn
"_PACKAGE"
