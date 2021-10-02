% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_zscore_hit.R
\name{find_zscore_hit}
\alias{find_zscore_hit}
\title{Title Find Z-score Hit}
\usage{
find_zscore_hit(table_treate_vs_control, number_barcode = 6)
}
\arguments{
\item{table_treate_vs_control}{table computet with the function
\code{compute_data_table}}

\item{number_barcode}{Number of barcode to have under the median}
}
\value{
return a tibble containing the hit for the Z-score
}
\description{
Title Find Z-score Hit
}