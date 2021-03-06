% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute.R
\name{calc_detected_by_decode}
\alias{calc_detected_by_decode}
\title{Tabulate the number of cases where the pathogen(s) are detected in TAC
  results by DeCoDe result (in causal chain or not, etc.) and by either
  site or age}
\usage{
calc_detected_by_decode(
  d,
  by = "site",
  condition,
  pathogen,
  icds,
  sites = NULL,
  specimen_types = NULL
)
}
\arguments{
\item{d}{A data object obtained from \code{\link{load_data}}.}

\item{by}{One of either "site" or "age", indicating the second dimension
of tabulation.}

\item{condition}{A string specifying the condition to tabulate causal chain
presence for. To list all possibilities, see
\code{\link{valid_conditions}}.}

\item{pathogen}{A string specifying the pathogen to count positive cases of
in the TAC results.}

\item{icds}{A vector of ICD10 codes to check for the DeCoDe result of
"Contributing (P2)".}

\item{sites}{An optional vector of site names to include in the tabulation.
If not provided, all sites will be used. See \code{\link{valid_sites}}.}

\item{specimen_types}{An optional vector of specimen types to include in the
 calculation. If not provided, all specimen types will be used. See
\code{\link{valid_specimen_types}}.}
}
\description{
Tabulate the number of cases where the pathogen(s) are detected in TAC
  results by DeCoDe result (in causal chain or not, etc.) and by either
  site or age
}
\examples{
specimen_types <- c(
  "Cerebrospinal fluid sample",
  "Tissue specimen from lung",
  "Whole blood",
  "Rectal swab",
  "Plasma or spun blood specimen"
)

calc_detected_by_decode(mock,
  by = "site",
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
  specimen_types = specimen_types)
}
