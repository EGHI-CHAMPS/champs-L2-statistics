#' @title Mock data with same format as CHAMPS download
#' @description This object works just like the output from \code{\link{load_data}}. The data found in this object is not real and is only used for testing purposes.
#' @format A list with objects:
#' \describe{
#'  \item{version}{Information like CHAMPS_dataset_version.csv}
#'  \item{dmg}{Example set of Demographic data from CHAMPS_deid_basic_demographics.csv. Only the columns used in champs are kept.}
#'  \item{dcd}{Example set of DeCoDe data from CHAMPS_deid_decode_results.csv}
#'  \item{tac}{Example set of TAC data from CHAMPS_deid_tac_results.csv}
#'  \item{icd_map}{Information like CHAMPS_ICD_Mappings.csv}
#'  \item{icd_desc}{Information like CHAMPS_icd10_descriptions.csv}
#'  \item{vocab}{Information like CHAMPS_vocabulary.csv}
#'  \item{tac_long}{A long-format version of tac}
#'  \item{dcd_long}{A long-format version of dcd}
#' }
#' @source \url{https://github.com/EGHI-CHAMPS/champs-L2-statistics/blob/master/tests/testthat/evaluation_data.R}
#' @examples
#' \dontrun{
#' mock
#'}
'mock'

#' @title A vector of named infectious causes.
'infectious_causes'

#' @title A table of TAC name information
#' @description A table built from the dataset description PDF that comes with the data download.
'tac_table'
