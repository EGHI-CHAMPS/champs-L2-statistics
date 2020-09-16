# TODO: if specimen_types is NULL, don't do any filtering
# TODO: add annotations to the data that provide information that will go into
# a caption
# TODO: allow multiple condition and pathogen specifications, using AND logic

#' Tabulate the number of cases of a given condition found in the causal chain
#'   by site and by age, within the context of all cases
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_cc_allcases_by_site_age(mock, condition = "Streptococcus agalactiae")
calc_cc_allcases_by_site_age <- function(d, condition, sites = NULL) {
  check_champs_data(d)
  condition <- check_valid_condition(d, condition)

  # denominator (dd) data is all cases
  # TODO: (technically should be all cases that are DeCoDed)
  dd <- d$dmg

  # filter to specified sites
  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  # either the "condition" is in etiol or champs_group_desc
  # get all ids that meet this
  ids <- d$dcd_long %>%
    dplyr::filter(
      .data$etiol == condition
      | .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # now find these in demographics to create numerator (nn) data frame
  nn <- dd %>%
    dplyr::filter(.data$champs_deid %in% ids)

  denominator <- stats::xtabs(~ dd$age_group + dd$site) %>% stats::addmargins()
  numerator <- stats::xtabs(~ nn$age_group + nn$site) %>% stats::addmargins()

  structure(list(
    version = d$version,
    condition = condition,
    numerator = numerator,
    denominator = denominator
  ), class = c("champs_computed", "cc_allcases_by_site_age"))
}

#' Tabulate the number of cases of a given condition found in the causal chain
#'   by site and by age, within the context of cases where the pathogen(s)
#'   are detected in TAC results (dying from vs. dying with)
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )
#'
#' calc_cc_detected_by_site_age(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   specimen_types = specimen_types)
calc_cc_detected_by_site_age <- function(d, condition, pathogen,
  sites = NULL, specimen_types = NULL) {

  check_champs_data(d)
  condition <- check_valid_condition(d, condition)
  pathogen <- check_valid_pathogen(d, pathogen)
  specimen_types <- check_valid_specimen_types(d, specimen_types)

  # get ids of all cases that are positive for the pathogen
  # and match the specified specimen types
  ids <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$specimen_type %in% !!specimen_types) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # denominator data is all matching cases
  dd <- d$dmg %>%
    dplyr::filter(
      .data$champs_deid %in% ids)

  # get ids where condition is found in causal chain
  ids <- d$dcd_long %>%
    dplyr::filter(
      .data$etiol == condition
      | .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # data for numerator is these ids
  nn <- d$dmg %>%
    dplyr::filter(
      .data$champs_deid %in% intersect(ids, dd$champs_deid))

  # filter to specified sites
  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
    nn <- dplyr::filter(nn, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    nn$site <- forcats::fct_drop(nn$site, only = exclude)
  }

  denominator <- stats::xtabs(~ dd$age_group + dd$site) %>%
    stats::addmargins()
  numerator <- stats::xtabs(~ nn$age_group + nn$site) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    sites = sites,
    specimen_types = specimen_types,
    numerator = numerator,
    denominator = denominator
  ), class = c("champs_computed", "cc_detected_site_case"))
}

#' Tabulate the number of cases where the pathogen(s) are detected in TAC
#'   results in the context of all cases by site and by age
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )
#'
#' calc_detected_allcases_by_site_age(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   specimen_types = specimen_types)
calc_detected_allcases_by_site_age <- function(d,
  condition, pathogen, sites = NULL, specimen_types = NULL) {

  check_champs_data(d)
  condition <- check_valid_condition(d, condition)
  pathogen <- check_valid_pathogen(d, pathogen)
  specimen_types <- check_valid_specimen_types(d, specimen_types)

  # denominator is all cases
  dd <- d$dmg

  # filter to specified sites
  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  # get ids of all cases that are positive for the pathogen
  # and match the specified specimen types
  ids <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$specimen_type %in% !!specimen_types) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  nn <- dd %>%
    dplyr::filter(.data$champs_deid %in% intersect(ids, dd$champs_deid))

  numerator <- stats::xtabs(~ nn$age_group + nn$site) %>%
    stats::addmargins()
  denominator <- stats::xtabs(~ dd$age_group + dd$site) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    sites = sites,
    specimen_types = specimen_types,
    numerator = numerator,
    denominator = denominator
  ), class = c("champs_computed", "detected_allcases_by_site_age"))
}

#' Tabulate the number of cases where the pathogen(s) are detected in TAC
#'   results by DeCoDe result (in causal chain or not, etc.) and by either
#'   site or age
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param by One of either "site" or "age", indicating the second dimension
#'   of tabulation.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param icds A vector of ICD10 codes to check for the DeCoDe result of
#'   "Contributing (P2)".
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'  \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )
#'
#' calc_detected_by_decode(mock,
#'   by = "site",
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
#'   specimen_types = specimen_types)
calc_detected_by_decode <- function(d, by = "site", condition, pathogen,
  icds, sites = NULL, specimen_types = NULL) {

  check_champs_data(d)

  if (!by %in% c("site", "age"))
    stop("Argument 'by' must be one of 'site' or 'age'.")

  # get ids of all cases that are positive for the pathogen
  # and match the specified specimen types
  ids <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$specimen_type %in% !!specimen_types) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  nn <- d$dmg %>%
    dplyr::filter(.data$champs_deid %in% ids)

  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    nn <- dplyr::filter(nn, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(nn$site), sites)
    nn$site <- forcats::fct_drop(nn$site, only = exclude)
  }

  # get ids that are in causal chain based on provided condition
  cc_ids <- d$dcd_long %>%
    dplyr::filter(
      .data$etiol == condition
      | .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # get ids that are contributing to causal chain based on provided ICDs
  tmp <- dplyr::select(d$dcd,
    tidyselect::starts_with("other_significant"))
  ccc_ids <- d$dcd$champs_deid[apply(tmp, 1,
    function(x) sum(x %in% icds, na.rm = TRUE)) > 0]

  # get ids that have infectious cause == 1
  ic_ids <- d$dcd_long %>%
    dplyr::filter(.data$champs_group_desc %in% infectious_causes) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # "result" categorizes which of these buckets cases fall into
  # note that there is overlap but precedence goes to CC -> contributing -> other
  nn$result <- ""
  nn$result[nn$champs_deid %in% cc_ids] <- "In causal chain"
  nn$result[nn$result != "In causal chain" & nn$champs_deid %in% ccc_ids] <- "Contributing (P2)"
  nn$result[nn$result == "" & nn$champs_deid %in% ic_ids] <- "Other Infectious"
  nn$result[nn$result == ""] <- "Not in CC/no ID"
  nn$result <- factor(nn$result,
    c("In causal chain", "Contributing (P2)",
      "Other Infectious", "Not in CC/no ID"))

  if (by == "site") {
    numerator <- stats::xtabs(~ nn$result + nn$site) %>%
      stats::addmargins()
  } else {
    numerator <- stats::xtabs(~ nn$age_group + nn$result) %>%
      stats::addmargins()
  }

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    icds = icds,
    sites = sites,
    specimen_types = specimen_types,
    numerator = numerator
  ), class = c("champs_computed", "detected_by_decode"))
}

#' Tabulate the number of positive specimens for each case where the pathogen(s)
#'   are detected in TAC results by postmortem interval range (PMI -
#'   time from death to MITS)
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_nspecimen_by_pmi(mock, pathogen = "Group B Streptococcus")

calc_nspecimen_by_pmi <- function(d, pathogen, sites = NULL) {
  check_champs_data(d)

  dd <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen) %>%
    dplyr::group_by(.data$champs_deid) %>%
    dplyr::tally() %>%
    dplyr::right_join(d$dmg, by = "champs_deid") %>%
    dplyr::filter(!is.na(.data$pmi_range))

  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  dd$n[is.na(dd$n)] <- 0
  dd$n <- factor(dd$n)

  numerator <- stats::xtabs(~ dd$n + dd$pmi_range) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    pathogen = pathogen,
    sites = sites,
    numerator = numerator
  ), class = c("champs_computed", "nspecimen_by_pmi"))
}

#' Calculate average postmortem interval (PMI - average time from death to MITS
#'   in hours) by specimen type of NP Only vs. Blood/CSF/Lung and by site
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_pmi_by_specimen_site(mock, pathogen = "Group B Streptococcus")
calc_pmi_by_specimen_site <- function(d, pathogen, sites = NULL) {
  check_champs_data(d)

  dd <- dplyr::filter(d$tac_long,
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$champs_deid %in% d$dmg$champs_deid) %>%
    dplyr::group_by(.data$champs_deid) %>%
    dplyr::summarise(
      has_np = any(.data$specimen_type %in%
        "Nasopharyngeal and Oropharyngeal swab"),
      has_other = any(.data$specimen_type %in% c(
        "Cerebrospinal fluid sample",
        "Tissue specimen from lung",
        "Whole blood",
        "Rectal swab",
        "Plasma or spun blood specimen"
      ))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      specimen_type2 = ifelse(.data$has_np & !.data$has_other,
        "NP Only", "Blood, CSF, or Lung")
    ) %>%
    dplyr::left_join(dplyr::select(d$dmg, tidyselect::one_of(
      c("champs_deid", "calc_postmortem_hrs", "site"))),
      by = "champs_deid") %>%
    dplyr::filter(!is.na(.data$calc_postmortem_hrs)
      & .data$calc_postmortem_hrs >= 0)

  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  dd$specimen_type2 <- forcats::fct_relevel(dd$specimen_type2,
    c("NP Only", "Blood, CSF, or Lung"))

  numerator <- stats::xtabs(dd$calc_postmortem_hrs ~ dd$specimen_type2 +
    dd$site) %>%
    stats::addmargins()
  denominator <- stats::xtabs(~ dd$specimen_type2 + dd$site) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    pathogen = pathogen,
    numerator = numerator,
    denominator = denominator
  ), class = c("champs_computed", "pmi_by_specimen_site"))
}

# internal
get_pmi_decode_data <- function(
  d, condition, pathogen, icds, specimen_types, sites
) {
  vars <- c(
    "champs_deid", condition, "infectious_cause", "calc_postmortem_hrs",
    paste0("other_significant_condition_0", 1:8), "site", "age_group"
  )

  # get ids of all cases that are positive for the pathogen
  # and match the specified specimen types
  ids <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$specimen_type %in% !!specimen_types) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  nn <- d$dmg %>%
    dplyr::filter(.data$champs_deid %in% ids
    & !is.na(.data$calc_postmortem_hrs) & .data$calc_postmortem_hrs >= 0)

  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    nn <- dplyr::filter(nn, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(nn$site), sites)
    nn$site <- forcats::fct_drop(nn$site, only = exclude)
  }

  # get ids that are in causal chain based on provided condition
  cc_ids <- d$dcd_long %>%
    dplyr::filter(
      .data$etiol == condition
      | .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # get ids that are contributing to causal chain based on provided ICDs
  tmp <- dplyr::select(d$dcd,
    tidyselect::starts_with("other_significant"))
  ccc_ids <- d$dcd$champs_deid[apply(tmp, 1,
    function(x) sum(x %in% icds, na.rm = TRUE)) > 0]

  # get ids that have infectious cause == 1
  ic_ids <- d$dcd_long %>%
    dplyr::filter(.data$champs_group_desc %in% infectious_causes) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # "result" categorizes which of these buckets cases fall into
  # note that there is overlap but precedence goes to CC -> contributing
  #  -> other
  nn$result <- ""
  nn$result[nn$champs_deid %in% cc_ids] <- "In causal chain"
  nn$result[nn$result != "In causal chain" & nn$champs_deid %in% ccc_ids] <-
    "Contributing (P2)"
  nn$result[nn$result == "" & nn$champs_deid %in% ic_ids] <- "Other Infectious"
  nn$result[nn$result == ""] <- "Not in CC/no ID"
  nn$result <- factor(nn$result,
    c("In causal chain", "Contributing (P2)",
      "Other Infectious", "Not in CC/no ID"))

  nn
}

#' Calculate average postmortem interval (PMI - average time from death to MITS
#'   in hours) by DeCoDe result and site
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param icds A vector of ICD10 codes to check for the DeCoDe result of
#'   "Contributing (P2)".
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )
#'
#' calc_pmi_by_decode_site(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
#'   specimen_types = specimen_types)

calc_pmi_by_decode_site <- function(d, condition, pathogen, icds,
  specimen_types = NULL, sites = NULL) {

  check_champs_data(d)

  dd <- get_pmi_decode_data(d, condition, pathogen, icds, specimen_types, sites)

  t1 <- stats::xtabs(dd$calc_postmortem_hrs ~ dd$result + dd$site) %>%
    stats::addmargins()
  t2 <- stats::xtabs(~ dd$result + dd$site) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    icds = icds,
    specimen_types = specimen_types,
    sites = sites,
    numerator = t1,
    denominator = t2
  ), class = c("champs_computed", "pmi_by_decode_site"))
}

#' Calculate average postmortem interval (PMI - average time from death to MITS
#'   in hours) by age and DeCoDe result
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param icds A vector of ICD10 codes to check for the DeCoDe result of
#'   "Contributing (P2)".
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )
#' calc_pmi_by_age_decode(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
#'   specimen_types = specimen_types)

calc_pmi_by_age_decode <- function(d, condition, pathogen, icds,
  specimen_types = NULL, sites = NULL) {

  check_champs_data(d)

  dd <- get_pmi_decode_data(d, condition, pathogen, icds, specimen_types, sites)

  # dd$age_group <- fct_other(dd$casetype_ofcl,
  #   drop = c(
  #     "Death in the first 24 hours",
  #     "Early Neonate (1 to 6 days)",
  #     "Late Neonate (7 to 27 days)"
  #   ),
  #   other_level = "Neonates"
  # )

  # dd$age_group <- forcats::fct_relevel(dd$age_group,
  #   c(
  #     "Stillbirth",
  #     "Neonates",
  #     "Infant (28 days to less than 12 months)",
  #     "Child (12 months to less than 60 Months)"
  #   )
  # )

  t1 <- stats::xtabs(dd$calc_postmortem_hrs ~ dd$age_group + dd$result) %>%
    stats::addmargins()
  t2 <- stats::xtabs(~ dd$age_group + dd$result) %>%
    stats::addmargins()

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    icds = icds,
    specimen_types = specimen_types,
    sites = sites,
    numerator = t1,
    denominator = t2
  ), class = c("champs_computed", "pmi_by_age_decode"))
}

# take cases filtered positive TAC for GBS but GBS not equal 1
# (not in causal chain) and infectious cause = 1 and then see
# what pathogens they are

#' Tabulate top other pathogens found in the causal chain for cases that have
#'   positive TAC results for a given pathogen
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param pathogen A string specifying the pathogen to count positive cases of
#'   in the TAC results.
#' @param icds A vector of ICD10 codes to check for the DeCoDe result of
#'   "Contributing (P2)".
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @export
#' @examples
#' specimen_types <- c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen"
#' )

#' calc_top_tac_pathogens(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
#'   specimen_types = specimen_types)

calc_top_tac_pathogens <- function(d, condition, pathogen, icds,
  specimen_types = NULL, sites = NULL) {

  check_champs_data(d)

  # get ids of all cases that are positive for the pathogen
  # and match the specified specimen types
  tac_ids <- d$tac_long %>%
    dplyr::filter(
      .data$pathogen == !!pathogen
      & .data$result == "Positive"
      & .data$specimen_type %in% !!specimen_types) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # get ids for all cases that don't have the specified condition
  # and have an infectious cause in the causal chain
  # we first need to get ids that *do* have the condition
  # (since there are multiple rows per id in the pivot table)
  tmp_ids <- d$dcd_long %>%
    dplyr::filter(
      .data$etiol == condition | .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # now get all who do not have the condition but have an infectious cause
  dcd_ids <- d$dcd_long %>%
    dplyr::filter(
      !.data$champs_deid %in% tmp_ids
      & .data$champs_group_desc %in% infectious_causes) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique()

  # get ids that are contributing to causal chain based on provided ICDs
  tmp <- dplyr::select(d$dcd,
    tidyselect::starts_with("other_significant"))
  ccc_ids <- d$dcd$champs_deid[apply(tmp, 1,
    function(x) sum(x %in% icds, na.rm = TRUE)) == 0]

  # now get intersection of these three
  ids <- intersect(tac_ids, dcd_ids)
  ids <- intersect(ids, ccc_ids)
  dd <- d$dmg %>%
    dplyr::filter(.data$champs_deid %in% ids)

  # filter to specified sites
  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  # now count how many DeCoDe results match TAC pathogens
  # need to check champs_group_desc and etiol independently
  # NOTE: what about instances where DeCoDe values are coded differently
  #  than in TAC? (e.g. "Streptococcus agalactiae" vs. "Group B Streptococcus")
  vals1 <- d$dcd_long %>%
    dplyr::filter(
      .data$champs_deid %in% dd$champs_deid
      & .data$champs_group_desc %in% valid_pathogens()) %>%
    dplyr::select(.data$champs_deid, .data$champs_group_desc) %>%
    dplyr::rename(value = "champs_group_desc")

  vals2 <- d$dcd_long %>%
    dplyr::filter(
      .data$champs_deid %in% dd$champs_deid
      & .data$etiol %in% valid_pathogens()) %>%
    dplyr::select(tidyselect::one_of(c("champs_deid", "etiol"))) %>%
    dplyr::rename(value = "etiol")

  # for all distinct cases, count how many times each pathogen appears
  res <- dplyr::bind_rows(vals1, vals2) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      value = dplyr::recode(.data$value,
        `Candida glabrata` = "Candida spp",
        `Candida albicans` = "Candida spp",
        `Candida` = "Candida spp",
        `Candida Krusei` = "Candida spp",
        `candida sp` = "Candida spp",
        `Candida spp.` = "Candida spp",
        `Candida tropicalis` = "Candida spp")
    ) %>%
    dplyr::group_by(.data$value) %>%
    dplyr::tally() %>%
    dplyr::arrange(-.data$n)

  structure(list(
    version = d$version,
    condition = condition,
    pathogen = pathogen,
    icds = icds,
    specimen_types = specimen_types,
    sites = sites,
    df = res,
    n = nrow(dd)
  ), class = c("champs_computed", "top_pathogens"))
}

#' Tabulate top pathogens associated with a condition by site of acquisition
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param age_groups Optional vector of age groups.
#'   See \code{\link{valid_age_groups}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_top_dcd_pathogens_by_acq(mock,
#'   condition = "Lower respiratory infections",
#'   age_groups = c(
#'     "Infant (28 days to less than 12 months)",
#'     "Child (12 months to less than 60 Months)")
#' )
calc_top_dcd_pathogens_by_acq <- function(
  d, condition, age_groups = NULL, sites = NULL
) {
  check_champs_data(d)

  age_groups <- check_valid_age_groups(d, age_groups)
  condition <- check_valid_condition(d, condition)

  dd <- dplyr::left_join(d$dcd_long, d$dmg, by = "champs_deid")

  if (!is.null(sites)) {
    sites <- check_valid_sites(d, sites)
    dd <- dplyr::filter(dd, .data$site %in% sites)
    exclude <- dplyr::setdiff(levels(dd$site), sites)
    dd$site <- forcats::fct_drop(dd$site, only = exclude)
  }

  # TODO: condition should only be in champs_group_desc - check that

  dd2 <- dd %>%
    dplyr::filter(.data$champs_group_desc == condition &
      .data$age_group %in% age_groups)

  if (nrow(dd2) == 0) {
    message("No records have champs_group_desc '", condition, "'")
    return()
  }

  res <- dd2 %>% group_by(.data$etiol, .data$acquired48) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    tidyr::pivot_wider(names_from = .data$acquired48, values_from = .data$n) %>%
    dplyr::mutate(Total = .data$Community + .data$Facility) %>%
    dplyr::arrange(-.data$Total) %>%
    dplyr::select(tidyselect::one_of(c("etiol", "Community", "Facility", "Total"))) %>%
    dplyr::filter(.data$etiol != "Other Etiology/Agent")

  structure(list(
    version = d$version,
    table = res
  ), class = c("champs_computed", "calc_top_dcd_pathogens_by_acq"))
}

#' Tabulate TAC top pathogens detected and causal chain
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param age_groups Optional vector of age groups.
#'   See \code{\link{valid_age_groups}}.
#' @param specimen_types An optional vector of specimen types to include as columns in the table.
#' @param specimen_abbrv An optional vector of abbreviations to use for the specified \code{specimen_types}.
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_top_tac_pathogens_cc(mock,
#'   condition = "Lower respiratory infections",
#'   age_groups = c(
#'     "Infant (28 days to less than 12 months)",
#'     "Child (12 months to less than 60 Months)"),
#'   specimen_types = c(
#'     "Nasopharyngeal and Oropharyngeal swab",
#'     "Tissue specimen from lung"),
#'   specimen_abbrv = c("# NP+", "# Lung+")
#' )

calc_top_tac_pathogens_cc <- function(
  d, condition, specimen_types = NULL, specimen_abbrv = NULL,
  age_groups = NULL, sites = NULL
) {
  check_champs_data(d)
  condition <- check_valid_condition(d, condition)
  specimen_types <- check_valid_specimen_types(d, specimen_types)
  age_groups <- check_valid_age_groups(d, age_groups)
  # TODO: handle age groups/subgroups appropriately

  if (is.null(specimen_abbrv))
    specimen_abbrv <- specimen_types

  nn <- left_join(d$tac, d$dmg, by = "champs_deid") %>%
    dplyr::filter(.data$age_group %in% age_groups) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique() %>%
    length()

  dcd_longj <- d$dcd_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid")

  nn2 <- dcd_longj %>%
    dplyr::filter(
      .data$age_group %in% age_groups &
      .data$champs_group_desc == condition) %>%
    dplyr::pull(.data$champs_deid) %>%
    unique() %>%
    length()

  specimen_abbrv <- paste0(specimen_abbrv, " (n=", nn, ")")

  rcd <- specimen_abbrv
  if (is.null(rcd))
    rcd <- specimen_types
  names(rcd) <- specimen_types

  # TODO: add filter by sites...
  # if (!is.null(sites)) {
  #   sites <- check_valid_sites(d, sites)
  #   dd <- dplyr::filter(dd, .data$site %in% sites)
  #   exclude <- dplyr::setdiff(levels(dd$site), sites)
  #   dd$site <- forcats::fct_drop(dd$site, only = exclude)
  # }

  res1 <- d$tac_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid") %>%
    dplyr::filter(
      .data$age_group %in% age_groups &
      !is.na(.data$pathogen) &
      .data$result == "Positive" &
      .data$specimen_type %in% specimen_types) %>%
    dplyr::group_by(.data$specimen_type, .data$pathogen) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    dplyr::arrange(-.data$n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(specimen_type = recode(.data$specimen_type, !!!rcd)) %>%
    tidyr::pivot_wider(names_from = .data$specimen_type, values_from = .data$n)

  nm <- paste0("# in CC (n=", nn, ")")
  res2 <- dcd_longj %>%
    dplyr::filter(
      .data$age_group %in% age_groups &
      .data$etiol != "Other Etiology/Agent") %>%
    dplyr::group_by(.data$etiol) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    dplyr::arrange(-.data$n) %>%
    dplyr::rename(pathogen = "etiol", !!nm := "n")

  nm <- paste0("# in CC as LRI (n=", nn2, ")")
  res3 <- dcd_longj %>%
    dplyr::filter(
      .data$age_group %in% age_groups &
      .data$champs_group_desc == "Lower respiratory infections" &
      .data$etiol != "Other Etiology/Agent") %>%
    dplyr::group_by(.data$etiol) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    dplyr::arrange(-.data$n) %>%
    dplyr::rename(pathogen = "etiol", !!nm := "n")

  res <- res1 %>%
    dplyr::left_join(res2, by = "pathogen") %>%
    dplyr::left_join(res3, by = "pathogen")

  structure(list(
    version = d$version,
    table = res,
    n = nn,
    n_condition = nn2
  ), class = c("champs_computed", "calc_top_tac_pathogens_cc"))
}

#' Tabulate top etiologies by specified age groups
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param age_groups Optional vector of age groups.
#'   See \code{\link{valid_age_groups}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_top_etiol_by_age(mock,
#'   age_groups = c(
#'     "Death in the first 24 hours",
#'     "Early Neonate (24-72 hours)",
#'     "Early Neonate (72+hrs to 6 days)",
#'     "Late Neonate (7 to 27 days)")
#' )
calc_top_etiol_by_age <- function(
  d, age_groups = NULL, sites = NULL
) {
  check_champs_data(d)

  age_groups <- check_valid_age_groups(d, age_groups)
  age_groups_var <- attr(age_groups, "group_var")

  dcd_longj <- d$dcd_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid") %>%
    dplyr::group_by(.data$champs_deid) %>%
    mutate(infectious = any(.data$champs_group_desc %in% infectious_causes)) %>%
    dplyr::ungroup()

  # TODO: support site

  res <- dcd_longj %>%
    dplyr::mutate(
      etiol = recode(.data$etiol,
        `Candida glabrata` = "Candida spp",
        `Candida albicans` = "Candida spp",
        `Candida glabrata` = "Candida spp",
        `Candida albicans` = "Candida spp",
        `Candida` = "Candida spp",
        `Candida Krusei` = "Candida spp",
        `candida sp` = "Candida spp",
        `Candida spp.` = "Candida spp",
        `Candida tropicalis` = "Candida spp")
    ) %>%
    dplyr::filter(.data[[age_groups_var]] %in% age_groups &
      .data$infectious) %>%
    dplyr::group_by(.data[[age_groups_var]], .data$etiol) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    # filter(etiol %in% incl) %>%
    dplyr::filter(.data$etiol != "Other Etiology/Agent") %>%
    dplyr::arrange(.data[[age_groups_var]], .data$etiol, -.data$n)
    # mutate(
    #   etiol = recode(etiol, !!!rcd),
    #   etiol = factor(etiol, levels = incl_names))

  top <- res %>%
    dplyr::group_by(.data$etiol) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::filter(.data$etiol != "Other Etiology/Agent") %>%
    dplyr::arrange(-.data$n)

  # denominators
  denoms <- dcd_longj %>%
    dplyr::filter(.data[[age_groups_var]] %in% age_groups) %>%
    dplyr::group_by(.data[[age_groups_var]]) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid)))

  # no etiology
  tmp <- dcd_longj %>%
    dplyr::filter(.data[[age_groups_var]] %in% age_groups &
      .data$infectious &
      is.na(.data$etiol)) %>%
    group_by(.data[[age_groups_var]], .data$champs_deid) %>%
    tally()
  noet <- tmp %>%
    dplyr::filter(.data$n == max(tmp$n)) %>%
    dplyr::group_by(.data[[age_groups_var]]) %>%
    dplyr::tally()

  structure(list(
    version = d$version,
    etiol_counts = res,
    top = top,
    denominators = denoms,
    no_etiol = noet
  ), class = c("champs_computed", "calc_top_etiol_by_age"))
}

# if (is.null(incl_names))
#   incl_names <- incl
# rcd <- incl_names
# if (is.null(rcd))
#   rcd <- incl
# names(rcd) <- incl

# incl <- c("Acinetobacter baumannii",
#   "Staphylococcus aureus",
#   "Candida spp",
#   "Klebsiella pneumoniae",
#   "Escherichia coli",
#   "Streptococcus agalactiae")

# incl_names <- c("A. baumanii", "S. aureus", "Candida spp",
#   "K. pneumoniae", "E. coli", "Streptococcus agalactiae")

#' Tabulate cases with condition in causal chain by age and site of acquisition
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param age_groups Optional vector of age groups.
#'   See \code{\link{valid_age_groups}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_cc_allcases_by_age_acq(mock,
#'   condition = "Klebsiella pneumoniae",
#'   age_groups = valid_age_subcats(mock)
#' )
calc_cc_allcases_by_age_acq <- function(
  d, condition, age_groups = NULL, sites = NULL
) {
  check_champs_data(d)
  condition <- check_valid_condition(d, condition)

  age_groups <- check_valid_age_groups(d, age_groups)
  age_groups_var <- attr(age_groups, "group_var")

  dcd_longj <- d$dcd_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid")

  res <- dcd_longj %>%
    dplyr::filter(.data$etiol == condition) %>%
    dplyr::group_by(.data[[age_groups_var]], .data$acquired48) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    tidyr::pivot_wider(names_from = .data$acquired48, values_from = .data$n,
      values_fill = list(n = 0)) %>%
    dplyr::mutate(Total = .data$Community + .data$Facility) %>%
    dplyr::select(tidyselect::one_of(c(age_groups_var,
      "Community", "Facility", "Total"))) %>%
    dplyr::arrange(.data[[age_groups_var]])

  # denominator
  denoms <- dcd_longj %>%
    dplyr::group_by(.data[[age_groups_var]]) %>%
    dplyr::summarise(n = length(unique(.data$champs_deid))) %>%
    dplyr::arrange(.data[[age_groups_var]])

  structure(list(
    version = d$version,
    table = res,
    denominators = denoms
  ), class = c("champs_computed", "calc_cc_allcases_by_age_acq"))
}

#' Tabulate syndrome combinations for a specified condition
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param syndrome_names A vector specifying conditions to tabulate
#'   combinations of presence of for the given \code{condition}
#' @param syndrome_values An optional vector of the same length of
#'   \code{syndrome_names} that specifies the mapping from the official
#'   condition names to the values to use for reporting. See example.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_syndrome_combinations(mock,
#'   condition = "Streptococcus pneumoniae",
#'   syndrome_names = c(
#'     "Lower respiratory infections",
#'     "Meningitis/Encephalitis",
#'     "Neonatal sepsis",
#'     "Congenital infection"),
#'   syndrome_values = c(
#'     "Pneumonia",
#'     "Meningitis",
#'     "Sepsis",
#'     "Sepsis"),
#'   specimen_types = c(
#'     "Cerebrospinal fluid sample",
#'     "Tissue specimen from lung",
#'     "Whole blood")
#' )
calc_syndrome_combinations <- function(
  d, condition, syndrome_names, syndrome_values = NULL,
  specimen_types = NULL, sites = NULL
) {
  check_champs_data(d)
  condition <- check_valid_condition(d, condition)
  # TODO: check syndrome_names conditions as well...

  rcd <- syndrome_values
  if (is.null(rcd))
    rcd <- syndrome_names
  names(rcd) <- syndrome_names

  # TODO: sites

  specimen_types <- check_valid_specimen_types(d, specimen_types)

  dcd_longj <- d$dcd_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid")

  tmp <- dcd_longj %>%
    dplyr::filter(.data$etiol == condition
      & .data$champs_group_desc %in% infectious_causes) %>%
    dplyr::mutate(
      champs_group_desc = dplyr::recode(.data$champs_group_desc, !!!rcd)) %>%
    dplyr::group_by(.data$champs_deid) %>%
    dplyr::mutate(syndrome = paste(sort(unique(.data$champs_group_desc)),
      collapse = ", ")) %>%
    dplyr::group_by(.data$syndrome, .data$champs_deid) %>%
    dplyr::tally()

  res <- tmp %>%
    dplyr::group_by(.data$syndrome) %>%
    dplyr::tally() %>%
    dplyr::arrange(-.data$n)

  age_breakdown <- d$dmg %>%
    dplyr::filter(.data$champs_deid %in% tmp$champs_deid) %>%
    dplyr::group_by(.data$age_group) %>%
    dplyr::tally()

  tmp <- dplyr::filter(d$tac_long, .data$champs_deid %in% d$dcd$champs_deid)

  tmp2 <- tmp %>%
    dplyr::filter(
      .data$specimen_type %in% specimen_types &
      # .data$condition == !!condition &
      .data$result == "Positive")

  # unique(tmp2$specimen_type)
  # length(unique(tmp2$champs_deid)) / length(unique(tmp$champs_deid))

  tac_age_breakdown <- tmp2 %>%
    dplyr::left_join(d$dmg, by = "champs_deid") %>%
    dplyr::group_by(.data$age_group) %>%
    dplyr::tally() %>%
    dplyr::mutate(pct = 100 * .data$n / sum(.data$n))

  structure(list(
    version = d$version,
    table = res,
    age_breakdown = age_breakdown,
    cc_leading_to_death = list(
      numerator = sum(res$n),
      denominator = nrow(d$dmg),
      pct = sum(res$n) / nrow(d$dmg) * 100
    ),
    tac_age_breakdown = tac_age_breakdown
  ), class = c("champs_computed", "calc_syndrome_combinations"))
}


# #' TODO
# #'
# #' @param d A data object obtained from \code{\link{load_data}}.
# #' @param pathogen A string specifying the pathogen to count cases of
# #'   in the causal chain.
# #' @param condition A string specifying the condition to tabulate causal chain
# #'   presence for. To list all possibilities, see
# #'   \code{\link{valid_conditions}}.
# #' @param syndrome_values An optional vector of the same length of
# #'   \code{conditions} that specifies the mapping from the official
# #'   condition names to the values to use for reporting. See example.
# #' @param specimen_types An optional vector of specimen types to include in the
# #'   calculation. If not provided, all specimen types will be used. See
# #'   \code{\link{valid_specimen_types}}.
# #' @param sites An optional vector of site names to include in the tabulation.
# #'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
# #' @export
# #' @examples
# 
# # calc_syndrome_combinations(mock, pathogen = "Streptococcus pneumoniae")
# 
# calc_syndrome_combinations <- function(
#   d, pathogen, conditions, syndrome_values = NULL,
#   specimen_types = NULL, sites = NULL
# ) {
#   check_champs_data(d)

#   # TODO: sites

#   res <- d$dcd_long %>%
#     filter(etiol == pathogen) %>%
#     group_by(age_group, acquired48) %>%
#     summarise(n = length(unique(champs_deid))) %>%
#     dplyr::ungroup() %>%
#     tidyr::complete(age_group, acquired48, fill = list(n = 0)) %>%
#     tidyr::pivot_wider(names_from = acquired48, values_from = n) %>%
#     mutate(Total = Community + Facility) %>%
#     select(age_group, Community, Facility, Total) %>%
#     arrange(age_group)

#   structure(list(
#     table = res
#   ), class = c("champs_computed", "calc_syndrome_combinations"))
# }


#' Tabulate cases for a condition by age and syndrome
#'
#' @param d A data object obtained from \code{\link{load_data}}.
#' @param condition A string specifying the condition to count cases of
#'   in the causal chain.
#' @param condition A string specifying the condition to tabulate causal chain
#'   presence for. To list all possibilities, see
#'   \code{\link{valid_conditions}}.
#' @param syndrome_names A vector specifying conditions to tabulate
#'   combinations of presence of for the given \code{condition}
#' @param syndrome_values An optional vector of the same length of
#'   \code{syndrome_names} that specifies the mapping from the official
#'   condition names to the values to use for reporting. See example.
#' @param specimen_types An optional vector of specimen types to include in the
#'   calculation. If not provided, all specimen types will be used. See
#'   \code{\link{valid_specimen_types}}.
#' @param sites An optional vector of site names to include in the tabulation.
#'   If not provided, all sites will be used. See \code{\link{valid_sites}}.
#' @export
#' @examples
#' calc_cc_by_age_syndrome(mock,
#'   condition = "Streptococcus pneumoniae",
#'   syndrome_names = c(
#'     "Lower respiratory infections",
#'     "Meningitis/Encephalitis",
#'     "Neonatal sepsis",
#'     "Congenital infection"),
#'   syndrome_values = c(
#'     "Pneumonia",
#'     "Meningitis",
#'     "Sepsis",
#'     "Sepsis")
#' )
calc_cc_by_age_syndrome <- function(
  d, condition, syndrome_names, syndrome_values = NULL,
  specimen_types = NULL, sites = NULL
) {
  check_champs_data(d)
  condition <- check_valid_condition(d, condition)
  # TODO: check syndrome_names conditions as well...

  rcd <- syndrome_values
  if (is.null(rcd))
    rcd <- syndrome_names
  names(rcd) <- syndrome_names

  # TODO: sites

  dcd_longj <- d$dcd_long %>%
    dplyr::left_join(d$dmg, by = "champs_deid")

  res <- dcd_longj %>%
    dplyr::filter(
      .data$etiol == condition
      & .data$champs_group_desc %in% infectious_causes) %>%
    dplyr::mutate(
      champs_group_desc = recode(.data$champs_group_desc, !!!rcd)) %>%
    dplyr::group_by(.data$champs_deid) %>%
    dplyr::mutate(syndrome = paste(sort(unique(.data$champs_group_desc)),
      collapse = ", ")) %>%
    dplyr::group_by(.data$syndrome, .data$age_group, .data$champs_deid) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    tidyr::complete(.data$age_group, fill = list(n = 0)) %>%
    dplyr::group_by(.data$syndrome, .data$age_group) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(names_from = .data$syndrome, values_from = .data$n,
      values_fill = list(n = 0)) %>%
    dplyr::arrange(match(.data$age_group, levels(dcd_longj$age_group)))

  structure(list(
    version = d$version,
    table = res
  ), class = c("champs_computed", "calc_cc_by_age_syndrome"))
}
