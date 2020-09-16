check_champs_data <- function(d) {
  if (!inherits(d, "champs_data"))
    stop("Argument 'd' must be created from load_data().")
}

check_champs_object <- function(x) {
  if (!inherits(x, "champs_computed"))
    stop("Argument must be a valid computed CHAMPS object.")
}

#' Get a list of valid conditions
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_conditions(d)
#' }
valid_conditions <- function(d) {
  list(
    champs_group_desc = sort(unique(d$dcd_long$champs_group_desc)),
    etiol = sort(unique(d$dcd_long$etiol))
  )

  # short <- unlist(lapply(rules, function(x) {
  #   if (x$method == "champs_group") {
  #     return(c(x$var, paste0(x$var, "Sum")))
  #   } else {
  #     return(x$var)
  #   }
  # }))
  # long <- unlist(lapply(rules, function(x) {
  #   if (x$method == "champs_group") {
  #     return(c(x$check, x$check))
  #   } else {
  #     return(x$check)
  #   }
  # }))
  # tibble::tibble(short = short, long = long)
}

check_valid_condition <- function(d, condition) {
  # This will be relaxed in a future version
  if (length(condition) > 1)
    stop("Currently only one condition at a time is supported.", call. = FALSE)
  vc <- valid_conditions(d)
  # cur_form <- "short"
  # sd <- dplyr::setdiff(condition, vc$short)
  # if (length(sd) > 0) {
  #   cur_form <- "long"
  #   sd <- dplyr::setdiff(condition, vc$long)
  #   if (length(sd) > 0)
  #     stop("Invalid condition", ifelse(length(sd) > 1, "s", ""),
  #       ": ", paste(sd, collapse = ", "), "\n",
  #       "See valid_conditions().", call. = FALSE)
  # }
  # if (form == "short" && cur_form == "long")
  #   condition <- vc$short[which(vc$long == condition)[1]]
  # if (form == "long" && cur_form == "short")
  #   condition <- vc$long[which(vc$short == condition)[1]]
  if (!(condition %in% vc$champs_group_desc || condition %in% vc$etiol))
    stop("Invalid condition",
      ": ", condition, "\n",
      "See valid_conditions().", call. = FALSE)

  condition
}

#' Get a list of valid site names based on the provided CHAMPS data
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_sites(d)
#' }
valid_sites <- function(d) {
  check_champs_data(d)
  sort(levels(d$dmg$site))
}

check_valid_sites <- function(d, sites) {
  vs <- valid_sites(d)
  if (is.null(sites))
    return(vs)

  sd <- dplyr::setdiff(sites, vs)
  if (length(sd) > 0)
    stop("Invalid site", ifelse(length(sd) > 1, "s", ""),
      ": ", paste(sd, collapse = ", "), "\n",
      "See valid_sites().", call. = FALSE)

  sites
}

#' Get a list of valid TAC results pathogens based on the provided CHAMPS data
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_pathogens()
#' }
#' @export
valid_pathogens <- function() {
  sort(champs::tac_table$assay)
}

check_valid_pathogen <- function(d, pathogen) {
  # This will be relaxed in a future version
  if (length(pathogen) > 1)
    stop("Currently only one pathogen at a time is supported.", call. = FALSE)

  vp <- valid_pathogens()
  sd <- dplyr::setdiff(pathogen, vp)
  if (length(sd) > 0)
    stop("Invalid pathogen", ifelse(length(sd) > 1, "s", ""),
      ": ", paste(sd, collapse = ", "), "\n",
      "See valid_pathogens().", call. = FALSE)

  pathogen
}

#' Get a list of valid TAC results specimen types based on the provided
#'   CHAMPS data
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_specimen_types(d)
#' }
valid_specimen_types <- function(d) {
  sort(unique(d$tac_long$specimen_type))
}

check_valid_specimen_types <- function(d, specimen_types) {
  vst <- valid_specimen_types(d)
  if (is.null(specimen_types))
    return(vst)

  sd <- dplyr::setdiff(specimen_types, vst)
  if (length(sd) > 0)
    stop("Invalid specimen_types",
      ": ", paste(sd, collapse = ", "), "\n",
      "See valid_specimen_types().", call. = FALSE)

  specimen_types
}

#' Get a list of valid age groups based on the provided CHAMPS data
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_age_groups(d)
#' }
valid_age_groups <- function(d) {
  levels(d$dmg$age_group)
}

#' Get a list of valid age subgroups based on the provided CHAMPS data
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_age_subcats(d)
#' }
valid_age_subcats <- function(d) {
  levels(d$dmg$age_group_subcat)
}

check_valid_age_groups <- function(d, age_groups) {
  vag <- valid_age_groups(d)
  vasg <- valid_age_subcats(d)

  if (is.null(age_groups)) {
    age_groups <- vag
    attr(age_groups, "group_var") <- "age_group"
    return(age_groups)
  }

  sd <- dplyr::setdiff(age_groups, vag)
  if (length(sd) > 0) {
    sd <- dplyr::setdiff(age_groups, vasg)
    if (length(sd) > 0) {
      stop("Invalid age_groups",
        ": ", paste(sd, collapse = ", "), "\n",
        "See valid_age_groups() or valid_age_subcats().",
        call. = FALSE)
    } else {
      attr(age_groups, "group_var") <- "age_group_subcat"
      return(age_groups)
    }
  }

  attr(age_groups, "group_var") <- "age_group"
  age_groups
}

#' Get a list of DeCoDe "Other Significant Cause" ICD 10 codes that exist in the
#'   CHAMPS data
#' @param d A data object obtained from \code{\link{load_data}}.
#' @export
#' @examples
#' \dontrun{
#' d <- load_data(...)
#' valid_icds(d)
#' }
valid_icds <- function(d) {
  d$dcd %>%
    dplyr::select(
      dplyr::matches("^morbid_condition_0[1-8]$|^other_significant_condition_0[1-8]$|^immediate_cod$|^underlying_cause$")) %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()
}
