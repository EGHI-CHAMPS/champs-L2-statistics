#' @importFrom utils str
#' @export
print.champs_data <- function(x, ...) {
  utils::str(x, 1)
}

fix_names <- function(x) {
  nms <- names(x)
  nms <- tolower(nms)
  nms <- gsub(" ", "_", nms)
  names(x) <- nms
  x
}

sm <- suppressMessages

#' Register and download L2 data
#' @importFrom utils browseURL
#' @export
get_l2_data <- function() {
  message("Register and download the data from: ",
    "https://champs.emory.edu/redcap/surveys/?s=PCEERX993Y")
  browseURL("https://champs.emory.edu/redcap/surveys/?s=PCEERX993Y")
}

#' Load and pre-process data from relevant CHAMPS database tables
#'
#' @param data_dir Path to directory containing the de-identified L2 data as
#'   directly downloaded from CHAMPS (see \code{\link{get_l2_data}} for
#'   information on how to access this data). Data files must not be renamed
#'   or modified after downloading.
#' @note The variables in the data are well-documented in the dataset
#'   definition PDF file that comes with the L2 data download.
#' @export
#' @importFrom readr read_csv
load_data <- function(
  data_dir
) {
  files <- c(
    "CHAMPS_dataset_version.csv",
    "CHAMPS_deid_basic_demographics.csv",
    "CHAMPS_deid_decode_results.csv",
    "CHAMPS_deid_lab_results.csv",
    "CHAMPS_deid_tac_results.csv",
    "CHAMPS_deid_verbal_autopsy.csv",
    "CHAMPS_ICD_Mappings.csv",
    "CHAMPS_icd10_descriptions.csv",
    "CHAMPS_vocabulary.csv"
  )
  fnames <- c("version", "dmg", "dcd", "lab", "tac", "va", "icd_map",
    "icd_desc", "vocab")

  if (!dir.exists(data_dir))
    stop("Specified data directory does not exist: ", data_dir, call. = FALSE)

  ff <- list.files(data_dir, full.names = TRUE)
  dff <- setdiff(files, basename(ff))
  if (length(dff) > 0)
    stop("The following files were expected but not found in ",
      data_dir, ":\n", paste(dff, collapse = ", "), call. = FALSE)

  rd <- function(path) {
    message("Reading ", basename(path), "...")

    # for verbal autopsy there are some time vars that it tries to read as dates
    # which(names(x$va) %in% c("id10011", "id10481"))
    # ncol(x$va)
    colspec <- rep("?", 456)
    colspec[c(38, 435)] <- "c"
    colspec <- paste(colspec, collapse = "")

    if (grepl("verbal_autopsy", basename(path))) {
      tmp <- sm(readr::read_csv(path, col_types = colspec,
        guess_max = 100000)) %>%
        fix_names()
    } else {
      tmp <- sm(readr::read_csv(path, guess_max = 100000)) %>%
        fix_names()
    }

    attr(tmp, "spec") <- NULL
    tmp
  }

  ff <- ff[which(basename(ff) %in% files)]

  x <- lapply(ff, rd)
  names(x) <- fnames

  tmp <- x$dmg$age_group_subcat
  idx <- which(is.na(tmp))
  tmp[idx] <- x$dmg$age_group[idx]
  tmp[tmp == "infant_>6M"] <- "Infant (28 days to less than 6 months)"
  tmp[tmp == "infant_6M"] <- "Infant (6 months to less than 12 months)"
  tmp[tmp == "neonate_72h"] <- "Early Neonate (24-72 hours)"
  tmp[tmp == "neonate_>72h"] <- "Early Neonate (72+hrs to 6 days)"

  x$dmg$age_group_subcat <- factor(tmp,
    levels = c(
      "Stillbirth",
      "Death in the first 24 hours",
      "Early Neonate (24-72 hours)",
      "Early Neonate (72+hrs to 6 days)",
      "Late Neonate (7 to 27 days)",
      "Infant (28 days to less than 6 months)",
      "Infant (6 months to less than 12 months)",
      "Child (12 months to less than 60 Months)"))

  x$dmg$age_group <- factor(x$dmg$age_group,
    levels = c(
      "Stillbirth",
      "Death in the first 24 hours",
      "Early Neonate (1 to 6 days)",
      "Late Neonate (7 to 27 days)",
      "Infant (28 days to less than 12 months)",
      "Child (12 months to less than 60 Months)"))

  x$dmg$site <- forcats::fct_relevel(
    recode(x$dmg$site_iso_code,
      BD = "Bangladesh",
      ET = "Ethiopia",
      KE = "Kenya",
      ML = "Mali",
      MZ = "Mozambique",
      SL = "Sierra Leone",
      ZA = "South Africa"
    ),
    c("Bangladesh", "Kenya", "Mali", "Mozambique",
      "South Africa", "Ethiopia", "Sierra Leone"))

  starts <- c(-Inf, 0, 4, 7, 10, 13, 16, 19, 22, 25, Inf)
  labels <- c("bad", "0 to 3", "4 to 6", "7 to 9", "10 to 12",
    "13 to 15", "16 to 18", "19 to 21", "22 to 24", "Over 24h")
  tmp <- cut(x$dmg$calc_postmortem_hrs, starts - 0.5, labels)
  tmp[tmp == "bad"] <- NA
  tmp <- droplevels(tmp)
  x$dmg$pmi_range <- tmp

  x$dmg <- x$dmg %>%
    dplyr::mutate(
      hosp_los_24h2 = ifelse(is.na(.data$hosp_los_24h), 0, .data$hosp_los_24h),
      hosp_los_48h2 = ifelse(is.na(.data$hosp_los_48h), 0, .data$hosp_los_48h),
      acquired24 = ifelse(
        .data$age_group == "Death in the first 24 hours" |
        .data$location_of_death == "community" |
        .data$hosp_los_24h2 == 1, "Community", "Facility"),
      acquired48 = ifelse(
        .data$age_group == "Stillbirth" |
        .data$age_group == "Death in the first 24 hours" |
        .data$location_of_death == "community" |
        .data$hosp_los_24h2 == 1 |
        .data$hosp_los_48h2 == 1, "Community", "Facility"),
    ) %>%
    dplyr::select(-.data$hosp_los_24h2, -.data$hosp_los_48h2)

  # # extra processing
  # message("Building joined DeCoDe and demographics table...")
  # x$dcd_join <- add_variables(x$dcd, x$dmg)

  message("Building TAC long...")
  x$tac_long <- build_tac_long(x$tac)

  # TODO: add etiol_tac column to dcd_long that maps...
  # message("Building DeCoDe pivot...")
  # # incl are variables to pull into pivot table from demographics
  # incl <- c("age_group", "age_group_subcat",
  #   "acquired48", "infectious_cause", "site")
  # x$dcd_long <- build_dcd_long(x$dcd_join, incl)
  message("Building DeCoDe long...")
  x$dcd_long <- build_dcd_long(x$dcd)

  # fix cases that are incorrectly labeled as "Haemophilus influenzae Type B"
  if (x$version$dataset_version == "4.1") {
    idx <- which(x$dcd_long$etiol == "Haemophilus influenzae Type B")
    x$dcd_long$etiol[idx] <- "Haemophilus influenzae"
  }

  class(x) <- c("champs_data", "list")

  message("CHAMPS De-Identified Dataset v", x$version$dataset_version,
    " (", x$version$dataset_release_date, ")")

  x
}
