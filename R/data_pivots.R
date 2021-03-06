# @param x wide format DeCoDe data frame
# @param gvar champs_group variable (ic, uc, morbid_cond)
# @param evar etiology variable (there are 3 for each)
# @param type "immediate_cause", "underlying_cause", "morbid_condition"
get_etiol_data <- function(x, gvar, evar, type) {
  evars <- paste0(evar, 1:3)
  cols <- c("champs_deid", gvar, evars)
  x %>%
    dplyr::select(tidyselect::one_of(cols)) %>%
    dplyr::mutate(type = type) %>%
    tidyr::pivot_longer(cols = tidyselect::one_of(evars),
      values_to = "etiol") %>%
    dplyr::rename(champs_group_desc = !!gvar) %>%
    dplyr::mutate(etiol_num =
      as.integer(substr(.data$name, nchar(.data$name), nchar(.data$name)))) %>%
    dplyr::select(-.data$name)
}

build_dcd_long <- function(dcd) {
  dplyr::bind_rows(c(
    list(get_etiol_data(dcd,
      gvar = "ic_champs_group_desc",
      evar = "immediate_cause_of_death_etiol",
      type = "immediate_cause"
    )),
    list(get_etiol_data(dcd,
      gvar = "uc_champs_group_desc",
      evar = "underlying_cause_factor_etiol",
      type = "underlying_cause"
    )),
    lapply(1:8, function(i) {
      get_etiol_data(dcd,
        gvar = paste0("morbid_cond_0", i, "_champs_group_desc"),
        evar = paste0("morbid_condition_0", i, "_etiol"),
        type = "morbid_condition"
      )
    })
  ))
}

build_tac_long <- function(tac) {
  tac_prfx_map <- list(
    bld_sp = "Plasma or spun blood specimen",
    lung = "Tissue specimen from lung",
    csf = "Cerebrospinal fluid sample",
    bld = "Whole blood",
    np_op = "Nasopharyngeal and Oropharyngeal swab",
    rect_swab = "Rectal swab"
  )

  tac_prfx <- names(tac_prfx_map)
  prfs_rgxp <- paste0("^(", paste(tac_prfx, collapse = "|"), ")")

  tac2 <- tidyr::pivot_longer(tac, tidyselect::matches(prfs_rgxp),
    values_to = "result")

  tac2$specimen_type <- NA
  tac2$target <- NA
  for (val in tac_prfx) {
    rgx <- paste0("^", val, "_")
    idx <- grepl(rgx, tac2$name) & is.na(tac2$specimen_type)
    tac2$specimen_type[idx] <- tac_prfx_map[[val]]
    tac2$target[idx] <- gsub(rgx, "", tac2$name[idx])
  }
  tac2$target <- toupper(tac2$target)
  tac2 <- dplyr::left_join(tac2,
    dplyr::select(champs::tac_table, tidyselect::one_of("code", "assay")) %>%
    dplyr::rename(target = "code", pathogen = "assay"),
    by = "target"
  )

  tac2
  # dplyr::filter(tac2, !is.na(result) &
  #   !grepl("Negative|Invalid|Indeterminate", result))
}
