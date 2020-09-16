context("Package")

d <- readRDS("evaluation_data_v4.1_2020-08-01.rds")
comp_dat <- readRDS("evaluation_result_v4.1_2020-08-01.rds")

specimen_types <- c(
  "Cerebrospinal fluid sample",
  "Tissue specimen from lung",
  "Whole blood",
  "Rectal swab",
  "Plasma or spun blood specimen"
)

test_that("Package works", {

  # calc_cc_allcases_by_age_acq()
  res <- calc_cc_allcases_by_age_acq(d,
    condition = "Klebsiella pneumoniae",
    age_groups = valid_age_subcats(d))

  expect_true(all(res$table == comp_dat$calc_cc_allcases_by_age_acq$table,
    na.rm = TRUE))
  expect_true(all(res$denominator ==
    comp_dat$calc_cc_allcases_by_age_acq$denominator, na.rm = TRUE))

  # calc_cc_allcases_by_site_age()
  res <- calc_cc_allcases_by_site_age(d, condition = "Streptococcus agalactiae")

  expect_true(all(res$denominator ==
    comp_dat$calc_cc_allcases_by_site_age$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_cc_allcases_by_site_age$numerator))

  # calc_cc_by_age_syndrome()
  res <- calc_cc_by_age_syndrome(d,
    condition = "Streptococcus pneumoniae",
    syndrome_names = c(
      "Lower respiratory infections",
      "Meningitis/Encephalitis",
      "Neonatal sepsis",
      "Congenital infection"),
    syndrome_values = c(
      "Pneumonia",
      "Meningitis",
      "Sepsis",
      "Sepsis"))

  expect_true(all(res$table ==
    comp_dat$calc_cc_by_age_syndrome$table))
  remove(res)

  # calc_cc_detected_by_site_age()
  res <- calc_cc_detected_by_site_age(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    specimen_types = specimen_types)

  expect_true(all(res$denominator ==
    comp_dat$calc_cc_detected_by_site_age$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_cc_detected_by_site_age$numerator))

  # calc_detected_allcases_by_site_age()
  res <- calc_detected_allcases_by_site_age(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    specimen_types = specimen_types)

  expect_true(all(res$denominator ==
    comp_dat$calc_detected_allcases_by_site_age$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_detected_allcases_by_site_age$numerator))

  # calc_detected_by_decode()
  res <- calc_detected_by_decode(d,
    by = "site",
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
    specimen_types = specimen_types)

  expect_true(all(res$numerator == comp_dat$calc_detected_by_decode$numerator))

  # calc_pmi_by_age_decode()
  res <- calc_pmi_by_age_decode(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
    specimen_types = specimen_types)

  expect_true(all(res$denominator ==
    comp_dat$calc_pmi_by_age_decode$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_pmi_by_age_decode$numerator))

  # calc_pmi_by_decode_site()
  res <- calc_pmi_by_decode_site(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
    specimen_types = specimen_types)

  expect_true(all(res$denominator ==
    comp_dat$calc_pmi_by_decode_site$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_pmi_by_decode_site$numerator))

  # calc_nspecimen_by_pmi()
  res <- calc_nspecimen_by_pmi(d, pathogen = "Group B Streptococcus")

  expect_true(all(res$numerator == comp_dat$calc_nspecimen_by_pmi$numerator))

  # calc_pmi_by_specimen_site()
  res <- calc_pmi_by_specimen_site(d, pathogen = "Group B Streptococcus")

  expect_true(all(res$denominator ==
    comp_dat$calc_pmi_by_specimen_site$denominator))
  expect_true(all(res$numerator ==
    comp_dat$calc_pmi_by_specimen_site$numerator))

  # calc_syndrome_combinations()
  ## LOOK AT HELP FILE EXAMPLE (bad variable name)
  res <- calc_syndrome_combinations(d,
    condition = "Streptococcus pneumoniae",
    syndrome_names = c(
      "Lower respiratory infections",
      "Meningitis/Encephalitis",
      "Neonatal sepsis",
      "Congenital infection"),
    syndrome_values = c(
      "Pneumonia",
      "Meningitis",
      "Sepsis",
      "Sepsis"),
    specimen_types = c(
      "Cerebrospinal fluid sample",
      "Tissue specimen from lung",
      "Whole blood"))

  expect_true(all(res$table ==
    comp_dat$calc_syndrome_combinations$table))
  expect_true(all(res$age_breakdown ==
    comp_dat$calc_syndrome_combinations$age_breakdown))
  expect_true(all(res$tac_age_breakdown ==
    comp_dat$calc_syndrome_combinations$tac_age_breakdown))
  expect_true(all(res$cc_leading_to_death$numerator ==
    comp_dat$cc_leading_to_death$numerator))
  expect_true(all(res$cc_leading_to_death$denominator ==
    comp_dat$cc_leading_to_death$denominator))
  expect_true(all(res$cc_leading_to_death$pct ==
    comp_dat$cc_leading_to_death$pct))

  # calc_top_dcd_pathogens_by_acq()
  res <- calc_top_dcd_pathogens_by_acq(d,
    condition = "Lower respiratory infections",
    age_groups = c(
      "Infant (28 days to less than 12 months)",
      "Child (12 months to less than 60 Months)"))

  expect_true(all(res$table ==
    comp_dat$calc_top_dcd_pathogens_by_acq$table,
    na.rm = TRUE))  # lots of NA

  # calc_top_etiol_by_age()
  res <- calc_top_etiol_by_age(d,
    age_groups = c(
      "Death in the first 24 hours",
      "Early Neonate (24-72 hours)",
      "Early Neonate (72+hrs to 6 days)",
      "Late Neonate (7 to 27 days)"))

  expect_true(all(res$etiol_counts ==
    comp_dat$calc_top_etiol_by_age$etiol_counts))
  expect_true(all(res$top == comp_dat$calc_top_etiol_by_age$top))
  expect_true(all(res$denominators ==
    comp_dat$calc_top_etiol_by_age$denominators))
  expect_true(all(res$no_etiol ==
    comp_dat$calc_top_etiol_by_age$no_etiol))

  # calc_top_tac_pathogens()
  res <- calc_top_tac_pathogens(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
    specimen_types = specimen_types)

  expect_true(all(res$df == comp_dat$calc_top_tac_pathogens$df))
  expect_true(all(res$n == comp_dat$calc_top_tac_pathogens$n))

  # calc_top_tac_pathogens_cc()
  res <- calc_top_tac_pathogens_cc(d,
    condition = "Lower respiratory infections",
    age_groups = c(
      "Infant (28 days to less than 12 months)",
      "Child (12 months to less than 60 Months)"),
    specimen_types = c(
      "Nasopharyngeal and Oropharyngeal swab",
      "Tissue specimen from lung"),
    specimen_abbrv = c("# NP+", "# Lung+"))

  expect_true(all(res$table ==
    comp_dat$calc_top_tac_pathogens_cc$table, na.rm = TRUE))
  expect_true(all(res$n ==
    comp_dat$calc_top_tac_pathogens_cc$n))
  expect_true(all(res$n_condition ==
    comp_dat$calc_top_tac_pathogens_cc$n_condition))
})

test_that("Plots and Tables work", {
  mock_syndrome <- calc_syndrome_combinations(d,
    condition = "Streptococcus pneumoniae",
    syndrome_names = c(
      "Lower respiratory infections",
      "Meningitis/Encephalitis",
      "Neonatal sepsis",
      "Congenital infection"),
    syndrome_values = c(
      "Pneumonia",
      "Meningitis",
      "Sepsis",
      "Sepsis"),
    specimen_types = c(
      "Cerebrospinal fluid sample",
      "Tissue specimen from lung",
      "Whole blood")
    )

  ptest <- plot_syndrome_combinations(mock_syndrome,
    legend_location = "none")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome,
    legend_location = "none", plot_type = "bar")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome,
    legend_location = "none",
    plot_type = "bar", include_text = "full_percent")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome,
    legend_location = "none", include_text = "full_percent")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome, "age_breakdown",
    legend_location = "none", include_text = "full_percent")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome, "tac_age_breakdown",
    legend_location = "none", include_text = "full_number")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome, "age_breakdown",
    legend_location = "none", include_text = "full_number",
    full_separator = ",")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome, "age_breakdown",
    plot_type = "pie", legend_location = "none",
    include_text = "full_number", full_separator = ",")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_syndrome_combinations(mock_syndrome, "tac_age_breakdown",
    plot_type = "bar", legend_location = "none",
    include_text = "full_number", full_separator = ",")
  expect_true(inherits(ptest, "ggplot"))

  mock_top_etiol_by_age <- calc_top_etiol_by_age(d,
    age_groups = c(
      "Death in the first 24 hours",
      "Early Neonate (24-72 hours)",
      "Early Neonate (72+hrs to 6 days)",
      "Late Neonate (7 to 27 days)"))

  ptest <- plot_top_etiol_by_age(mock_top_etiol_by_age)
  expect_true(inherits(ptest, "ggplot"))

  specimen_types <- c("Cerebrospinal fluid sample",
    "Tissue specimen from lung",
    "Whole blood",
    "Rectal swab",
    "Plasma or spun blood specimen")

  mock_dcd <- calc_cc_allcases_by_site_age(d,
    condition =  "Streptococcus pneumoniae")
  mock_tac <- calc_detected_allcases_by_site_age(d,
    condition =  "Streptococcus pneumoniae", pathogen = "Group B Streptococcus")
  mock_both <- calc_cc_detected_by_site_age(d,
    condition = "Streptococcus agalactiae",
    pathogen = "Group B Streptococcus",
    specimen_types = specimen_types)

  ptest <- plot_margins_site_age(mock_tac, include_x_label = FALSE,
    include_text = "full_fraction",
    plot_title = "TAC Marginal Distributions for Streptococcus pneumoniae")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_margins_site_age(mock_dcd, include_x_label = TRUE,
    include_text = "fraction",
    plot_title = "DeCoDe Marginal Distribution for Streptococcus pneumoniae")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- plot_margins_site_age(mock_both, include_x_label = FALSE,
    include_text = "full_fraction",
    plot_title = "DeCoDe Strep pneumoniae where TAC contains Strep pneumoniae")
  expect_true(inherits(ptest, "ggplot"))

  mock_calc <- calc_cc_allcases_by_site_age(d,
    condition = "Streptococcus agalactiae")

  ptest <- heatmap_site_age(mock_calc, include_text = "percent",
    plot_title = "TAC Results", plot_subtitle = "Streptococcus pneumoniae")
  expect_true(inherits(ptest, "ggplot"))

  ptest <- heatmap_site_age(mock_calc, include_text = "fraction",
    plot_title = "TAC Results", plot_subtitle = "Streptococcus pneumoniae")
  expect_true(inherits(ptest, "ggplot"))

  httest <- html_table_site_age(mock_dcd)

  a <- build_dcd_long(mock$dcd)
  expect_true(all(names(a) %in% c("champs_deid", "champs_group_desc", "type", 
    "etiol", "etiol_num")))
  a <- build_tac_long(mock$tac)
  expect_true(all(names(a) %in% c("champs_deid", "name", "result", 
    "specimen_type", "target", "pathogen")))
})
