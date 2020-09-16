library(champs)

d <- readRDS("tests/testthat/evaluation_data_v4.1_2020-08-01.rds")

specimen_types <- c(
  "Cerebrospinal fluid sample",
  "Tissue specimen from lung",
  "Whole blood",
  "Rectal swab",
  "Plasma or spun blood specimen"
)

d_result <- list()

# calc_cc_allcases_by_age_acq()
d_result$calc_cc_allcases_by_age_acq <- calc_cc_allcases_by_age_acq(d, 
  condition = "Klebsiella pneumoniae", age_groups = valid_age_subcats(d))

# calc_cc_allcases_by_site_age()
d_result$calc_cc_allcases_by_site_age <- calc_cc_allcases_by_site_age(d, 
  condition = "Streptococcus agalactiae")

# calc_cc_by_age_syndrome()
d_result$calc_cc_by_age_syndrome <- calc_cc_by_age_syndrome(d,
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
 
# calc_cc_detected_by_site_age()
d_result$calc_cc_detected_by_site_age <- calc_cc_detected_by_site_age(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  specimen_types = specimen_types)

# calc_detected_allcases_by_site_age()
d_result$calc_detected_allcases_by_site_age <- calc_detected_allcases_by_site_age(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  specimen_types = specimen_types)

# calc_detected_by_decode()
d_result$calc_detected_by_decode <- calc_detected_by_decode(d,
  by = "site",
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
  specimen_types = specimen_types)

# calc_pmi_by_age_decode()
d_result$calc_pmi_by_age_decode <- calc_pmi_by_age_decode(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
  specimen_types = specimen_types)

# calc_pmi_by_decode_site()                       ## LOOK AT HELP FILE EXAMPLE (assignment <- used only on this)
d_result$calc_pmi_by_decode_site <- calc_pmi_by_decode_site(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
  specimen_types = specimen_types)

# calc_nspecimen_by_pmi()
d_result$calc_nspecimen_by_pmi <- calc_nspecimen_by_pmi(d, pathogen = "Group B Streptococcus")

# calc_pmi_by_specimen_site()
d_result$calc_pmi_by_specimen_site <- calc_pmi_by_specimen_site(d, pathogen = "Group B Streptococcus")

# calc_syndrome_combinations()                    ## LOOK AT HELP FILE EXAMPLE (bad variable name)
d_result$calc_syndrome_combinations <- calc_syndrome_combinations(d,
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

# calc_top_dcd_pathogens_by_acq()                 
d_result$calc_top_dcd_pathogens_by_acq <- calc_top_dcd_pathogens_by_acq(d,
  condition = "Lower respiratory infections",
  age_groups = c(
    "Infant (28 days to less than 12 months)",
    "Child (12 months to less than 60 Months)"))

# calc_top_etiol_by_age()                     ## LOOK AT HELP FILE EXAMPLE (not correct help file function in example helps files not built?)
d_result$calc_top_etiol_by_age <- calc_top_etiol_by_age(d,
  age_groups = c(
    "Death in the first 24 hours",
    "Early Neonate (24-72 hours)",
    "Early Neonate (72+hrs to 6 days)",
    "Late Neonate (7 to 27 days)"))

# calc_top_tac_pathogens()
d_result$calc_top_tac_pathogens <- calc_top_tac_pathogens(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  icds = c("P36.0", "A40.1", "P23.3", "G00.2"),
  specimen_types = specimen_types)

# calc_top_tac_pathogens_cc()
d_result$calc_top_tac_pathogens_cc <- calc_top_tac_pathogens_cc(d,
  condition = "Lower respiratory infections",
  age_groups = c(
    "Infant (28 days to less than 12 months)",
    "Child (12 months to less than 60 Months)"),
  specimen_types = c(
    "Nasopharyngeal and Oropharyngeal swab",
    "Tissue specimen from lung"),
  specimen_abbrv = c("# NP+", "# Lung+"))

saveRDS(d_result, paste0("tests/testthat/evaluation_result_v", d$version$dataset_version, "_", d$version$dataset_release_date, ".rds"))
