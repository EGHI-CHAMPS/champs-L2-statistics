# do this once right now assuming all functions are currently correct

dummy_data <- readRDS("data-raw/dummy_data.rds")

comp_dat <- list()

specimen_types <- c(
  "Cerebrospinal fluid sample",
  "Tissue specimen from lung",
  "Whole blood",
  "Rectal swab",
  "Plasma or spun blood specimen"
)

comp_dat$calc_cc_allcases_by_site_age <- calc_cc_allcases_by_site_age(d,
  condition = "aaa")

comp_dat$calc_cc_detected_by_site_age <- calc_cc_detected_by_site_age(d,
  condition = "Streptococcus agalactiae",
  pathogen = "Group B Streptococcus",
  specimen_types = specimen_types)

# ...

# saveRDS(comp_dat, file = "data-raw/test_comp_dat.rds")
