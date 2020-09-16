library(champs)
library(dplyr)
library(stringr)

dat <- load_data("/CHAMPS_de_identified_data/")

# Details 2020081252
# version, icd_map, icd_desc, and vocab have no privacy concerns
# dmg, dcd, tac, tac_, and dcd_ are currently used in the package
# lab, va are not currently used in the package

# The demographic data has potential identifiable information even with de-identification.
# NOTE: Change IDs for all tables at the same time.
# 1. Randomizing the site_iso_code and site columns to be with different champs_deids.
# 2. Change champs_deid to shorter names.
# 3. Keep 500 or 600
# 4. keep only champs_deid, site_iso_code, age_group, age_group_subcat,  location_of_death, hosp_<cols>, calc_postmortem_hrs to the end.
# 5. for tac_ get rid of 'negative' in result.


# Drop columns and tables

# dmg column info keep only champs_deid, site_iso_code, age_group, age_group_subcat,  
#   location_of_death, hosp_<cols>, calc_postmortem_hrs to the end.
# Drop lab, va are not currently used in the package

dat$lab <- NULL
dat$va <- NULL

dat$dmg <- dat$dmg %>% select(champs_deid, site_iso_code, age_group, 
  age_group_subcat, location_of_death, contains('hosp_'), calc_postmortem_hrs:acquired48)

# 6. for tac_ get rid of 'negative' in result.

dat$tac_long <- dat$tac_long %>% filter(result != 'Negative')

# 1. Randomizing the site_iso_code and site columns to be with different champs_deids.
# 2. Change champs_deid to shorter names.
# 3. Keep 500 or 600

site_map <- dat$dmg %>%
  select(site_iso_code) %>%
  filter(!duplicated(site_iso_code)) %>%
  mutate(random_iso = sample(LETTERS, 7), 
         random_site = str_c(random_iso, sample(letters, 7), sample(letters, 7), sample(letters, 7), sample(letters, 7)))

id_map <- dat$dmg %>%
  select(champs_deid, site_iso_code) %>%
  mutate(random_id = str_c(sample(LETTERS, n(), replace = TRUE), sample(LETTERS, n(), replace = TRUE), 
                           sample(9, n(), replace = TRUE), sample(9, n(), replace = TRUE), sample(LETTERS, n(), replace = TRUE))) %>%
  group_by(site_iso_code) %>%
  slice_sample(n = 75) %>%
  left_join(site_map) %>%
  ungroup() %>%
  mutate(new_order = sample(n(), n()),
         random_iso = random_iso[new_order], random_site = random_site[new_order])

with(id_map, table(random_site, site_iso_code, useNA = 'ifany'))

# NOTE: Change IDs for all tables at the same time.

dat$dmg <- id_map %>%
  select(-new_order, -site_iso_code) %>%
  left_join(dat$dmg) %>%
  select(-champs_deid, -site, -site_iso_code) %>%
  select(champs_deid = random_id, site_iso_code = random_iso, site = random_site, everything())

dat$dcd <- id_map %>%
  select(-new_order, -site_iso_code, -random_iso, -random_site) %>%
  left_join(dat$dcd) %>% #only change from above
  select(-champs_deid) %>%
  select(champs_deid = random_id, everything())

dat$tac <- id_map %>%
  select(-new_order, -site_iso_code, -random_iso, -random_site) %>%
  left_join(dat$tac) %>% #only change from above
  select(-champs_deid) %>%
  select(champs_deid = random_id, everything())

dat$tac_long <- dat$tac_long %>%
  inner_join(select(id_map, -new_order, -site_iso_code, -random_iso, -random_site)) %>%
  select(-champs_deid) %>%
  select(champs_deid = random_id, everything())

dat$dcd_long <- dat$dcd_long %>%
  inner_join(select(id_map, -new_order, -site_iso_code, -random_iso, -random_site)) %>%
  select(-champs_deid) %>%
  select(champs_deid = random_id, everything())

dat$dmg$site <- factor(dat$dmg$site)

saveRDS(dat, str_c("tests/testthat/evaluation_data_v", dat$version$dataset_version, "_", dat$version$dataset_release_date, ".rds"))
