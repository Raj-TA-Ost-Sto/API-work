# Eurostat - processing of data
# 2026-08-11 
# Amelia Bodin

# Could combine this code with the "Eurostat.R" API-call file to keep everything combined?

#############################Packages###########################################
library(readxl)
library(dplyr)
library(stringr)
library(DDFConverter)
library(purrr)
library(tidyr)

# Set working directory
setwd("S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main")

# Import CSV-files
data_files <- list.files("S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/data/raw", pattern = "\\.csv$", full.names = TRUE)

for (file in data_files) {
  name <- tools::file_path_sans_ext(basename(file))
  assign(name, read.csv(file), envir = .GlobalEnv)
}
# Import metadata

# Eurostat metadata
dictionary_files <- list.files("S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/metadata/dictionaries", pattern = "\\.csv$", full.names = TRUE)

for (file in dictionary_files) {
  name <- tools::file_path_sans_ext(basename(file))
  assign(name, read.csv(file), envir = .GlobalEnv)
}
# Transforma metadata


# Merge Eurostat metadata with datafiles
add_metadata <- function(data, ...) {
  metadata <- list(...)
  
  purrr::reduce(
    names(metadata),
    function(data, var) {
      data %>%
        left_join(
          metadata[[var]],
          by = setNames("code_name", var)
        ) %>%
        rename(!!paste0(var, "_name") := full_name) #%>%
        #select(-c(code_name))
    },
    .init = data
  )
}

#####################
# "bd_size"
bd_size_meta <- 
  add_metadata(
    bd_size,
    age = bd_size_age,
    freq = bd_size_freq,
    geo = bd_size_geo,
    indic_sbs = bd_size_indic_sbs,
    nace_r2 = bd_size_nace_r2,
    sizeclas = bd_size_sizeclas
  )
  
# Turn years into wide format
bd_size_meta <- 
  bd_size_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_esst_gg"
env_esst_gg_meta <- 
  add_metadata(
    env_esst_gg,
    ceparema = env_esst_gg_ceparema,
    freq = env_esst_gg_freq,
    geo = env_esst_gg_geo,
    na_item = env_esst_gg_na_item,
    sector = env_esst_gg_sector,
    unit = env_esst_gg_unit
  )

env_esst_gg_meta <- 
  env_esst_gg_meta %>% 
  pivot_wider(id_cols = everything(),
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_ac_ainah_r2"
env_ac_ainah_r2_meta <- 
  add_metadata(
    env_ac_ainah_r2,
    airpol = env_ac_ainah_r2_airpol,
    freq = env_ac_ainah_r2_freq,
    geo = env_ac_ainah_r2_geo,
    nace_r2 = env_ac_ainah_r2_nace_r2,
    unit = env_ac_ainah_r2_unit
  )

env_ac_ainah_r2_meta <- 
  env_ac_ainah_r2_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_ac_taxind2"
env_ac_taxind2_meta <- 
  add_metadata(
    env_ac_taxind2,
    tax = env_ac_taxind2_tax,
    freq = env_ac_taxind2_freq,
    geo = env_ac_taxind2_geo,
    nace_r2 = env_ac_taxind2_nace_r2,
    unit = env_ac_taxind2_unit
  )

env_ac_taxind2_meta <- 
  env_ac_taxind2_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")
  
#####################
# "env_ac_pefa04"
env_ac_pefa04_meta <- 
  add_metadata(
    env_ac_pefa04,
    indic_pefa = env_ac_pefa04_indic_pefa,
    freq = env_ac_pefa04_freq,
    geo = env_ac_pefa04_geo,
    nace_r2 = env_ac_pefa04_nace_r2,
    unit = env_ac_pefa04_unit
  )
  
env_ac_pefa04_meta <- 
  env_ac_pefa04_meta %>% 
  pivot_wider(id_cols = everything(),
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_ac_ccminv"
env_ac_ccminv_meta <- 
  add_metadata(
    env_ac_ccminv,
    env_pa = env_ac_ccminv_env_pa,
    freq = env_ac_ccminv_freq,
    geo = env_ac_ccminv_geo,
    nace_r2 = env_ac_ccminv_nace_r2,
    unit = env_ac_ccminv_unit
  )

env_ac_ccminv_meta <- 
  env_ac_ccminv_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_wasgen"
env_wasgen_meta <- 
  add_metadata(
    env_wasgen,
    hazard = env_wasgen_hazard,
    freq = env_wasgen_freq,
    geo = env_wasgen_geo,
    nace_r2 = env_wasgen_nace_r2,
    unit = env_wasgen_unit,
    waste = env_wasgen_waste
  )

env_wasgen_meta <- 
  env_wasgen_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "env_wat_cat"
env_wat_cat_meta <- add_metadata(
  env_wat_cat,
  wat_proc = env_wat_cat_wat_proc,
  freq = env_wat_cat_freq,
  geo = env_wat_cat_geo,
  nace_r2 = env_wat_cat_nace_r2,
  unit = env_wat_cat_unit
)

env_wat_cat_meta <- 
  env_wat_cat_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "rd_e_berdfundr2"
rd_e_berdfundr2_meta <- add_metadata(
  rd_e_berdfundr2,
  sectfund = rd_e_berdfundr2_sectfund,
  freq = rd_e_berdfundr2_freq,
  geo = rd_e_berdfundr2_geo,
  nace_r2 = rd_e_berdfundr2_nace_r2,
  unit = rd_e_berdfundr2_unit
)

rd_e_berdfundr2_meta <- 
  rd_e_berdfundr2_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "nama_10_a64"
nama_10_a64_meta <- add_metadata(
  nama_10_a64,
  na_item = nama_10_a64_na_item,
  freq = nama_10_a64_freq,
  geo = nama_10_a64_geo,
  nace_r2 = nama_10_a64_nace_r2,
  unit = nama_10_a64_unit
)

nama_10_a64_meta <- 
  nama_10_a64_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")

#####################
# "nama_10_a64_e"
nama_10_a64_e_meta <- add_metadata(
  nama_10_a64_e,
  na_item = nama_10_a64_e_na_item,
  freq = nama_10_a64_e_freq,
  geo = nama_10_a64_e_geo,
  nace_r2 = nama_10_a64_e_nace_r2,
  unit = nama_10_a64_e_unit
)

nama_10_a64_e_meta <- 
  nama_10_a64_e_meta %>% 
  pivot_wider(id_cols = everything(), 
              values_from = "values",
              names_from = "TIME_PERIOD")
#####################



# Create DDF-filesystem
# Multidimensional datasets created by a seperate function (create_multidimensional_datapoints)

# Sum into one value per country and year

# "bd_size"
bd_size_total <- bd_size_meta %>% 
  group_by(geo, indic_sbs, indic_sbs_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(indic_sbs),
         var_name = indic_sbs_name) %>% 
  select(-c(geo, indic_sbs, indic_sbs_name))


#####################
# "env_esst_gg"
env_esst_gg_total <- env_esst_gg_meta %>% 
  group_by(geo, ceparema, ceparema_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(ceparema),
         var_name = ceparema_name) %>% 
  select(-c(geo, ceparema, ceparema_name))



#####################
# "env_ac_ainah_r2"
env_ac_ainah_r2_total <- env_ac_ainah_r2_meta %>% 
  group_by(geo, airpol, airpol_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(airpol),
         var_name = airpol_name) %>% 
  select(-c(geo, airpol, airpol_name))

#####################
# "env_ac_taxind2"
env_ac_taxind2_total <- env_ac_taxind2_meta %>% 
  group_by(geo, tax, tax_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(tax),
         var_name = tax_name) %>% 
  select(-c(geo, tax, tax_name))

#####################
# "env_ac_pefa04"
env_ac_pefa04_total <- env_ac_pefa04_meta %>% 
  group_by(geo, indic_pefa, indic_pefa_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(indic_pefa),
         var_name = indic_pefa_name) %>% 
  select(-c(geo, indic_pefa, indic_pefa_name))

#####################
# "env_ac_ccminv"
env_ac_ccminv_total <- env_ac_ccminv_meta %>% 
  group_by(geo, env_pa, env_pa_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(env_pa),
         var_name = env_pa_name) %>% 
  select(-c(geo, env_pa, env_pa_name))

#####################
# "env_wasgen"
env_wasgen_total <- env_wasgen_meta %>% 
  group_by(geo, hazard, hazard_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(hazard),
         var_name = hazard_name) %>% 
  select(-c(geo, hazard, hazard_name))

#####################
# "env_wat_cat"
env_wat_cat_total <- env_wat_cat_meta %>% 
  group_by(geo, wat_proc, wat_proc_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(wat_proc),
         var_name = wat_proc_name) %>% 
  select(-c(geo, wat_proc, wat_proc_name))

#####################
# "rd_e_berdfundr2"
rd_e_berdfundr2_total <- rd_e_berdfundr2_meta %>% 
  group_by(geo, sectfund, sectfund_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
#         var_id = tolower(sectfund),
         var_name = sectfund_name,
         var_id = ifelse(tolower(sectfund) == "total","total_all_sect", tolower(sectfund))) %>% 
  select(-c(geo, sectfund, sectfund_name))

#####################
# "nama_10_a64"
nama_10_a64_total <- nama_10_a64_meta %>% 
  group_by(geo, na_item, na_item_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(na_item),
         var_name = na_item_name) %>% 
  select(-c(geo, na_item, na_item_name))

#####################
# "nama_10_a64_e"
nama_10_a64_e_total <- nama_10_a64_e_meta %>% 
  group_by(geo, na_item, na_item_name, geo_name, freq, freq_name) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(geog = tolower(geo),
         var_id = tolower(na_item),
         var_name = na_item_name) %>% 
  select(-c(geo, na_item, na_item_name))

#####################

# Combine datasets
keys <- c("var_id", "var_name", "geog", "geo_name", "freq", "freq_name")


dfs <- list(
  env_esst_gg_total,
  env_ac_ainah_r2_total,
  env_ac_taxind2_total,
  env_ac_pefa04_total,
  env_ac_ccminv_total,
  env_wasgen_total,
  env_wat_cat_total,
  rd_e_berdfundr2_total,
  nama_10_a64_total,
  nama_10_a64_e_total
)

dataset_total <- map_dfr(dfs, function(df) {
  df %>%
    pivot_longer(
      cols = -all_of(keys),
      names_to = "year",
      values_to = "value"
    )
}) %>%
  group_by(across(all_of(keys)), year) %>%
  summarise(
    value = first(na.omit(value)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = value
  )

dataset_total$var_name <- gsub(",", ":", dataset_total$var_name) # Cannot have commas in the names
dataset_total$freq <- tolower(dataset_total$freq) # ID must be lowercase

############### Create DDf with every variable #################################

create_ddf(dataset = dataset_total,
           variable_id = "var_id",
           variable_name = "var_name",
           entity_id = "geog",
           entity_name = "geo_name",
           datacolumns = "7-58",
           tag_id = "freq",
           tag_name = "freq_name"
)
################################################################################

# Test for if there are duplicates in the var_id
# duplicates <- dataset_total %>%
#   count(geog, var_id) %>%
#   filter(n > 1)
# duplicates
