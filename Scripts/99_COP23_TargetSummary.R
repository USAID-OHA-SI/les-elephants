# PURPOSE: LES-ELEPHANTS
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: COP23 YR2 Targets Summary
# REF ID:  add6538b
# LICENSE: MIT
# DATE:    2024-02-27
# UPDATE:  2024-02-27
# NOTES:   SCA request before validation

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(grabr)
  library(tameDP)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(tidytext)


# LOCALS & SETUP ====

  # Set Params

  ref_id <- "b8140e7e"
  agency <- "USAID"
  cntry <- "Nigeria"
  cntry_uid <- get_ouuid(cntry)

  # Set paths

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Cote d'Ivoire")

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21-.*_Cote d'Ivoire")
  file_site <- si_path() %>% return_latest("Site_IM_FY21-.*_Cote d'Ivoire")

  dir_cntry

  file_cop_psnu <- file.path(dir_cntry, "COPs/COP23YR2/Tools") %>%
    return_latest("PSNUxIM_Cote.*202402.*.xlsx")

  file_cop <- file.path(dir_cntry, "COPs/COP23YR2/Tools") %>%
    return_latest("Target Setting Tool_Cote.*202402.*.xlsx")

  get_metadata(file_psnu)

  meta <- metadata



# Functions  =====

# LOAD DATA =====

  # Levels
  cntry_levels <- get_levels(reshape = T) %>%
    filter(countryname == cntry) %>%
    arrange(desc(level), label) %>%
    select(label, level)

  # PSNU x IM
  df_msd_psnu <- file_psnu %>% read_psd()

  # MUNGE =====

  df_msd_psnu %>% glimpse()

  df_orgs <- df_msd_psnu %>%
    select(fiscal_year,
           operatingunituid, operatingunit,
           country, snu1uid, snu1,
           psnuuid, psnu, snuprioritization,
           cop22_psnuuid, cop22_psnu, cop22_snuprioritization)

  df_orgs

  # Target Testing Tool

  file_cop_psnu %>% excel_sheets()

  file_cop %>% excel_sheets()

  df_tst <- file_cop %>% tame_dp()

  df_tst %>% glimpse()

  df_tst %>%
    distinct(indicator, standardizeddisaggregate,
             numeratordenom, sex,
             target_age_2024)

  df_tst %>%
    filter(str_detect(standardizeddisaggregate, "Total")) %>%
    distinct(indicator, standardizeddisaggregate, target_age_2024)

  df_tst %>%
    filter(str_detect(indicator, "OVC"),
           str_detect(standardizeddisaggregate, "Total"))

  df_tst %>%
    select(-c(source_name, source_processed)) %>%
    filter(str_detect(standardizeddisaggregate, "Total", negate = T)) %>%
    mutate(across(contains("age"), ~ifelse(!is.na(.x), paste("'", .x), .x))) %>%
    relocate(fiscal_year, .before = 1) %>%
    write_csv(
      na = "",
      file = file.path(
        dir_cntry,
        "COPs/COP23YR2/Tools/PSNU_IM_Targets_20240227.csv"))


# VIZ =====



# OUTPUTS =====

