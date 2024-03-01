# PURPOSE: les-elephants
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Review COP23 / PSNU Changes
# REF ID:  cce8ad86
# LICENSE: MIT
# DATE:    2023-12-13
# UPDATE:  2023-12-13
# NOTES:   CDI has changed the Orgunit Level used as PSNU

# Libraries ====

  library(tidyverse)
  library(glamr)
  library(gophr)
  library(grabr)


# LOCALS & SETUP ====

  # Set Params

  ref_id <- "cce8ad86"
  agency <- "USAID"
  cntry <- "Cote d'Ivoire"

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21-.*_Cote d'Ivoire")
  file_site <- si_path() %>% return_latest("Site_IM_FY21-.*_Cote d'Ivoire")

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

# VIZ =====



# OUTPUTS =====

