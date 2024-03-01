# PURPOSE: CDI Support - Data Review
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Summary Table
# REF ID:  ca0cb0c6
# LICENSE: MIT
# DATE:    2023-08-24
# UPDATE:  2023-08-24
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)


  # Set paths
    dir_data   <- "Data"
    dir_dataout <- "Dataout"
    dir_images  <- "Images"
    dir_graphics  <- "Graphics"

    dir_mer <- glamr::si_path("path_msd")
    dir_ras <- glamr::si_path("path_raster")
    dir_shp <- glamr::si_path("path_vector")

  # Set Params

    ref_id <- "ca0cb0c6"
    agency <- "USAID"
    cntry <- "Cote d'Ivoire"

  # Files

    file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
    file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21.*_Cote")
    file_site <- si_path() %>% return_latest("Site_IM_FY21.*_Cote")

    file_psnu %>% get_metadata()

    meta <- metadata


# Functions


# LOAD DATA ============================================================================

  # PSNU x IM
  df_msd_psnu <- file_psnu %>% read_psd()

  df_msd_sites <- file_site %>% read_psd()

# MUNGE ============================================================================

  # # of PEPFAR SUPPORTED SITES

  df_msd_site %>% distinct(sitetype)
  df_msd_site %>% distinct(indicatortype)

  df_sites <- df_msd_site %>%
    filter(funding_agency != "Default",
           indicatortype != "Not Applicable",
           sitetype == "Facility",
           !is.na(cumulative)) %>%
    distinct(fiscal_year, operatingunit, facilityuid) %>%
    count(fiscal_year, operatingunit)

  # Indicator Summary at OU level

  df_msd_psnu %>% glimpse()

  df_msd_psnu %>% distinct(funding_agency)

  df_ou <- df_msd_psnu %>%
    clean_agency() %>%
    filter(fiscal_year %in% (meta$curr_fy - 1):meta$curr_fy,
           standardizeddisaggregate == "Total Numerator") %>%
    distinct(indicator) %>% prinf()



  #

# VIZ ============================================================================

  #

# OUTPUTS ============================================================================

