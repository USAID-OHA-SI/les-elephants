# PURPOSE: Munge and Analysis of
# AUTHOR: Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: FY23Q3i Data Review
# REF ID: 8387bcfe
# LICENSE: MIT
# DATE: 2023-08-22
# UPDATE: 2023-08-24
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(cascade)
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

    ref_id <- "8387bcfe"
    agency <- "USAID"
    cntry <- "Cote d'Ivoire"

  # Files

    file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
    file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21.*Cote")

    file_psnu %>% get_metadata()

    meta <- metadata

  # Functions

# LOAD DATA ============================================================================

  df_msd_psnu <- file_psnu %>% read_psd()


# MUNGE ============================================================================

  # Cascade Data
  df_cascade <- df_msd_psnu %>%
      return_cascade(cscd_num = 1)

  df_cascade %>% glimpse()

# VIZ ============================================================================

  ## Check cascade plots
  plot_name

  return_cascade_plot(msd_df = df_msd_psnu,
                      export = T,
                      path = dir_images)

# OUTPUTS ============================================================================

