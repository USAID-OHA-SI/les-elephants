# PURPOSE: SI Support for Cote d'Ivoire Programs
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Global config and params
# REF ID:  b7096822
# LICENSE: MIT
# DATE:    2023-08-24
# UPDATE:  2023-08-24
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries

    # Library to be loaded from str
  # Set paths

    dir_data   <- "Data"
    dir_dataout <- "Dataout"
    dir_images  <- "Images"
    dir_graphics  <- "Graphics"

    dir_mer <- glamr::si_path("path_msd")
    dir_ras <- glamr::si_path("path_raster")
    dir_shp <- glamr::si_path("path_vector")
    dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Cote d'Ivoire")

  # Set Params

    ref_id <- "b7096822"
    agency <- "USAID"
    cntry <- "Cote d'Ivoire"

    # Indicators Reference list
    inds <- list(
      'prevention' = c(
        "FPINT_SITE", "GEND_GBV", "KP_MAT", "KP_PREV",
        "OVC_SERV", "PP_PREV", "PREP_CT",
        "PREP_NEW", "TB_PREV", "VMMC_CIRC"
      ),
      'testing' = c(
        "CXCA_SCRN", "HTS_INDEX", "HTS_RECENT",
        "HTS_SELF", "HTS_TST", "OVC_HIVSTAT",
        "PMTCT_EID", "PMTCT_FO", "PMTCT_HEI_POS", "PMTCT_STAT", "TB_STAT"
      ),
      'treatement' = c(
        "CXCA_TX", "PMTCT_ART", "TB_ART",
        "TX_CURR", "TX_ML", "TX_NEW",
        "TX_RTT", "TX_TB"
      ),
      'viralload' = c("TX_PVLS"),
      'healthsystem' = c(
        "EMR_SITE", "LAB_PTCQI", "SC_ARVDISP", "SC_CURR"
      )
    )


  # Files

    file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
    file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21")

# Functions

# LOAD DATA ============================================================================

  #

# MUNGE ============================================================================

  #

# VIZ ============================================================================

  #

# OUTPUTS ============================================================================

