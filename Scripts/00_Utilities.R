# PURPOSE:
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE:
# REF ID:  b03ca497
# LICENSE: MIT
# DATE:    2023-08-24
# UPDATE:  2023-08-24
# NOTES:

# LOCALS & SETUP ====
  # Libraries

  # Dependencies

  source("./Scripts/00_Config.R")

# Functions ====

  #' @title Download CDI MSDs
  #'
  cdi_msds <- function(dest = glamr::si_path(type = "path_msd"),
                       country = "Cote d'Ivoire",
                       username = glamr::pano_user(),
                       password = glamr::pano_pwd()) {


    # Extract all global & Country Specific MSD
    grabr::pano_extract_msds(operatingunit = country,
                             archive = FALSE,
                             dest_path = dest,
                             username = username,
                             password = password)
  }

# LOAD DATA ====



# MUNGE =====

  #

# VIZ =====

  #

# OUTPUTS =====

