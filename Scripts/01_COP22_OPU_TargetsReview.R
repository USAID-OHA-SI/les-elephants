# PROJECT:  les-elephants
# AUTHOR:   B. Kagniniwa | USAID
# PURPOSE:  Review CDI COP22 OPU
# LICENSE:  MIT
# DATE:     2023-05-16
# UPDATED:  2023-05-16


# LIBS ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glamr)
  library(tameDP)

# Params ----

  ## Directories
  dir_mer <- si_path("path_msd")
  dir_in <- "./Data"
  dir_out <- "./Dataout"

  ## Files
  file_opu <- dir_in %>%
    return_latest("OPU Data Pack_Cote.*.xlsx")

  file_msd_psnu <- dir_mer %>%
    return_latest(".*_PSNU_IM_FY21-23_.*_Cote d'Ivoire.zip")

  ## Params
  cntry <- "Cote d'Ivoire"

  fy <- "2022"

# Data

  ## OPU
  file_opu %>% excel_sheets()

  df_opu <- file_opu %>%
    tame_dp(type = "PSNUxIM")

  df_opu %>% glimpse()

  df_opu_inds <- df_opu %>%
    distinct(indicator, indicatortype,
             numeratordenom, standardizeddisaggregate)

  ## MSD
  df_msd <- file_msd_psnu %>% read_psd()

    pmap_dfr(function(indicatortype,
                      indicator,
                      standardizeddisaggregate,
                      numeratordenom){
      df_msd %>%
        filter(fiscal_year == fy,
               indicatortype == indicatortype,
               indicator == indicator,
               standardizeddisaggregate == standardizeddisaggregate,
               numeratordenom == numeratordenom,
               source_name == "DATIM")
    })




