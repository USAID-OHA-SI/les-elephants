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
  library(openxlsx)
  library(glue)

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

  fy <- 2023

# Data

  ## Mechanisms

  df_mechs <- Wavelength::pull_mech(ou_sel = cntry)

  ## OPU

  file_opu %>% excel_sheets()

  df_opu_raw <- file_opu %>%
    read_excel(sheet = "PSNUxIM", skip = 13)

  df_opu_raw %>%
    filter(str_detect(indicator_code, "OVC")) %>%
    distinct(indicator_code)

  df_opu <- file_opu %>% tame_dp(type = "PSNUxIM")

  df_opu %>% glimpse()

  df_opu <- df_opu %>%
    mutate(fiscal_year = fy) %>%
    select(-cumulative) %>%
    rename(targets_opu = targets)

  df_opu %>%
    filter(str_detect(indicator, "OVC")) %>%
    distinct(indicator, standardizeddisaggregate)

  opu_cols <- df_opu %>% names()

  df_opu <- df_opu %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, "PREP") ~ str_replace(indicator, "PREP", "PrEP"),
        TRUE ~ indicator
      ),
      standardizeddisaggregate = case_when(
        indicator == "HTS_INDEX" ~ paste0("4:", standardizeddisaggregate),
        indicator == "TB_ART" ~ "Age/Sex/NewExistingArt/HIVStatus",
        indicator == "TB_STAT" & numeratordenom == "D" ~ "Age/Sex",
        indicator == "PrEP_NEW" & standardizeddisaggregate == "KeyPop" ~ "KeyPopAbr",
        TRUE ~ standardizeddisaggregate
      ))

  df_opu_inds <- df_opu %>%
    distinct(indicator,
             indicatortype,
             numeratordenom,
             standardizeddisaggregate) %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, "PREP") ~ str_replace(indicator, "PREP", "PrEP"),
        TRUE ~ indicator
      ),
      standardizeddisaggregate = case_when(
        indicator == "HTS_INDEX" ~ paste0("4:", standardizeddisaggregate),
        indicator == "TB_ART" ~ "Age/Sex/NewExistingArt/HIVStatus",
        indicator == "TB_STAT" & numeratordenom == "D" ~ "Age/Sex",
        indicator == "PrEP_NEW" & standardizeddisaggregate == "KeyPop" ~ "KeyPopAbr",
        TRUE ~ standardizeddisaggregate
      )) %>%
    arrange(indicator, standardizeddisaggregate)

  ## MSD

  df_msd <- file_msd_psnu %>% read_psd()

  df_msd %>% glimpse()

  df_msd_inds <- df_msd %>%
    distinct(indicator, indicatortype,
             numeratordenom, standardizeddisaggregate, source_name) %>%
    arrange(indicator, standardizeddisaggregate)

  # Filter original targets
  df_targets <- df_opu_inds %>%
    pmap_dfr(~ filter(.data = df_msd,
                      fiscal_year == fy,
                      indicator == ..1,
                      indicatortype == ..2,
                      numeratordenom == ..3,
                      standardizeddisaggregate == ..4))



  df_targets %>% glimpse()

  df_targets %>%
    distinct(indicator, indicatortype,
              numeratordenom, standardizeddisaggregate) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    mutate(source = "MSD") %>%
    left_join(df_opu_inds, by = c(
      "indicator",
      "indicatortype",
      "numeratordenom",
      "standardizeddisaggregate"
    )) %>%
    filter(is.na(source))

  # Narrow down the dataset
  df_targets <- df_targets %>%
    filter(!is.na(targets)) %>%
    select(any_of(opu_cols), targets)

  # Get all mech / funding info
  df_msd_mechs <- df_targets %>%
    select(mech_code, mech_name, prime_partner_name, prime_partner_name, funding_agency) %>%
    distinct_all()

  ## Check OVC Targets
  df_msd %>%
    filter(fiscal_year == fy,
           indicator == "OVC_SERV") %>%
    distinct(standardizeddisaggregate) %>%
    arrange(standardizeddisaggregate)

  df_msd %>%
    filter(fiscal_year == fy,
           indicator == "OVC_SERV",
           str_detect(standardizeddisaggregate, "ProgramStatus")) %>%
    summarise(targets = sum(targets, na.rm = T),
              .by = c(indicator, standardizeddisaggregate))

# MUNGING

  ## OPUs

  df_opu <- df_opu %>%
    select(-c(mech_name, prime_partner_name, prime_partner_name, funding_agency)) %>%
    left_join(df_msd_mechs, by = "mech_code")

  ## Summarise OPU Numbers

  df_opu_ou <- df_opu %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(indicator, standardizeddisaggregate))

  df_opu_agency <- df_opu %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency,
                      indicator, standardizeddisaggregate))

  df_opu_psnu <- df_opu %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency, psnu,
                      indicator, standardizeddisaggregate))

  df_opu_mechs <- df_opu %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency, mech_code, mech_name,
                      indicator, standardizeddisaggregate))

  ## ## Summarise MSD Numbers

  df_msd_ou <- df_targets %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(indicator, standardizeddisaggregate)) %>%
    full_join(df_opu_ou) %>%
    mutate(diff = targets_opu - targets)

  df_msd_agency <- df_targets %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency,
                      indicator, standardizeddisaggregate)) %>%
    full_join(df_opu_agency) %>%
    mutate(diff = targets_opu - targets) %>%
    arrange(funding_agency, indicator)

  df_msd_psnu <- df_targets %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency, psnu,
                      indicator, standardizeddisaggregate)) %>%
    full_join(df_opu_psnu) %>%
    mutate(diff = targets_opu - targets) %>%
    arrange(funding_agency, psnu, indicator)

  df_msd_mechs <- df_targets %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(funding_agency, mech_code, mech_name,
                      indicator, standardizeddisaggregate)) %>%
    full_join(df_opu_mechs) %>%
    mutate(diff = targets_opu - targets) %>%
    arrange(funding_agency, mech_code, mech_name, indicator)

# output

  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "OU")
  writeDataTable(wb, sheet = "OU", x = df_msd_ou)

  addWorksheet(wb, sheetName = "AGENCY")
  writeDataTable(wb, sheet = "AGENCY", x = df_msd_agency)

  addWorksheet(wb, sheetName = "PSNU")
  writeDataTable(wb, sheet = "PSNU", x = df_msd_psnu)

  addWorksheet(wb, sheetName = "IM")
  writeDataTable(wb, sheet = "IM", x = df_msd_mechs)

  saveWorkbook(wb,
               file = file.path(dir_out, "COP22 - OPU Targets Checks.xlsx"),
               overwrite = TRUE)



