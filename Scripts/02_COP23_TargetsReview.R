# PROJECT:  les-elephants
# AUTHOR:   B. Kagniniwa | USAID
# PURPOSE:  Review CDI COP23 Targets
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
  dir_cntry_data <- file.path(dir_mer, "../PEPFAR/COUNTRIES/Cote d'Ivoire/Data")
  dir_in <- "./Data"
  dir_out <- "./Dataout"

  ## Files
  file_tst_cop23 <- dir_cntry_data %>%
    return_latest("Target Setting Tool_Cote d'Ivoire_.*.xlsx")

  file_tst_cop23_psnuim <- dir_cntry_data %>%
    return_latest("PSNUxIM_CotedIvoire_.*.xlsx")

  file_msd_psnu <- dir_mer %>%
    return_latest(".*_PSNU_IM_FY21-23_.*_Cote d'Ivoire.zip")

  ## Params
  cntry <- "Cote d'Ivoire"

  fy <- 2024

# Data

  ## Mechanisms

  df_mechs <- Wavelength::pull_mech(usaid_only = F, ou_sel = cntry)

  ## TaST

  file_tst_cop23 %>% excel_sheets()

  file_tst_cop23_psnuim %>% excel_sheets()

  df_tst <- file_tst_cop23_psnuim %>% tame_dp(type = "PSNUxIM")

  df_tst <- df_tst %>%
    mutate(fiscal_year = fy) %>%
    select(-cumulative)

  df_tst %>%
    distinct(indicator,
             indicatortype,
             numeratordenom,
             standardizeddisaggregate)

  df_tst %>%
    distinct(snuprioritization) %>%
    arrange(snuprioritization)

  df_tst %>%
    distinct(ageasentered) %>%
    arrange(ageasentered)

  df_tst <- df_tst %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% c("<=02 Months", "02 - 12 Months", "<01", "01-04", "05-09", "01-09", "10-14") ~ "<15",
        ageasentered %in% c("15+", "15-24", "25-34", "35-49", "50+") ~ "15+",
        ageasentered %in% c("15-17") ~ "<18",
        TRUE ~ ageasentered
      )
    ) %>%
    relocate(agecoarse, .after = ageasentered)


# MUNGING

  df_tst <- df_tst %>%
    select(-c(mech_name, prime_partner_name, prime_partner_name, funding_agency)) %>%
    left_join(df_mechs, by = "mech_code")

  ## Summarise OPU Numbers

  df_tst_ou <- df_tst %>%
    clean_indicator() %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(indicator, standardizeddisaggregate)) %>%
    mutate(agecoarse = 'total') %>%
    relocate(agecoarse, .before = targets) %>%
    arrange(indicator, standardizeddisaggregate)

  df_tst_ou_age <- df_tst %>%
    clean_indicator() %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(indicator, standardizeddisaggregate, agecoarse)) %>%
    bind_rows(df_tst_ou) %>%
    arrange(indicator, standardizeddisaggregate, agecoarse)

  df_tst_ou_age_confirm <- df_tst_ou_age %>%
    filter(!is.na(agecoarse)) %>%
    mutate(
      population = case_when(
        str_detect(standardizeddisaggregate, "KeyPop") ~ "KP",
        TRUE ~ "GP"
      ),
      agecoarse = case_when(
        str_detect(agecoarse, "<") ~ str_replace(agecoarse, "<", "u"),
        str_detect(agecoarse, "[+]") ~ str_replace(agecoarse, "[+]", "plus"),
        TRUE ~ agecoarse
      )
    ) %>%
    pivot_wider(names_from = agecoarse,
                values_from = targets,
                names_sort = T) %>%
    relocate(u15, `15plus`, u18, `18plus`, total, .after = population)


  df_tst_agency <- df_tst %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(fundingagency,
                      indicator, standardizeddisaggregate)) %>%
    arrange(fundingagency, indicator, standardizeddisaggregate)

  df_tst_psnu <- df_tst %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(fundingagency, psnu,
                      indicator, standardizeddisaggregate)) %>%
    arrange(fundingagency, psnu, indicator, standardizeddisaggregate)

  df_tst_mechs <- df_tst %>%
    summarise(across(starts_with("targets"), \(.x) sum(.x, na.rm = T)),
              .by = c(fundingagency, mech_code, mech_name,
                      indicator, standardizeddisaggregate)) %>%
    arrange(fundingagency, mech_code, mech_name, indicator, standardizeddisaggregate)


# output

  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "Summary")
  writeDataTable(wb, sheet = "Summary", x = df_tst_ou_age_confirm)

  addWorksheet(wb, sheetName = "OU")
  writeDataTable(wb, sheet = "OU", x = df_tst_ou)

  addWorksheet(wb, sheetName = "AGENCY")
  writeDataTable(wb, sheet = "AGENCY", x = df_tst_agency)

  addWorksheet(wb, sheetName = "PSNU")
  writeDataTable(wb, sheet = "PSNU", x = df_tst_psnu)

  addWorksheet(wb, sheetName = "IM")
  writeDataTable(wb, sheet = "IM", x = df_tst_mechs)

  file_output <- file.path(dir_out, "COP23 - Targets Checks.xlsx")

  saveWorkbook(wb,
               file = file_output,
               overwrite = TRUE)

  open_path(file_output)


