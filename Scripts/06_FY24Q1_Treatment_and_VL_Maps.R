# PURPOSE: LES-ELEPHANTS
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: TX and VLC/S Distribution
# REF ID:  eebfed1a
# LICENSE: MIT
# DATE:    2024-02-29
# UPDATE:  2024-02-29
# NOTES:   Sample Scripts for CDI

# Libraries ====

  library(tidyverse)
  library(glamr)
  library(gophr)
  library(grabr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(lubridate)
  library(glue)


# LOCALS & SETUP ====

  # Set Params

  ref_id <- "b8140e7e"
  agency <- "USAID"
  cntry <- "Cote d'Ivoire"
  cntry_uid <- get_ouuid(cntry)

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path(glue("../../PEPFAR/COUNTRIES/{cntry}"))


  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu1 <- si_path() %>% return_latest(glue("PSNU_IM_FY22"))
  file_psnu2 <- si_path() %>% return_latest(glue("PSNU_IM_FY22-.*_{cntry}"))
  file_site1 <- si_path() %>% return_latest(glue("Site_IM_FY15-.*_{cntry}"))
  file_site2 <- si_path() %>% return_latest(glue("Site_IM_FY22-.*_{cntry}"))

  get_metadata(file_nat)

  meta <- metadata

# Functions  =====

  #' @title Export Map plot
  #'
  export_map <- function(mplot, name, ...) {
    si_save(plot = mplot,
            filename = file.path(dir_graphics, glue::glue("{name}.png")),
            scale = 1.4,
            dpi = 350,
            width = 8,
            height = 6,
            ...)
  }


  #' @title Convert sf Bounding Box to data frame
  #'
  #' @param spdf   Spatial Data Frame, preferably and sf object
  #' @param expand Expansion rate as a numerical value and in the spdf unit. Number of rows/columns to extend by.
  #'
  #' @return A data frame containing the bounds of latitude and longitude
  #'
  bbox_as_df <- function(spdf, expand = 0) {
    # Extract bounding box
    spext <- sf::st_bbox(spdf)

    # Extend bbox if asked
    if (expand > 0) {
      spext <- terra::extend(terra::ext(spext), {{expand}})
    }

    # Convert bbox to data frame
    spext %>%
      base::as.list() %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "bound",
                          values_to = "value") %>%
      dplyr::mutate(axis = str_sub(bound, 1, 1),
                    bound = str_remove(bound, "x|y")) %>%
      tidyr::pivot_wider(names_from = axis, values_from = value) %>%
      dplyr::rename(longitude = x, latitude = y)
  }

# LOAD DATA =====

  ## MER
  df_nat <- file_nat %>% read_psd()
  df_msd <- file_psnu2 %>% read_psd()

  ## Geo data

  ## Terrain data
  dt_terr <- get_raster(folderpath = si_path("path_raster"))

  ## VC POlygons

  spdf_pepfar <- get_vcpolygons(folderpath = si_path("path_vector"))

  ## Country boundaries
  spdf_cnty <- spdf_pepfar %>% filter(uid == cntry_uid)

  spdf_cnty %>% bbox_as_df()
  spdf_cnty %>% bbox_as_df(.5)
  spdf_cnty %>% bbox_as_df(1)

  ext_cntry <- spdf_cnty %>% bbox_as_df()

  ## Hexbins
  spdf_hex1 <- spdf_cnty %>%
    st_make_valid() %>%
    get_hexbins(size = 30000, clip = F) %>%
    filter(st_geometry_type(x) != "POINT") %>%
    select(id, area) %>%
    mutate(puid = cntry_uid)

  spdf_hex2 <- spdf_cnty %>%
    st_make_valid() %>%
    get_hexbins(size = 30000, clip = T) %>%
    filter(st_geometry_type(x) != "POINT") %>%
    select(id, area) %>%
    mutate(puid = cntry_uid)

  spdf_hex1 %>% gview()
  spdf_hex2 %>% gview()

  spdf_hex_big <- spdf_cnty %>%
    st_make_valid() %>%
    get_hexbins(size = 100000) %>%
    filter(st_geometry_type(x) != "POINT") %>%
    select(id, area) %>%
    mutate(puid = cntry_uid)

  spdf_hex_big %>% gview()

  ## States boundaries
  spdf_psnu <- cntry_uid %>%
    get_ouorgs(level = get_ouorglevel(cntry, org_type = "prioritization")) %>%
    left_join(spdf_pepfar, ., by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_psnu %>% glimpse()


# MUNGE =====

  ## NAT & SubNat - Extract

  df_nat %>% glimpse()

  df_pop <- df_nat %>%
    filter(operatingunit == cntry,
           fiscal_year == meta$curr_fy,
           indicator %in% c("POP_EST", "PLHIV"))

  df_pop <- df_nat %>%
    filter(operatingunit == cntry,
           fiscal_year %in% c(meta$curr_fy -1, meta$curr_fy),
           indicator %in% c("POP_EST", "PLHIV"),
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(value = sum(targets, na.rm = T),
              .by = c(fiscal_year, snu1uid, snu1, psnuuid, psnu, indicator)) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    rename_with(str_to_lower) %>%
    group_by(snu1uid, snu1, psnuuid, psnu) %>%
    mutate(
      pop_est = case_when(
        fiscal_year == meta$curr_fy &
          is.na(pop_est) ~ pop_est[fiscal_year == meta$curr_fy -1],
        TRUE ~ pop_est
      )) %>%
    ungroup() %>%
    filter(fiscal_year == meta$curr_fy) %>%
    mutate(prev = plhiv / pop_est)

  ## TX & VLC/S

  df_tx <- df_msd %>%
    filter(fiscal_year %in% c(meta$curr_fy -1, meta$curr_fy),
           indicator %in% c("TX_CURR", "TX_PVLS"),
           str_detect(standardizeddisaggregate, "Total"))

  df_tx %>% glimpse()

  df_tx %>% distinct(indicator, standardizeddisaggregate)

  df_tx <- df_tx %>%
    clean_indicator() %>%
    reshape_msd() %>%
    summarise(value =  sum(value, na.rm = T),
              .by = c(period, period_type, snu1uid, snu1,
                      psnuuid, psnu, indicator)) %>%
    filter(period_type == "results") %>%
    select(-period_type) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    rename_with(str_to_lower) %>%
    group_by(snu1uid, snu1, psnuuid, psnu) %>%
    mutate(vlc = tx_pvls / lag(tx_curr, 2, order_by = period),
           vls = tx_pvls / tx_pvls_d) %>%
    ungroup() %>%
    filter(period == meta$curr_pd)

  ## Merge both Pop & TX

  df_vl <- df_pop %>%
    left_join(df_tx, by = c("snu1uid", "snu1", "psnuuid", "psnu")) %>%
    relocate(period, .after = 1) %>%
    mutate(period = case_when(is.na(period) ~ first(period), TRUE ~ period))

  ## Spatial

  spdf_psnu_vl <- spdf_psnu %>%
    left_join(df_vl, by = c("uid" = "psnuuid", "orgunit" = "psnu")) %>%
    filter(!is.na(snu1))


# VIZ =====

  ## Basemap

  bmap <- terrain_map(countries = spdf_cnty,
                      adm0 = spdf_cnty,
                      adm1 = spdf_psnu,
                      terr = dt_terr,
                      mask = T)

  # Treatment
  map_tx <- bmap +
    geom_sf(data = spdf_psnu_vl,
            aes(fill = tx_curr), color = grey30k,
            linewidth = .8, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = filter(spdf_psnu, str_detect(orgunit, "Abidjan", negate = T)),
                 aes(label = str_replace(orgunit, "-| ", "\n"))) +
    scale_fill_si(palette = "burnt_siennas", na.value = grey10k,
                  breaks = seq(0, max(df_vl$tx_curr, na.rm = T), 10000),
                  limits = c(0, max(df_vl$tx_curr, na.rm = T)),
                  labels = comma) +
    labs(
      x = "", y = "",
      title = str_to_upper(glue("{cntry} - TREATMENT DISTRIBUTION BY SUB-NATIONAL UNIT")),
      subtitle = glue(
      "As of **{meta$curr_pd}**, <span style='color:{burnt_sienna}'>**{comma(sum(df_vl$tx_curr, na.rm = T))}**</span> are on Antiretroviral Therapy (ART)"
    )) +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(.3, "cm"))

  export_map(map_tx, glue("CDI - {meta$curr_pd} TX_CURR Distribution"))


  # VLC

  range(df_vl$vlc, na.rm = T)

  vlc_max <- max(df_vl$vlc, na.rm = T)
  vlc_max <- ifelse(vlc_max < 1, 1, ceiling(vlc_max))

  map_vlc <- bmap +
    geom_sf(data = spdf_psnu_vl,
            aes(fill = vlc), color = grey30k,
            linewidth = .8, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = filter(spdf_psnu, str_detect(orgunit, "Abidjan", negate = T)),
                 aes(label = str_replace(orgunit, "-| ", "\n"))) +
    scale_fill_si(palette = "scooters", na.value = grey10k,
                  breaks = seq(.75, vlc_max, .05),
                  limits = c(.75, vlc_max),
                  labels = percent) +
    labs(
      x = "", y = "",
      title = str_to_upper(glue("{cntry} - TREATMENT DISTRIBUTION BY SUB-NATIONAL UNIT")),
      subtitle = glue(
        "As of **{meta$curr_pd}**, <span style='color:{burnt_sienna}'>**{comma(sum(df_vl$tx_curr, na.rm = T))}**</span> are on Antiretroviral Therapy (ART)"
      )) +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(.3, "cm"))

  export_map(map_vlc, glue("CDI - {meta$curr_pd} VL Coverage Distribution"))


  # VLS

  range(df_vl$vls, na.rm = T)

  vls_max <- max(df_vl$vlc, na.rm = T)
  vls_max <- ifelse(vls_max < 1, 1, ceiling(vls_max))

  map_vls <- bmap +
    geom_sf(data = spdf_psnu_vl,
            aes(fill = vls), color = grey30k,
            linewidth = .8, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = filter(spdf_psnu, str_detect(orgunit, "Abidjan", negate = T)),
                 aes(label = str_replace(orgunit, "-| ", "\n")), color = grey10k) +
    scale_fill_si(palette = "genoas", na.value = grey10k,
                  breaks = seq(.75, vls_max, .05),
                  limits = c(.75, vlc_max),
                  labels = percent) +
    labs(
      x = "", y = "",
      title = str_to_upper(glue("{cntry} - TREATMENT DISTRIBUTION BY SUB-NATIONAL UNIT")),
      subtitle = glue(
        "As of **{meta$curr_pd}**, <span style='color:{burnt_sienna}'>**{comma(sum(df_vl$tx_curr, na.rm = T))}**</span> are on Antiretroviral Therapy (ART)"
      )) +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(.3, "cm"))

  export_map(map_vls, glue("CDI - {meta$curr_pd} VL Suppression Distribution"))

# OUTPUTS =====

