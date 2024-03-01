# PURPOSE: les-elephants
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Program Coverage
# REF ID:  40a821e3
# LICENSE: MIT
# DATE:    2023-10-09
# UPDATE:  2023-10-09
# NOTES:   For Incoming MD Presentation

# Libraries ====

  library(tidyverse)
  library(gophr)
  library(grabr)
  library(glitr)
  library(glamr)
  library(sf)
  library(gisr)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(cowplot)

# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Cote d'Ivoire")

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21.*_Cote")
  #file_site <- si_path() %>% return_latest("Site_IM_FY21.*_Cote")

  # Shapefile path
  file_shp <- dir_shp %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

  get_metadata(file_psnu)

  meta <- metadata

  # Set Params

  ref_id <- "40a821e3"
  agency <- "USAID"
  cntry <- "Cote d'Ivoire"

# Functions  =====

# LOAD DATA =====

  df_msd_psnu <- file_psnu %>% read_psd()

  # SPATIAL DATA

  terr <- gisr::get_raster(path = dir_ras)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Org Levels & Attributes

  ouuid <- get_ouuid(cntry)

  df_levels <- get_levels(
      username = datim_user(),
      password = datim_pwd()
    ) %>%
    pivot_longer(
      cols = where(is.numeric),
      names_to = "orgunit_label",
      values_to = "orgunit_level"
    ) %>%
    mutate(
      orgunit_level = case_when(
        orgunit_label == "prioritization" & countryname == cntry ~ orgunit_level + 1,
        TRUE ~ orgunit_level
      )
    )

  # Org units attributes

  df_attrs <- grabr::datim_orgunits(
      cntry = cntry,
      username = datim_user(),
      password = datim_pwd()
      #,base_url = "https://datim.org"
    )

  df_attrs <- df_levels %>%
    filter(countryname == cntry) %>%
    select(countryname, orgunit_label, orgunit_level) %>%
    mutate(orgunit_level = as.character(orgunit_level)) %>%
    left_join(df_attrs, .,
              by = c("regionorcountry_name" = "countryname", "orgunit_level"),
              relationship = "many-to-many") %>%
    mutate(
      orgunit_label = case_when(
        is.na(orgunit_label) ~ "snu1",
        TRUE ~ orgunit_label
      )
    ) %>%
    rename_with(.cols = contains("_internal_id"),
                .fn = ~str_replace(.x, "internal_id", "uid")) %>%
    relocate(orgunit_label, .after = orgunit_level) %>%
    select(-contains(c("_parent", "_code", "moh"))) %>%
    filter(str_detect(orgunit_name, "_Mil", negate = T))

# MUNGE =====

  # Geography

  spdf_pepfar %>% glimpse()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = join_by("uid" == "orgunit_uid")) %>%
    filter(!is.na(orgunit_label))

  # Admin Boundaries

  spdf_cntry <- spdf_pepfar %>%
    filter(orgunit_label == "country")

  spdf_snu1 <- spdf_pepfar %>%
    filter(orgunit_label == "snu1")

  spdf_psnu <- spdf_pepfar %>%
    filter(orgunit_label == "prioritization")

  spdf_ccity <- spdf_pepfar %>%
    filter(orgunit_label == "snu1",
           str_detect(orgunit_name, "Abidjan"))

  spdf_ccity %>% gview

  # OVC ----

  df_ovc <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency == agency,
           str_detect(indicator, "OVC_")) %>%
    clean_agency() %>%
    clean_indicator()

  df_ovc_serv <- df_ovc %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, country, psnuuid, psnu, indicator))

  df_ovc_cov <- df_ovc %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, country, snu1uid, snu1, psnuuid, psnu, mech_name, prime_partner_name))

  df_ovc_cov %>% distinct(mech_name, prime_partner_name)

  spdf_ovc_cov <- spdf_psnu %>%
    left_join(df_ovc_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_name))

  # KP ----

  df_kp <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency == agency &
           str_detect(mech_name, "KP CARE")) %>%
    mutate(mech_name = case_when(
      psnu %in% trans_states ~ "USAID Transition",
      psnu %in% gf_states ~ "GF Transition",
      TRUE ~ mech_name
    ))

  df_kp %>% distinct(mech_name)

  df_kp_cov <- df_kp %>%
    distinct(fiscal_year, funding_agency, country, psnuuid, psnu, mech_name) %>%
    clean_mechs() %>%
    add_row(psnu = "Anambra", mech_name = "GF Transition") %>%
    add_row(psnu = "Ebonyi", mech_name = "GF Transition")

  spdf_kp_cov <- spdf_psnu %>%
    left_join(df_kp_cov, by = c("orgunit_name" = "psnu")) %>%
    filter(!is.na(mech_name))

  # TX ----

  df_tx <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency == agency,
           str_detect(indicator, "TX_")) %>%
    mutate(mech_name = case_when(
      str_detect(mech_name, "RISE") ~ "RISE",
      TRUE ~ mech_name
    ))

  df_tx %>% distinct(mech_name, prime_partner_name)

  df_tx_cov <- df_tx %>%
    distinct(fiscal_year, funding_agency, country, psnuuid, psnu, mech_name) %>%
    filter(str_detect(psnu, "_Mil", negate = T)) %>%
    filter(!(mech_name == "RISE" & psnu != "Taraba"))

  spdf_tx_cov <- spdf_psnu %>%
    left_join(df_tx_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_name))

# VIZ =====

  # Base map

  bmap <- terrain_map(
    countries = spdf_cntry,
    adm0 = spdf_cntry,
    adm1 = spdf_snu1,
    mask = TRUE
  )

  bmap %>% print()

  # OVC Partners Coverage ----

  ovc_map <- bmap +
    geom_sf(data = spdf_ovc_cov,
            aes(fill = mech_name),
            size = .3,
            color = grey10k) +
    geom_sf(data = spdf_snu1,
            color = grey30k,
            linetype = "dotted",
            fill = NA,
            size = 1) +
    geom_sf(data = spdf_cntry,
            color = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            color = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_snu1,
                 aes(label = orgunit_name),
                 size = 3,
                 color = grey90k) +
    scale_fill_manual(
      values = c(
        "RISK-ZERO in Côte d’Ivoire" = scooter,
        "REVE Reducing Vulerability in Children" = moody_blue,
        "Impact4Life" = burnt_sienna
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  ovc_map

  #ovc_map + si_style()

  bbox <- spdf_ovc_cov %>%
    filter(str_detect(snu1, "Abidjan")) %>%
    st_bbox()

  ovc_map_inset <- ovc_map +
    xlim(bbox$xmin, bbox$xmax) +
    ylim(bbox$ymin, bbox$ymax) +
    theme(legend.position = "none",
          plot.background = element_rect(linewidth = 1, color = usaid_red))

  ovc_map_inset

  ovc_map <- ovc_map +
    geom_sf(data = st_as_sfc(bbox), fill = NA, color = usaid_red) +
    annotate(geom = "segment",
             x = -3.6, xend = -2.6, y = 5.30, yend = 5.30,
             arrow = arrow(type = "closed", length = unit(0.02, "npc")),
             color = usaid_red, linewidth = 1.5)

  ovc_map <- ggdraw() +
    draw_plot(ovc_map) +
    draw_plot(ovc_map_inset,
              x = .63, y = .05,
              width = .3, height = .2)


  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - OVC PROGRAM COVERAGE",
             ".png")),
    plot = ovc_map,
    width = 10,
    height = 5,
    dpi = 320,
    scale = 1.2)


  # KP Partners (including FY24 transition) Coverage ----

  kp_map <- bmap +
    geom_sf(data = spdf_kp_cov,
            aes(fill = mech_name),
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_psnu,
                 aes(label = orgunit_name),
                 size = 2,
                 color = grey90k) +
    scale_fill_manual(
      values = c(
        "KP CARE 1" = scooter,
        "KP CARE 2" = moody_blue,
        "USAID Transition" = burnt_sienna,
        "GF Transition" = golden_sand
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  kp_map

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - KP PROGRAM COVERAGE",
             ".png")),
    plot = kp_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.2)

  # TX Partners

  tx_map <- bmap +
    geom_sf(data = spdf_tx_cov,
            aes(fill = mech_name),
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_psnu,
                 aes(label = orgunit_name),
                 size = 2,
                 color = grey90k) +
    scale_fill_manual(
      values = c(
        "ACE 1" = scooter,
        "ACE 2" = denim,
        "ACE 3" = moody_blue,
        "ACE 4" = burnt_sienna,
        "ACE 5" = golden_sand,
        "ACE 6" = genoa,
        "RISE" = old_rose
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  tx_map

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - TX PROGRAM COVERAGE",
             ".png")),
    plot = tx_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.2)


