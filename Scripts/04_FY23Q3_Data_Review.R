# PURPOSE: CDI Support - Data Review
# AUTHOR: Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: FY23Q3i Data Review
# REF ID: 8387bcfe
# LICENSE: MIT
# DATE: 2023-08-22
# UPDATE: 2023-10-10
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries

    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(cascade)
    library(mindthegap)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)

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

    meta$source <- meta$source %>% paste("- Ref. ID =", ref_id)

  # Functions

# LOAD DATA ============================================================================

  df_msd_nat <- file_nat %>% read_psd()
  df_msd_psnu <- file_psnu %>% read_psd()

  #df_est <- pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)

  #df_tt <- pull_unaids(data_type = "Test & Treat - Percent", pepfar_only = TRUE)

  ## EPI Control

  #epi_plot(sel_cntry = cntry)

# MUNGE ============================================================================

  # Cascade Data
  df_cascade <- df_msd_psnu %>%
      return_cascade(cscd_num = 1)

  df_cascade %>% glimpse()

  # TX Traitment

  df_msd_psnu %>%
    filter(operatingunit == cntry,
           str_detect(indicator, "TX_")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator) %>%
    prinf()

  df_tx <- df_msd_psnu %>%
    filter(operatingunit == cntry,
           str_detect(indicator, "TX_"),
           standardizeddisaggregate == "Total Numerator")

  df_tx %>% distinct(indicator)

  df_tx_curr <- df_tx %>%
    filter(indicator == "TX_CURR" |
           str_detect(indicator, "TX_ML_IIT")) %>% #distinct(indicator) %>%
    mutate(indicator = case_when(
      str_detect(indicator, "TX_ML_IIT") ~ "TX_ML_IIT",
      TRUE ~ indicator
    )) %>%
    select(-targets) %>%
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, snu1uid, snu1, indicator)) %>%
    reshape_msd() %>%
    select(-period_type) %>%
    filter(str_detect(period, "FY24", negate = T)) %>%
    # mutate(value = case_when(
    #   indicator == "TX_ML_IIT" ~ value * -1,
    #   TRUE ~ value
    # ))
    pivot_wider(names_from = indicator, values_from = value) %>%
    group_by(snu1uid, snu1) %>%
    mutate(IIT_Share = TX_ML_IIT / lag(TX_CURR, 1, order_by = period)) %>%
    ungroup()

  df_tx_loss <- df_msd_psnu %>%
    filter(operatingunit == cntry,
           str_detect(indicator, "TX_"),
           standardizeddisaggregate %in%
             c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, "TX_ML_IIT") ~ "TX_ML_IIT",
        TRUE ~ indicator
      ),
      age_group = case_when(
        trendscoarse == "<15" ~ "<15",
        sex == "Male" & trendscoarse == "15+" ~ "M15+",
        sex == "Female" & trendscoarse == "15+" ~ "F15+",
        TRUE ~ "Others"
      )) %>%
    select(-targets) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, snu1uid, snu1,
                      psnuuid, psnu, indicator, age_group)) %>% #view
    reshape_msd() %>%
    filter(period_type == "cumulative") %>%
    select(-period_type)

  df_tx_loss <- df_tx_loss %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, snu1uid, snu1,
                      psnuuid, psnu, indicator)) %>%
    mutate(age_group = "All") %>%
    bind_rows(df_tx_loss, .) %>%
    filter(indicator %in% c("TX_ML_IIT", "TX_RTT"))

  df_tx_loss %>%
    group_by(period, snu1uid, snu1, psnuuid, psnu, age_group) %>%
    summarise(value = value[indicator=="TX_RTT"] - value[indicator == "TX_ML_IIT"]) %>%
    ungroup() %>%
    mutate(indicator = "GAIN-LOSS") %>%
    bind_rows(df_tx_loss, .) %>%
    pivot_wider(names_from = c(indicator, age_group), values_from = value,
                names_sort = T)



# VIZ ============================================================================

  ## Check cascade plots
  cascade::plot_name

  cascase_standard <- return_cascade_plot(
    msd_df = df_msd_psnu,
    export = T,
    path = dir_images)

  si_save(filename = file.path(dir_graphics, "CDI - Standard Cascade.png"),
          plot = cascase_standard,
          scale = 2,
          width = 10,
          height = 5)

  # TX Growth

  # plot_tx_curr <- df_tx_curr %>%
  #   filter(snu1 == "Abidjan 2") %>%
  #   ggplot(aes(x = period, y = value, group = indicator)) +
  #   geom_line(aes(color = indicator, group = indicator), linewidth = 1.5) +
  #   geom_point(aes(fill = indicator, group = indicator), shape = 21, color = grey10k, size = 5) +
  #   geom_text(data = df_tx_curr %>%
  #               filter(snu1 == "Abidjan 2",
  #                      indicator == "TX_CURR",
  #                      str_detect(period, "FY21Q2|FY21Q3|FY21Q4|FY22Q1|FY22Q2|FY22Q3", negate = T)),
  #             aes(x = period, y = value, label = comma(value), color = indicator),
  #             size = 6, color = usaid_black, vjust = -1.2, hjust = "inward") +
  #   geom_text(data = df_tx_curr %>%
  #               filter(snu1 == "Abidjan 2",
  #                      indicator == "TX_ML_IIT",
  #                      str_detect(period, "FY21Q2|FY21Q3|FY21Q4|FY22Q1|FY22Q2|FY22Q3", negate = T)),
  #             aes(x = period, y = value, label = comma(value), color = indicator),
  #             size = 6, color = usaid_black, vjust = 1.2, hjust = "inward") +
  #   scale_color_manual(values = c("TX_CURR" = scooter, "TX_ML_IIT" = old_rose)) +
  #   scale_fill_manual(values = c("TX_CURR" = scooter, "TX_ML_IIT" = old_rose)) +
  #   scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
  #                      breaks = seq(0, 60000, 10000)) +
  #   coord_cartesian(clip = "off") +
  #   labs(x = "", y = "",
  #        caption = glue("{meta$source} - Updated on {curr_date()}")) +
  #   si_style() +
  #   theme(legend.title = element_blank())

  plot_tx_curr1 <- df_tx_curr %>%
    filter(snu1 == "Abidjan 2") %>%
    ggplot(aes(x = period, y = TX_CURR, group = 1)) +
    geom_line(color = scooter, linewidth = 1.5) +
    geom_point(fill = scooter, shape = 21, color = grey10k, size = 5) +
    geom_text(data = df_tx_curr %>%
                filter(snu1 == "Abidjan 2",
                       str_detect(period, "FY21Q2|FY21Q3|FY21Q4|FY22Q1|FY22Q2|FY22Q3", negate = T)),
              aes(x = period, y = TX_CURR, label = comma(TX_CURR)),
              size = 6, color = usaid_black, vjust = -1.2, hjust = "inward") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    coord_cartesian(clip = "off") +
    labs(x = "", y = "",
         #caption = glue("{meta$source} - Updated on {curr_date()}")
         ) +
    si_style() +
    theme(legend.title = element_blank(),
          axis.text.x = element_blank())

  plot_tx_curr1

  plot_tx_iit1 <- df_tx_curr %>%
    filter(snu1 == "Abidjan 2") %>%
    ggplot(aes(x = period, y = IIT_Share, group = 1)) +
    geom_line(color = old_rose, linewidth = 1.5) +
    geom_point(fill = old_rose, shape = 21, color = grey10k, size = 5) +
    geom_text(data = df_tx_curr %>%
                filter(snu1 == "Abidjan 2",
                       !is.na(IIT_Share),
                       str_detect(period, "FY21Q4|FY22Q1|FY22Q2|FY23Q1", negate = T)),
              aes(x = period, y = IIT_Share, label = percent(IIT_Share, .1)),
              size = 6, color = usaid_black, vjust = -1.2, hjust = "inward") +
    scale_y_continuous(labels = percent) +
    #coord_cartesian(clip = "off") +
    labs(x = "", y = "",
         caption = glue("{meta$source} - Updated on {curr_date()}")) +
    si_style() +
    theme(legend.title = element_blank())


  plot_tx_iit1

  plot_tx_loss <- plot_tx_curr1 / plot_tx_iit1

  plot_tx_loss

  si_save(filename = file.path(dir_graphics, "CDI - Historical Treatment Growth - Abidjan 2.png"),
          plot = plot_tx_loss,
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)


# OUTPUTS ============================================================================

