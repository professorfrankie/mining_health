#2003
rm(list = ls())
library(readr)
library(sf)
library(did)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(patchwork)
library(stringr)

# only with control group
# here G=0 if upstream, G=NA if never part of a basin chian, G=min_year if either mine or downstream
process_treatment_data <- function(
    path,
    year_min = 2003,
    year_max = 2019
) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  data_processed <- data_raw %>% 
    group_by(muni_id) %>%
    # remove municipalities with any dist_n > dist_n_max
    filter(!any(dist_n > 2, na.rm = TRUE)) %>%
    # remove municipalities with any mine
    filter(!any(status == "mine", na.rm = TRUE)) %>%
    ungroup() %>%
    # filter for years within the specified range
    filter(between(year, year_min, year_max)) %>%
    group_by(muni_id) %>%
    mutate(
      G = case_when(
        all(is.na(basin_id)) ~ NA_real_,
        all(is.na(status) | status == "upstream") ~ 0,
        any(status %in% c("downstream"), na.rm = TRUE) ~ {
          yrs <- year[
            status %in% c("downstream") &
              !is.na(basin_id) &
              !is.na(year)
          ]
          if (length(yrs) == 0) NA_real_ else min(yrs)
        },
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(G))
  
  return(data_processed)
}

treatment_capital_child_mort <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort_scaled.gpkg",
  )

treatment_combined_child_mort <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg",
  )

treatment_overlap_child_mort <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort_20_scaled.gpkg",
  )

treatment_capital_hosp <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp_scaled.gpkg",
  )

treatment_combined_hosp <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg",
  )

treatment_overlap_hosp <-
  process_treatment_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp_20_scaled.gpkg",
  )

### from 2002-2019 with controls only artisanal
treatment_capital_child_mort_art <- treatment_capital_child_mort %>%
  filter(mine_type == "artisanal")
treatment_combined_child_mort_art <- treatment_combined_child_mort %>%
  filter(mine_type == "artisanal")
treatment_overlap_child_mort_art <- treatment_overlap_child_mort %>%
  filter(mine_type == "artisanal")
treatment_capital_hosp_art <- treatment_capital_hosp %>%
  filter(mine_type == "artisanal")
treatment_combined_hosp_art <- treatment_combined_hosp %>%
  filter(mine_type == "artisanal")
treatment_overlap_hosp_art <- treatment_overlap_hosp %>%
  filter(mine_type == "artisanal")

### from 2002-2019 with controls and with artisanal mining share
treatment_capital_child_mort_art_share <- treatment_capital_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))
treatment_combined_child_mort_art_share <- treatment_combined_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))
treatment_overlap_child_mort_art_share <- treatment_overlap_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))
treatment_capital_hosp_art_share <- treatment_capital_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))
treatment_combined_hosp_art_share <- treatment_combined_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))
treatment_overlap_hosp_art_share <- treatment_overlap_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_artisanal_share > 0.75))

### from 2002-2019 with controls only industrial
treatment_capital_child_mort_ind <- treatment_capital_child_mort %>%
  filter(mine_type == "industrial")
treatment_combined_child_mort_ind <- treatment_combined_child_mort %>%
  filter(mine_type == "industrial")
treatment_overlap_child_mort_ind <- treatment_overlap_child_mort %>%
  filter(mine_type == "industrial")
treatment_capital_hosp_ind <- treatment_capital_hosp %>%
  filter(mine_type == "industrial")
treatment_combined_hosp_ind <- treatment_combined_hosp %>%
  filter(mine_type == "industrial")
treatment_overlap_hosp_ind <- treatment_overlap_hosp %>%
  filter(mine_type == "industrial")

### from 2002-2019 with controls and with industrial mining share
treatment_capital_child_mort_ind_share <- treatment_capital_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))
treatment_combined_child_mort_ind_share <- treatment_combined_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))
treatment_overlap_child_mort_ind_share <- treatment_overlap_child_mort %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))
treatment_capital_hosp_ind_share <- treatment_capital_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))
treatment_combined_hosp_ind_share <- treatment_combined_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))
treatment_overlap_hosp_ind_share <- treatment_overlap_hosp %>%
  group_by(muni_id) %>%
  filter(any(mining_ha_industrial_share > 0.75))

#impact decay
distance_decay_data <- function(
    path,
    dist_decay_n,
    year_min = 2003,
    year_max = 2019
) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  data_processed <- data_raw %>% 
    group_by(muni_id) %>%
    filter(!any(dist_n != dist_decay_n, na.rm = TRUE)) %>%
    # remove municipalities with any mine
    filter(!any(status == "mine", na.rm = TRUE)) %>%
    ungroup() %>%
    # filter for years within the specified range
    filter(between(year, year_min, year_max)) %>%
    group_by(muni_id) %>%
    mutate(
      G = case_when(
        all(is.na(basin_id)) ~ NA_real_,
        all(is.na(status) | status == "upstream") ~ 0,
        any(status %in% c("downstream"), na.rm = TRUE) ~ {
          yrs <- year[
            status %in% c("downstream") &
              !is.na(basin_id) &
              !is.na(year)
          ]
          if (length(yrs) == 0) NA_real_ else min(yrs)
        },
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(G))
  
  return(data_processed)
}

treatment_combined_child_mort_d3 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg",
    dist_decay_n = 3
  )

treatment_combined_hosp_d3 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg",
    dist_decay_n = 3
  )

treatment_combined_child_mort_d2 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg",
    dist_decay_n = 2
  )

treatment_combined_hosp_d2 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg",
    dist_decay_n = 2
  )

treatment_combined_child_mort_d1 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg",
    dist_decay_n = 1
  )

treatment_combined_hosp_d1 <-
  distance_decay_data(
    path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg",
    dist_decay_n = 1
  )

setwd("/home/francesca/brazil_mining/mining_health/figures/mine_opening_final/")

staggered_did <- function(
    d_var,
    dataframe
) {
  regression <- att_gt(yname = d_var,
                       tname = "year",
                       idname = "muni_id",
                       gname = "G",
                       xformla = ~ gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                       data = dataframe
  )
  return(regression)
}

dynamic_att <- function(
  att
  ) {
  dynamic <- aggte(att, type = "dynamic",
                      min_e = -8, max_e = 8,
                      na.rm = TRUE)
  return(dynamic)
}

event_study_plot <- function(
    agg.es,
    title.plot,
    subtitle.plot,
    y.lab
) {
  plot <- ggdid(agg.es) +
    ggplot2::labs(
      title = title.plot,
      subtitle = subtitle.plot,
      x = "Years relative to mine opening",
      y = y.lab
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        color = "black",
        size = 12,
        hjust = 0
      )
    )
  return(plot)
}


#baseline results
combined_child_0_2_full <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort
)
combined_child_0_2_full_agg.es <- dynamic_att(combined_child_0_2_full)
plot1 <- event_study_plot(
  agg.es = combined_child_0_2_full_agg.es,
  title.plot = "Child Mortality (0–2 years)",
  subtitle.plot = "",
  y.lab = "Rate per 1,000 inhabitants"
)
combined_intestinal_full <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp
)
combined_intestinal_full_agg.es <- dynamic_att(combined_intestinal_full)
plot2 <- event_study_plot(
  agg.es = combined_intestinal_full_agg.es,
  title.plot = "Intestinal Diseases",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_pregnancy_full <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp
)
combined_pregnancy_full_agg.es <- dynamic_att(combined_pregnancy_full)
plot3 <- event_study_plot(
  agg.es = combined_pregnancy_full_agg.es,
  title.plot = "Pregnancy-related Diseases",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_toxic_full <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp
)
combined_toxic_full_agg.es <- dynamic_att(combined_toxic_full)
plot4 <- event_study_plot(
  agg.es = combined_toxic_full_agg.es,
  title.plot = "Toxic Effects",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)

combined_full_sample <-
  (plot1 | plot2) /
  (plot3 | plot4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "combined_full_sample_event_studies.png",
  combined_full_sample,
  width = 10,
  height = 8,
  dpi = 300
)

#heterogeneity test

#artisanal
combined_child_0_2_art <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_art
)
combined_child_0_2_art_agg.es <- dynamic_att(combined_child_0_2_art)
plot5 <- event_study_plot(
  agg.es = combined_child_0_2_art_agg.es,
  title.plot = "Child Mortality (0–2 years)",
  subtitle.plot = "Artisanal Mining only",
  y.lab = "Rate per 1,000 inhabitants"
)
combined_intestinal_art <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_art
)
combined_intestinal_art_agg.es <- dynamic_att(combined_intestinal_art)
plot6 <- event_study_plot(
  agg.es = combined_intestinal_art_agg.es,
  title.plot = "Intestinal Diseases",
  subtitle.plot = "Artisanal Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_pregnancy_art <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_art
)
combined_pregnancy_art_agg.es <- dynamic_att(combined_pregnancy_art)
plot7 <- event_study_plot(
  agg.es = combined_pregnancy_art_agg.es,
  title.plot = "Pregnancy-related Diseases",
  subtitle.plot = "Artisanal Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_toxic_art <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_art
)
combined_toxic_art_agg.es <- dynamic_att(combined_toxic_art)
plot8 <- event_study_plot(
  agg.es = combined_toxic_art_agg.es,
  title.plot = "Toxic Effects",
  subtitle.plot = "Artisanal Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)

#industrial
combined_child_0_2_ind <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_ind
)
combined_child_0_2_ind_agg.es <- dynamic_att(combined_child_0_2_ind)
plot9 <- event_study_plot(
  agg.es = combined_child_0_2_ind_agg.es,
  title.plot = "Child Mortality (0–2 years)",
  subtitle.plot = "Industrial Mining only",
  y.lab = "Rate per 1,000 inhabitants"
)
combined_intestinal_ind <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_ind
)
combined_intestinal_ind_agg.es <- dynamic_att(combined_intestinal_ind)
plot10 <- event_study_plot(
  agg.es = combined_intestinal_ind_agg.es,
  title.plot = "Intestinal Diseases",
  subtitle.plot = "Industrial Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_pregnancy_ind <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_ind
)
combined_pregnancy_ind_agg.es <- dynamic_att(combined_pregnancy_ind)
plot11 <- event_study_plot(
  agg.es = combined_pregnancy_ind_agg.es,
  title.plot = "Pregnancy-related Diseases",
  subtitle.plot = "Industrial Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_toxic_ind <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_ind
)
combined_toxic_ind_agg.es <- dynamic_att(combined_toxic_ind)
plot12 <- event_study_plot(
  agg.es = combined_toxic_ind_agg.es,
  title.plot = "Toxic Effects",
  subtitle.plot = "Industrial Mining only",
  y.lab = "Rate per 100,000 inhabitants"
)

combined_hetero <-
  (plot5 | plot9) / 
  (plot6 | plot10) / 
  (plot7 | plot11) / 
  (plot8 | plot12) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
    legend.position = "bottom")
  )

ggsave(
  "combined_hetero_event_studies.png",
  combined_hetero,
  width = 10,
  height = 13,
  dpi = 300
)

#heterogeneity test share
#artisanal
combined_child_0_2_art_share <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_art_share
)
combined_child_0_2_art_share_agg.es <- dynamic_att(combined_child_0_2_art_share)
plot5a <- event_study_plot(
  agg.es = combined_child_0_2_art_share_agg.es,
  title.plot = "Child Mortality (0–2 years)",
  subtitle.plot = "Artisanal Mining > 75%",
  y.lab = "Rate per 1,000 inhabitants"
)
combined_intestinal_art_share <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_art_share
)
combined_intestinal_art_share_agg.es <- dynamic_att(combined_intestinal_art_share)
plot6a <- event_study_plot(
  agg.es = combined_intestinal_art_share_agg.es,
  title.plot = "Intestinal Diseases",
  subtitle.plot = "Artisanal Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_pregnancy_art_share <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_art_share
)
combined_pregnancy_art_share_agg.es <- dynamic_att(combined_pregnancy_art_share)
plot7a <- event_study_plot(
  agg.es = combined_pregnancy_art_share_agg.es,
  title.plot = "Pregnancy-related Diseases",
  subtitle.plot = "Artisanal Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_toxic_art_share <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_art_share
)
combined_toxic_art_share_agg.es <- dynamic_att(combined_toxic_art_share)
plot8a <- event_study_plot(
  agg.es = combined_toxic_art_share_agg.es,
  title.plot = "Toxic Effects",
  subtitle.plot = "Artisanal Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)

#industrial
combined_child_0_2_ind_share <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_ind_share
)
combined_child_0_2_ind_share_agg.es <- dynamic_att(combined_child_0_2_ind_share)
plot9a <- event_study_plot(
  agg.es = combined_child_0_2_ind_share_agg.es,
  title.plot = "Child Mortality (0–2 years)",
  subtitle.plot = "Industrial Mining > 75%",
  y.lab = "Rate per 1,000 inhabitants"
)
combined_intestinal_ind_share <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_ind_share
)
combined_intestinal_ind_share_agg.es <- dynamic_att(combined_intestinal_ind_share)
plot10a <- event_study_plot(
  agg.es = combined_intestinal_ind_share_agg.es,
  title.plot = "Intestinal Diseases",
  subtitle.plot = "Industrial Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_pregnancy_ind_share <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_ind_share
)
combined_pregnancy_ind_share_agg.es <- dynamic_att(combined_pregnancy_ind_share)
plot11a <- event_study_plot(
  agg.es = combined_pregnancy_ind_share_agg.es,
  title.plot = "Pregnancy-related Diseases",
  subtitle.plot = "Industrial Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_toxic_ind_share <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_ind_share
)
combined_toxic_ind_share_agg.es <- dynamic_att(combined_toxic_ind_share)
plot12a <- event_study_plot(
  agg.es = combined_toxic_ind_share_agg.es,
  title.plot = "Toxic Effects",
  subtitle.plot = "Industrial Mining > 75%",
  y.lab = "Rate per 100,000 inhabitants"
)

combined_hetero_share <-
  (plot5a | plot9a) / 
  (plot6a | plot10a) / 
  (plot7a | plot11a) / 
  (plot8a | plot12a) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom")
  )

ggsave(
  "combined_hetero_share_event_studies.png",
  combined_hetero_share,
  width = 10,
  height = 13,
  dpi = 300
)

#robustness checks

#stds
combined_stds_full <- staggered_did(
  d_var = "stds_pc",
  dataframe = treatment_combined_hosp
)
combined_stds_full_agg.es <- dynamic_att(combined_stds_full)
plot13 <- event_study_plot(
  agg.es = combined_stds_full_agg.es,
  title.plot = "",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)
ggsave("combined_stds_full.png", plot13, width = 8, height = 5, dpi = 300)

combined_stds_art <- staggered_did(
  d_var = "stds_pc",
  dataframe = treatment_combined_hosp_art
)
combined_stds_art_agg.es <- dynamic_att(combined_stds_art)
plot14 <- event_study_plot(
  agg.es = combined_stds_art_agg.es,
  title.plot = "Artisanal Mining only",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)
combined_stds_ind <- staggered_did(
  d_var = "stds_pc",
  dataframe = treatment_combined_hosp_ind
)
combined_stds_ind_agg.es <- dynamic_att(combined_stds_ind)
plot15 <- event_study_plot(
  agg.es = combined_stds_ind_agg.es,
  title.plot = "Industrial Mining only",
  subtitle.plot = "",
  y.lab = "Rate per 100,000 inhabitants"
)

combined_stds_art_ind <-
  (plot14 | plot15) +
  plot_layout(guides = "collect") +
  plot_annotation(
  tag_levels = "A",
  theme = theme(
    legend.position = "bottom")
)

ggsave(
  "combined_stds_art_ind_event_studies.png",
  combined_stds_art_ind,
  width = 10,
  height = 5,
  dpi = 300
)

#alternative specifications using different matching rules
#capital
capital_child_0_2_full <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_capital_child_mort
)
capital_child_0_2_full_agg.es <- dynamic_att(capital_child_0_2_full)
capital_intestinal_full <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_capital_hosp
)
capital_intestinal_full_agg.es <- dynamic_att(capital_intestinal_full)
capital_pregnancy_full <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_capital_hosp
)
capital_pregnancy_full_agg.es <- dynamic_att(capital_pregnancy_full)
capital_toxic_full <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_capital_hosp
)
capital_toxic_full_agg.es <- dynamic_att(capital_toxic_full)
#overlap
overlap_child_0_2_full <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_overlap_child_mort
)
overlap_child_0_2_full_agg.es <- dynamic_att(overlap_child_0_2_full)
overlap_intestinal_full <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_overlap_hosp
)
overlap_intestinal_full_agg.es <- dynamic_att(overlap_intestinal_full)
overlap_pregnancy_full <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_overlap_hosp
)
overlap_pregnancy_full_agg.es <- dynamic_att(overlap_pregnancy_full)
overlap_toxic_full <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_overlap_hosp
)
overlap_toxic_full_agg.es <- dynamic_att(overlap_toxic_full)

matching_rule_results_tidy <- rbind(
  tidy(capital_child_0_2_full_agg.es) |> transmute(d_var = "child_mort", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_full_agg.es) |> transmute(d_var = "child_mort", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_child_0_2_full_agg.es) |> transmute(d_var = "child_mort", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_intestinal_full_agg.es) |> transmute(d_var = "intestinal", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_agg.es) |> transmute(d_var = "intestinal", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_intestinal_full_agg.es) |> transmute(d_var = "intestinal", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_pregnancy_full_agg.es) |> transmute(d_var = "pregnancy", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_agg.es) |> transmute(d_var = "pregnancy", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_pregnancy_full_agg.es) |> transmute(d_var = "pregnancy", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_toxic_full_agg.es) |> transmute(d_var = "toxic_effect", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_agg.es) |> transmute(d_var = "toxic_effect", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_toxic_full_agg.es) |> transmute(d_var = "toxic_effect", type = "Area Overlap", event.time, estimate, conf.low, conf.high))
  

matching_rule_results_tidy$type <- factor(matching_rule_results_tidy$type,
                               levels = unique(matching_rule_results_tidy$type))

capital_overlap_child_mort <- matching_rule_results_tidy |>
  filter(d_var == "child_mort")
capital_overlap_intestinal <- matching_rule_results_tidy |>
  filter(d_var == "intestinal")
capital_overlap_pregnancy <- matching_rule_results_tidy |>
  filter(d_var == "pregnancy")
capital_overlap_toxic_effect <- matching_rule_results_tidy |>
  filter(d_var == "toxic_effect")

capital_overlap_plot <- function(
    df,
    title.disease
) {
  
  df_name <- deparse(substitute(df))
  
  y_label <- if (grepl("child", df_name)) {
    "Rate per 1,000 inhabitants"
  } else {
    "Rate per 100,000 inhabitants"
  }
  
  plot <- ggplot(df,
                 aes(x = event.time, y = estimate, fill = type, color = type, shape = type)) +
    geom_hline(aes(yintercept = 0), col = "black", linewidth = 0.25, lty = 2) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_vline(aes(xintercept = -0.5), col = "darkred", lty = 2) +
    geom_vline(aes(xintercept = 0.5), col = "darkred", lty = 2) +
    theme_minimal() +
    scale_color_manual(values=c("darkgreen", "grey", "darkmagenta")) +
    labs(title = title.disease,
         x = "Years relative to mine opening",
         y = y_label) +
    scale_x_continuous(labels = unique(capital_overlap_child_mort$event.time),
                       breaks = unique(capital_overlap_child_mort$event.time)) +
    theme(panel.grid.minor = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          panel.grid.major = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          plot.title = element_text(face = "bold", hjust = 0),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          axis.line = element_line(color = "black",
                                   linewidth = 0.5,
                                   linetype = 1))
  return(plot)
}


plot16 <- capital_overlap_plot(
  df = capital_overlap_child_mort,
  title.disease = "Child Mortality (0-2 years)"
)
plot17 <- capital_overlap_plot(
  df = capital_overlap_intestinal,
  title.disease = "Intestinal Diseases"
)
plot18 <- capital_overlap_plot(
  df = capital_overlap_pregnancy,
  title.disease = "Pregnancy-related Diseases"
)
plot19 <- capital_overlap_plot(
  df = capital_overlap_toxic_effect,
  title.disease = "Toxic Effects"
)

capital_overlap_full_sample <-
  (plot16 | plot17) /
  (plot18 | plot19) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "capital_overlap_full_sample_event_studies.png",
  capital_overlap_full_sample,
  width = 10,
  height = 8,
  dpi = 300
)

#alternative specifications using different matching rules artisanal
#capital
capital_child_0_2_art <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_capital_child_mort_art
)
capital_child_0_2_art_agg.es <- dynamic_att(capital_child_0_2_art)
capital_intestinal_art <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_capital_hosp_art
)
capital_intestinal_art_agg.es <- dynamic_att(capital_intestinal_art)
capital_pregnancy_art <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_capital_hosp_art
)
capital_pregnancy_art_agg.es <- dynamic_att(capital_pregnancy_art)
capital_toxic_art <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_capital_hosp_art
)
capital_toxic_art_agg.es <- dynamic_att(capital_toxic_art)
#overlap
overlap_child_0_2_art <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_overlap_child_mort_art
)
overlap_child_0_2_art_agg.es <- dynamic_att(overlap_child_0_2_art)
overlap_intestinal_art <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_overlap_hosp_art
)
overlap_intestinal_art_agg.es <- dynamic_att(overlap_intestinal_art)
overlap_pregnancy_art <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_overlap_hosp_art
)
overlap_pregnancy_art_agg.es <- dynamic_att(overlap_pregnancy_art)
overlap_toxic_art <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_overlap_hosp_art
)
overlap_toxic_art_agg.es <- dynamic_att(overlap_toxic_art)

matching_rule_results_tidy_art <- rbind(
  tidy(capital_child_0_2_art_agg.es) |> transmute(d_var = "child_mort", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_art_agg.es) |> transmute(d_var = "child_mort", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_child_0_2_art_agg.es) |> transmute(d_var = "child_mort", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_intestinal_art_agg.es) |> transmute(d_var = "intestinal", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_art_agg.es) |> transmute(d_var = "intestinal", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_intestinal_art_agg.es) |> transmute(d_var = "intestinal", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_pregnancy_art_agg.es) |> transmute(d_var = "pregnancy", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_art_agg.es) |> transmute(d_var = "pregnancy", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_pregnancy_art_agg.es) |> transmute(d_var = "pregnancy", type = "Area Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_toxic_art_agg.es) |> transmute(d_var = "toxic_effect", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_art_agg.es) |> transmute(d_var = "toxic_effect", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_toxic_art_agg.es) |> transmute(d_var = "toxic_effect", type = "Area Overlap", event.time, estimate, conf.low, conf.high))


matching_rule_results_tidy_art$type <- factor(matching_rule_results_tidy_art$type,
                                          levels = unique(matching_rule_results_tidy_art$type))

capital_overlap_child_mort_art <- matching_rule_results_tidy_art |>
  filter(d_var == "child_mort")
capital_overlap_intestinal_art <- matching_rule_results_tidy_art |>
  filter(d_var == "intestinal")
capital_overlap_pregnancy_art <- matching_rule_results_tidy_art |>
  filter(d_var == "pregnancy")
capital_overlap_toxic_effect_art <- matching_rule_results_tidy_art |>
  filter(d_var == "toxic_effect")

plot16a <- capital_overlap_plot(
  df = capital_overlap_child_mort_art,
  title.disease = "Child Mortality (0-2 years)"
)
plot17a <- capital_overlap_plot(
  df = capital_overlap_intestinal_art,
  title.disease = "Intestinal Diseases"
)
plot18a <- capital_overlap_plot(
  df = capital_overlap_pregnancy_art,
  title.disease = "Pregnancy-related Diseases"
)
plot19a <- capital_overlap_plot(
  df = capital_overlap_toxic_effect_art,
  title.disease = "Toxic Effects"
)

capital_overlap_art <-
  (plot16a | plot17a) /
  (plot18a | plot19a) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "capital_overlap_art_event_studies.png",
  capital_overlap_art,
  width = 10,
  height = 8,
  dpi = 300
)

#alternative specifications using different matching rules industrial
#capital
capital_child_0_2_ind <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_capital_child_mort_ind
)
capital_child_0_2_ind_agg.es <- dynamic_att(capital_child_0_2_ind)
capital_intestinal_ind <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_capital_hosp_ind
)
capital_intestinal_ind_agg.es <- dynamic_att(capital_intestinal_ind)
capital_pregnancy_ind <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_capital_hosp_ind
)
capital_pregnancy_ind_agg.es <- dynamic_att(capital_pregnancy_ind)
capital_toxic_ind <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_capital_hosp_ind
)
capital_toxic_ind_agg.es <- dynamic_att(capital_toxic_ind)
#overlap
overlap_child_0_2_ind <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_overlap_child_mort_ind
)
overlap_child_0_2_ind_agg.es <- dynamic_att(overlap_child_0_2_ind)
overlap_intestinal_ind <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_overlap_hosp_ind
)
overlap_intestinal_ind_agg.es <- dynamic_att(overlap_intestinal_ind)
overlap_pregnancy_ind <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_overlap_hosp_ind
)
overlap_pregnancy_ind_agg.es <- dynamic_att(overlap_pregnancy_ind)
overlap_toxic_ind <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_overlap_hosp_ind
)
overlap_toxic_ind_agg.es <- dynamic_att(overlap_toxic_ind)

matching_rule_results_tidy_ind <- rbind(
  tidy(capital_child_0_2_ind_agg.es) |> transmute(d_var = "child_mort", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_ind_agg.es) |> transmute(d_var = "child_mort", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_child_0_2_ind_agg.es) |> transmute(d_var = "child_mort", type = "Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_intestinal_ind_agg.es) |> transmute(d_var = "intestinal", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_ind_agg.es) |> transmute(d_var = "intestinal", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_intestinal_ind_agg.es) |> transmute(d_var = "intestinal", type = "Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_pregnancy_ind_agg.es) |> transmute(d_var = "pregnancy", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_ind_agg.es) |> transmute(d_var = "pregnancy", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_pregnancy_ind_agg.es) |> transmute(d_var = "pregnancy", type = "Overlap", event.time, estimate, conf.low, conf.high),
  tidy(capital_toxic_ind_agg.es) |> transmute(d_var = "toxic_effect", type = "Capital", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_ind_agg.es) |> transmute(d_var = "toxic_effect", type = "Combined", event.time, estimate, conf.low, conf.high),
  tidy(overlap_toxic_ind_agg.es) |> transmute(d_var = "toxic_effect", type = "Overlap", event.time, estimate, conf.low, conf.high))


matching_rule_results_tidy_ind$type <- factor(matching_rule_results_tidy_ind$type,
                                              levels = unique(matching_rule_results_tidy_ind$type))

capital_overlap_child_mort_ind <- matching_rule_results_tidy_ind |>
  filter(d_var == "child_mort")
capital_overlap_intestinal_ind <- matching_rule_results_tidy_ind |>
  filter(d_var == "intestinal")
capital_overlap_pregnancy_ind <- matching_rule_results_tidy_ind |>
  filter(d_var == "pregnancy")
capital_overlap_toxic_effect_ind <- matching_rule_results_tidy_ind |>
  filter(d_var == "toxic_effect")

plot16i <- capital_overlap_plot(
  df = capital_overlap_child_mort_ind,
  title.disease = "Child Mortality (0-2 years)"
)
plot17i <- capital_overlap_plot(
  df = capital_overlap_intestinal_ind,
  title.disease = "Intestinal Diseases"
)
plot18i <- capital_overlap_plot(
  df = capital_overlap_pregnancy_ind,
  title.disease = "Pregnancy-related Diseases"
)
plot19i <- capital_overlap_plot(
  df = capital_overlap_toxic_effect_ind,
  title.disease = "Toxic Effects"
)

capital_overlap_ind <-
  (plot16i | plot17i) /
  (plot18i | plot19i) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "capital_overlap_ind_event_studies.png",
  capital_overlap_ind,
  width = 10,
  height = 8,
  dpi = 300
)

#alternative specifications using different distances
#d1
combined_child_0_2_full_d1 <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_d1
)
combined_child_0_2_full_d1_agg.es <- dynamic_att(combined_child_0_2_full_d1)
combined_intestinal_full_d1 <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_d1
)
combined_intestinal_full_d1_agg.es <- dynamic_att(combined_intestinal_full_d1)
combined_pregnancy_full_d1 <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_d1
)
combined_pregnancy_full_d1_agg.es <- dynamic_att(combined_pregnancy_full_d1)
combined_toxic_full_d1 <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_d1
)
combined_toxic_full_d1_agg.es <- dynamic_att(combined_toxic_full_d1)

#d2
combined_child_0_2_full_d2 <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_d2
)
combined_child_0_2_full_d2_agg.es <- dynamic_att(combined_child_0_2_full_d2)
combined_intestinal_full_d2 <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_d2
)
combined_intestinal_full_d2_agg.es <- dynamic_att(combined_intestinal_full_d2)
combined_pregnancy_full_d2 <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_d2
)
combined_pregnancy_full_d2_agg.es <- dynamic_att(combined_pregnancy_full_d2)
combined_toxic_full_d2 <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_d2
)
combined_toxic_full_d2_agg.es <- dynamic_att(combined_toxic_full_d2)

#d3
combined_child_0_2_full_d3 <- staggered_did(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort_d3
)
combined_child_0_2_full_d3_agg.es <- dynamic_att(combined_child_0_2_full_d3)
combined_intestinal_full_d3 <- staggered_did(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp_d3
)
combined_intestinal_full_d3_agg.es <- dynamic_att(combined_intestinal_full_d3)
combined_pregnancy_full_d3 <- staggered_did(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp_d3
)
combined_pregnancy_full_d3_agg.es <- dynamic_att(combined_pregnancy_full_d3)
combined_toxic_full_d3 <- staggered_did(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp_d3
)
combined_toxic_full_d3_agg.es <- dynamic_att(combined_toxic_full_d3)

distance_results_tidy <- rbind(
  tidy(combined_child_0_2_full_d1_agg.es) |> transmute(d_var = "child_mort", type = "Distance Order 1", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_full_d2_agg.es) |> transmute(d_var = "child_mort", type = "Distance Order 2", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_full_d3_agg.es) |> transmute(d_var = "child_mort", type = "Distance Order 3", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_d1_agg.es) |> transmute(d_var = "intestinal", type = "Distance Order 1", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_d2_agg.es) |> transmute(d_var = "intestinal", type = "Distance Order 2", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_d3_agg.es) |> transmute(d_var = "intestinal", type = "Distance Order 3", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_d1_agg.es) |> transmute(d_var = "pregnancy", type = "Distance Order 1", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_d2_agg.es) |> transmute(d_var = "pregnancy", type = "Distance Order 2", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_d3_agg.es) |> transmute(d_var = "pregnancy", type = "Distance Order 3", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_d1_agg.es) |> transmute(d_var = "toxic_effect", type = "Distance Order 1", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_d2_agg.es) |> transmute(d_var = "toxic_effect", type = "Distance Order 2", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_d3_agg.es) |> transmute(d_var = "toxic_effect", type = "Distance Order 3", event.time, estimate, conf.low, conf.high))


distance_results_tidy$type <- factor(distance_results_tidy$type,
                                     levels = unique(distance_results_tidy$type))

distance_child_mort <- distance_results_tidy |>
  filter(d_var == "child_mort")
distance_intestinal <- distance_results_tidy |>
  filter(d_var == "intestinal")
distance_pregnancy <- distance_results_tidy |>
  filter(d_var == "pregnancy")
distance_toxic_effect <- distance_results_tidy |>
  filter(d_var == "toxic_effect")

distance_decay_plot <- function(
    df,
    title.disease
) {
    df_name <- deparse(substitute(df))
    
    y_label <- if (grepl("child", df_name)) {
      "Rate per 1,000 inhabitants"
    } else {
      "Rate per 100,000 inhabitants"
    }
    
  plot <- ggplot(df,
                 aes(x = event.time, y = estimate, fill = type, color = type, shape = type)) + #  Francesca, here you would probably have to add something on the grouping variable, which could be your different robustness specifications
    geom_hline(aes(yintercept = 0), col = "black", linewidth = 0.25, lty = 2) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_vline(aes(xintercept = -0.5), col = "darkred", lty = 2) +
    geom_vline(aes(xintercept = 0.5), col = "darkred", lty = 2) +
    theme_minimal() +
    scale_color_manual(values=c("darkgreen", "darkgoldenrod1", "darkmagenta")) +
    labs(title = title.disease,
         x = "Years relative to mine opening",
         y = y_label) +
    scale_x_continuous(labels = unique(capital_overlap_child_mort$event.time),
                       breaks = unique(capital_overlap_child_mort$event.time)) +
    theme(panel.grid.minor = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          panel.grid.major = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          plot.title = element_text(face = "bold", hjust = 0),
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          axis.line = element_line(color = "black",
                                   linewidth = 0.5,
                                   linetype = 1),
          legend.position = "bottom")
  return(plot)
}

plot20 <- distance_decay_plot(
  df = distance_child_mort,
  title.disease = "Child Mortality (0-2 years)"
)
plot21 <- distance_decay_plot(
  df = distance_intestinal,
  title.disease = "Intestinal Diseases"
)
plot22 <- distance_decay_plot(
  df = distance_pregnancy,
  title.disease = "Pregnancy-related Diseases"
)
plot23 <- distance_decay_plot(
  df = distance_toxic_effect,
  title.disease = "Toxic Effects"
)

distance_full_sample <-
  (plot20 | plot21) /
  (plot22 | plot23) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "distance_full_sample_event_studies.png",
  distance_full_sample,
  width = 10,
  height = 8,
  dpi = 300
)

#baseline results weighted
staggered_did_weighted <- function(
    d_var,
    dataframe
) {
  regression <- att_gt(yname = d_var,
                       tname = "year",
                       idname = "muni_id",
                       gname = "G",
                       xformla = ~ gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                       data = dataframe,
                       weightsname = "pop"
  )
  return(regression)
}


#baseline results
combined_child_0_2_full_weighted <- staggered_did_weighted(
  d_var = "child_mort_pc_0_2",
  dataframe = treatment_combined_child_mort
)
combined_child_0_2_full_weighted_agg.es <- dynamic_att(combined_child_0_2_full_weighted)
combined_intestinal_full_weighted <- staggered_did_weighted(
  d_var = "intestinal_diseases_pc",
  dataframe = treatment_combined_hosp
)
combined_intestinal_full_weighted_agg.es <- dynamic_att(combined_intestinal_full_weighted)
combined_pregnancy_full_weighted <- staggered_did_weighted(
  d_var = "pregnancy_pc",
  dataframe = treatment_combined_hosp
)
combined_pregnancy_full_weighted_agg.es <- dynamic_att(combined_pregnancy_full_weighted)
combined_toxic_full_weighted <- staggered_did_weighted(
  d_var = "toxic_effects_pc",
  dataframe = treatment_combined_hosp
)
combined_toxic_full_weighted_agg.es <- dynamic_att(combined_toxic_full_weighted)


weighted_results_tidy <- rbind(
  tidy(combined_child_0_2_full_agg.es) |> transmute(d_var = "child_mort", type = "Baseline Results", event.time, estimate, conf.low, conf.high),
  tidy(combined_child_0_2_full_weighted_agg.es) |> transmute(d_var = "child_mort", type = "Baseline Results with Population Weights", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_agg.es) |> transmute(d_var = "intestinal", type = "Baseline Results", event.time, estimate, conf.low, conf.high),
  tidy(combined_intestinal_full_weighted_agg.es) |> transmute(d_var = "intestinal", type = "Baseline Results with Population Weights", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_agg.es) |> transmute(d_var = "pregnancy", type = "Baseline Results", event.time, estimate, conf.low, conf.high),
  tidy(combined_pregnancy_full_weighted_agg.es) |> transmute(d_var = "pregnancy", type = "Baseline Results with Population Weights", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_agg.es) |> transmute(d_var = "toxic_effect", type = "Baseline Results", event.time, estimate, conf.low, conf.high),
  tidy(combined_toxic_full_weighted_agg.es) |> transmute(d_var = "toxic_effect", type = "Baseline Results with Population Weights", event.time, estimate, conf.low, conf.high))

weighted_results_tidy$type <- factor(weighted_results_tidy$type,
                                     levels = unique(weighted_results_tidy$type))

weighted_child_mort <- weighted_results_tidy |>
  filter(d_var == "child_mort")
weighted_intestinal <- weighted_results_tidy |>
  filter(d_var == "intestinal")
weighted_pregnancy <- weighted_results_tidy |>
  filter(d_var == "pregnancy")
weighted_toxic_effect <- weighted_results_tidy |>
  filter(d_var == "toxic_effect")

weights_plot <- function(
    df,
    title.disease
) {
  df_name <- deparse(substitute(df))
  
  y_label <- if (grepl("child", df_name)) {
    "Rate per 1,000 inhabitants"
  } else {
    "Rate per 100,000 inhabitants"
  }
  
  plot <- ggplot(df,
                 aes(x = event.time, y = estimate, fill = type, color = type, shape = type)) + #  Francesca, here you would probably have to add something on the grouping variable, which could be your different robustness specifications
    geom_hline(aes(yintercept = 0), col = "black", linewidth = 0.25, lty = 2) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_vline(aes(xintercept = -0.5), col = "darkred", lty = 2) +
    geom_vline(aes(xintercept = 0.5), col = "darkred", lty = 2) +
    theme_minimal() +
    scale_color_manual(values=c("grey", "darkgreen")) +
    labs(title = title.disease,
         x = "Years relative to mine opening",
         y = y_label) +
    scale_x_continuous(labels = unique(capital_overlap_child_mort$event.time),
                       breaks = unique(capital_overlap_child_mort$event.time)) +
    theme(panel.grid.minor = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          panel.grid.major = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
          plot.title = element_text(face = "bold", hjust = 0),
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          axis.line = element_line(color = "black",
                                   linewidth = 0.5,
                                   linetype = 1),
          legend.position = "bottom")
  return(plot)
}

plot1w <- weights_plot(
  df = weighted_child_mort,
  title.disease = "Child Mortality (0-2 years)"
)
plot2w <- weights_plot(
  df = weighted_intestinal,
  title.disease = "Intestinal Diseases"
)
plot3w <- weights_plot(
  df = weighted_pregnancy,
  title.disease = "Pregnancy-related Diseases"
)
plot4w <- weights_plot(
  df = weighted_toxic_effect,
  title.disease = "Toxic Effects"
)

combined_full_sample_weighted <-
  (plot1w | plot2w) /
  (plot3w | plot4w) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom"
    )
  )

ggsave(
  "combined_full_sample_event_studies_weighted.png",
  combined_full_sample_weighted,
  width = 10,
  height = 8,
  dpi = 300
)


#treatment control table
count_treat_control <- function(df) {
  df %>%
    ungroup() %>% 
    distinct(muni_id, G) %>%
    mutate(group = if_else(G == 0, "control_upstream", "treated_downstream")) %>%
    count(group)
}
count_treat_control(treatment_combined_child_mort)
count_treat_control(treatment_combined_child_mort_art)
count_treat_control(treatment_combined_child_mort_art_share)
count_treat_control(treatment_combined_child_mort_ind)
count_treat_control(treatment_combined_child_mort_ind_share)
count_treat_control(treatment_capital_child_mort)
count_treat_control(treatment_overlap_child_mort)
count_treat_control(treatment_combined_child_mort_d1)
count_treat_control(treatment_combined_child_mort_d2)
count_treat_control(treatment_combined_child_mort_d3)
