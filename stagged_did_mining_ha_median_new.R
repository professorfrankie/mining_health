library(readr)
library(sf)
library(did)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyselect)

# only with control group
# here G=0 if upstream, G=NA if never part of a basin chian, G=min_year if either mine or downstream
process_treatment_data <- function(path) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  # 1. Compute global median cutoff (mine basin–year level)
  mining_median_mine <- data_raw %>%
    filter(status == "mine") %>%
    group_by(mine_basin, year) %>%
    summarise(
      mining_ha_total = first(mining_ha_total),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining_ha_total, na.rm = TRUE)
    ) %>%
    pull(median)
  
  # 2. Apply cutoff and construct G
  data_processed <- data_raw %>% 
    group_by(muni_id) %>% 
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>% 
    ungroup() %>% 
    
    mutate(
      mining_median_mine = mining_median_mine,
      crossed = mining_ha_total > mining_median_mine
    ) %>%
    
    group_by(muni_id) %>% 
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>% 
    ungroup() %>% 
    filter(!is.na(G))
  
  return(data_processed)
}
treatment_capital_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg"
)

treatment_combined_child_mort <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg"
)

treatment_overlap_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg"
)

treatment_capital_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg"
)

treatment_combined_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg"
)

treatment_overlap_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg"
)

compute_median <- function(data, mining_var) {
  data %>%
    filter(status == "mine") %>%
    st_set_geometry(NULL) %>%  # drop geometry before summarising
    group_by(mine_basin, year) %>%
    summarise(
      mining = first(.data[[mining_var]]),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining, na.rm = TRUE)
    ) %>%
    pull(median)
}

build_treatment <- function(
    path,
    mining_var,
    year_min = 1998,
    year_max = 2019,
    mine_type_filter = NULL,
    share_var = NULL,
    share_cutoff = NULL
) {
  
  # Read data and drop geometry immediately
  data_raw <- st_read(path) %>%
    # st_set_geometry(NULL) %>%   <--- drop geometry
    filter(between(year, year_min, year_max))
  
  # Optional filters
  if (!is.null(mine_type_filter)) {
    data_raw <- data_raw %>% filter(mine_type == mine_type_filter)
  }
  
  if (!is.null(share_var)) {
    data_raw <- data_raw %>%
      group_by(muni_id) %>%
      filter(any(.data[[share_var]] > share_cutoff)) %>%
      ungroup()
  }
  
  # Compute cutoff ONCE
  median <- compute_median(data_raw, mining_var)
  
  # Apply treatment logic
  data_raw %>%
    group_by(muni_id) %>%
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      crossed = .data[[mining_var]] > median
    ) %>%
    group_by(muni_id) %>%
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(G))
}

# artisanal only
treatment_capital_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_capital_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

#artisanal share >0.5
treatment_capital_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_capital_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

#industrial only
treatment_capital_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_capital_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)

#industrial share >0.5
treatment_capital_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_capital_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)


### from 2002-2019 with controls
treatment_capital_child_mort_2019 <- treatment_capital_child_mort %>%
  filter(between(year, 1998, 2019))
treatment_combined_child_mort_2019 <- treatment_combined_child_mort %>%
  filter(between(year, 1998, 2019))
treatment_overlap_child_mort_2019 <- treatment_overlap_child_mort %>%
  filter(between(year, 1998, 2019))
treatment_capital_hosp_2019 <- treatment_capital_hosp %>%
  filter(between(year, 1998, 2019))
treatment_combined_hosp_2019 <- treatment_combined_hosp %>%
  filter(between(year, 1998, 2019))
treatment_overlap_hosp_2019 <- treatment_overlap_hosp %>%
  filter(between(year, 1998, 2019))

### from 2002-2019 with controls only legal amazon
treatment_capital_child_mort_2019_amazon <- treatment_capital_child_mort %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_child_mort_2019_amazon <- treatment_combined_child_mort %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_child_mort_2019_amazon <- treatment_overlap_child_mort %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)
treatment_capital_hosp_2019_amazon <- treatment_capital_hosp %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_hosp_2019_amazon <- treatment_combined_hosp %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_hosp_2019_amazon <- treatment_overlap_hosp %>%
  filter(between(year, 1998, 2019)) %>%
  filter(legal_amazon == 1)

setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_1998/")


#capital
capital_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                      data = treatment_capital_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019)
capital_toxic_controls_2019_agg.es <- aggte(capital_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_agg.es)
f1 <- ggdid(capital_toxic_controls_2019_agg.es, title = "capital_toxic_controls_2019")
ggsave("capital_toxic_controls_2019.png", f1, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_capital_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019)
capital_intestinal_controls_2019_agg.es <- aggte(capital_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_agg.es)
f2 <- ggdid(capital_intestinal_controls_2019_agg.es, title = "capital_intestinal_controls_2019")
ggsave("capital_intestinal_controls_2019.png", f2, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_capital_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019)
capital_pregnancy_controls_2019_agg.es <- aggte(capital_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_agg.es)
f3 <- ggdid(capital_pregnancy_controls_2019_agg.es, title = "capital_pregnancy_controls_2019")
ggsave("capital_pregnancy_controls_2019.png", f3, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                       tname = "year",
                                       idname = "muni_id",
                                       gname = "G",
                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                       data = treatment_combined_hosp_2019,
                                       #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019)
combined_toxic_controls_2019_agg.es <- aggte(combined_toxic_controls_2019, type = "dynamic",
                                             min_e = -10, max_e = 10,
                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_agg.es)
f4 <- ggdid(combined_toxic_controls_2019_agg.es, title = "combined_toxic_controls_2019")
ggsave("combined_toxic_controls_2019.png", f4, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                            tname = "year",
                                            idname = "muni_id",
                                            gname = "G",
                                            xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                            data = treatment_combined_hosp_2019,
                                            #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019)
combined_intestinal_controls_2019_agg.es <- aggte(combined_intestinal_controls_2019, type = "dynamic",
                                                  min_e = -10, max_e = 10,
                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_agg.es)
f5 <- ggdid(combined_intestinal_controls_2019_agg.es, title = "combined_intestinal_controls_2019")
ggsave("combined_intestinal_controls_2019.png", f5, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_combined_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019)
combined_pregnancy_controls_2019_agg.es <- aggte(combined_pregnancy_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_agg.es)
f6 <- ggdid(combined_pregnancy_controls_2019_agg.es, title = "combined_pregnancy_controls_2019")
ggsave("combined_pregnancy_controls_2019.png", f6, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                      data = treatment_overlap_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019)
overlap_toxic_controls_2019_agg.es <- aggte(overlap_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_agg.es)
f7 <- ggdid(overlap_toxic_controls_2019_agg.es, title = "overlap_toxic_controls_2019")
ggsave("overlap_toxic_controls_2019.png", f7, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_overlap_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019)
overlap_intestinal_controls_2019_agg.es <- aggte(overlap_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_agg.es)
f8 <- ggdid(overlap_intestinal_controls_2019_agg.es, title = "overlap_intestinal_controls_2019")
ggsave("overlap_intestinal_controls_2019.png", f8, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_overlap_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019)
overlap_pregnancy_controls_2019_agg.es <- aggte(overlap_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_agg.es)
f9 <- ggdid(overlap_pregnancy_controls_2019_agg.es, title = "overlap_pregnancy_controls_2019")
ggsave("overlap_pregnancy_controls_2019.png", f9, width = 8, height = 5, dpi = 300)


#capital
capital_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_capital_hosp_2019_art,
)
summary(capital_toxic_controls_2019_art)
capital_toxic_controls_2019_art_agg.es <- aggte(capital_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_art_agg.es)
f10 <- ggdid(capital_toxic_controls_2019_art_agg.es, title = "capital_toxic_controls_2019_art")
ggsave("capital_toxic_controls_2019_art.png", f10, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_capital_hosp_2019_art,
)
summary(capital_intestinal_controls_2019_art)
capital_intestinal_controls_2019_art_agg.es <- aggte(capital_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_agg.es)
f11 <- ggdid(capital_intestinal_controls_2019_art_agg.es, title = "capital_intestinal_controls_2019_art")
ggsave("capital_intestinal_controls_2019_art.png", f11, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_capital_hosp_2019_art,
)
summary(capital_pregnancy_controls_2019_art)
capital_pregnancy_controls_2019_art_agg.es <- aggte(capital_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_agg.es)
f12 <- ggdid(capital_pregnancy_controls_2019_art_agg.es, title = "capital_pregnancy_controls_2019_art")
ggsave("capital_pregnancy_controls_2019_art.png", f12, width = 8, height = 5, dpi = 300)


#combined
combined_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_combined_hosp_2019_art,
)
summary(combined_toxic_controls_2019_art)
combined_toxic_controls_2019_art_agg.es <- aggte(combined_toxic_controls_2019_art, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_art_agg.es)
f13 <- ggdid(combined_toxic_controls_2019_art_agg.es, title = "combined_toxic_controls_2019_art")
ggsave("combined_toxic_controls_2019_art.png", f13, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_combined_hosp_2019_art,
)
summary(combined_intestinal_controls_2019_art)
combined_intestinal_controls_2019_art_agg.es <- aggte(combined_intestinal_controls_2019_art, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_agg.es)
f14 <- ggdid(combined_intestinal_controls_2019_art_agg.es, title = "combined_intestinal_controls_2019_art")
ggsave("combined_intestinal_controls_2019_art.png", f14, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_combined_hosp_2019_art,
)
summary(combined_pregnancy_controls_2019_art)
combined_pregnancy_controls_2019_art_agg.es <- aggte(combined_pregnancy_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_agg.es)
f15 <- ggdid(combined_pregnancy_controls_2019_art_agg.es, title = "combined_pregnancy_controls_2019_art")
ggsave("combined_pregnancy_controls_2019_art.png", f15, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_overlap_hosp_2019_art,
)
summary(overlap_toxic_controls_2019_art)
overlap_toxic_controls_2019_art_agg.es <- aggte(overlap_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_agg.es)
f16 <- ggdid(overlap_toxic_controls_2019_art_agg.es, title = "overlap_toxic_controls_2019_art")
ggsave("overlap_toxic_controls_2019_art.png", f16, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_overlap_hosp_2019_art,
)
summary(overlap_intestinal_controls_2019_art)
overlap_intestinal_controls_2019_art_agg.es <- aggte(overlap_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_agg.es)
f17 <- ggdid(overlap_intestinal_controls_2019_art_agg.es, title = "overlap_intestinal_controls_2019_art")
ggsave("overlap_intestinal_controls_2019_art.png", f17, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_overlap_hosp_2019_art,
)
summary(overlap_pregnancy_controls_2019_art)
overlap_pregnancy_controls_2019_art_agg.es <- aggte(overlap_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_agg.es)
f18 <- ggdid(overlap_pregnancy_controls_2019_art_agg.es, title = "overlap_pregnancy_controls_2019_art")
ggsave("overlap_pregnancy_controls_2019_art.png", f18, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_capital_hosp_2019_art_share,
)
summary(capital_toxic_controls_2019_art_share)
capital_toxic_controls_2019_art_share_agg.es <- aggte(capital_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_agg.es)
f19 <- ggdid(capital_toxic_controls_2019_art_share_agg.es, title = "capital_toxic_controls_2019_art_share")
ggsave("capital_toxic_controls_2019_art_share.png", f19, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_capital_hosp_2019_art_share,
)
summary(capital_intestinal_controls_2019_art_share)
capital_intestinal_controls_2019_art_share_agg.es <- aggte(capital_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_agg.es)
f20 <- ggdid(capital_intestinal_controls_2019_art_share_agg.es, title = "capital_intestinal_controls_2019_art_share")
ggsave("capital_intestinal_controls_2019_art_share.png", f20, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_capital_hosp_2019_art_share,
)
summary(capital_pregnancy_controls_2019_art_share)
capital_pregnancy_controls_2019_art_share_agg.es <- aggte(capital_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_agg.es)
f21 <- ggdid(capital_pregnancy_controls_2019_art_share_agg.es, title = "capital_pregnancy_controls_2019_art_share")
ggsave("capital_pregnancy_controls_2019_art_share.png", f21, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_combined_hosp_2019_art_share,
)
summary(combined_toxic_controls_2019_art_share)
combined_toxic_controls_2019_art_share_agg.es <- aggte(combined_toxic_controls_2019_art_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_agg.es)
f22 <- ggdid(combined_toxic_controls_2019_art_share_agg.es, title = "combined_toxic_controls_2019_art_share")
ggsave("combined_toxic_controls_2019_art_share.png", f22, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                      data = treatment_combined_hosp_2019_art_share,
)
summary(combined_intestinal_controls_2019_art_share)
combined_intestinal_controls_2019_art_share_agg.es <- aggte(combined_intestinal_controls_2019_art_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_agg.es)
f23 <- ggdid(combined_intestinal_controls_2019_art_share_agg.es, title = "combined_intestinal_controls_2019_art_share")
ggsave("combined_intestinal_controls_2019_art_share.png", f23, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_combined_hosp_2019_art_share,
)
summary(combined_pregnancy_controls_2019_art_share)
combined_pregnancy_controls_2019_art_share_agg.es <- aggte(combined_pregnancy_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_agg.es)
f24 <- ggdid(combined_pregnancy_controls_2019_art_share_agg.es, title = "combined_pregnancy_controls_2019_art_share")
ggsave("combined_pregnancy_controls_2019_art_share.png", f24, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_toxic_controls_2019_art_share)
overlap_toxic_controls_2019_art_share_agg.es <- aggte(overlap_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_agg.es)
f25 <- ggdid(overlap_toxic_controls_2019_art_share_agg.es, title = "overlap_toxic_controls_2019_art_share")
ggsave("overlap_toxic_controls_2019_art_share.png", f25, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_intestinal_controls_2019_art_share)
overlap_intestinal_controls_2019_art_share_agg.es <- aggte(overlap_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_agg.es)
f26 <- ggdid(overlap_intestinal_controls_2019_art_share_agg.es, title = "overlap_intestinal_controls_2019_art_share")
ggsave("overlap_intestinal_controls_2019_art_share.png", f26, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_pregnancy_controls_2019_art_share)
overlap_pregnancy_controls_2019_art_share_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_agg.es)
f27 <- ggdid(overlap_pregnancy_controls_2019_art_share_agg.es, title = "overlap_pregnancy_controls_2019_art_share")
ggsave("overlap_pregnancy_controls_2019_art_share.png", f27, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_capital_hosp_2019_ind,
)
summary(capital_toxic_controls_2019_ind)
capital_toxic_controls_2019_ind_agg.es <- aggte(capital_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_agg.es)
f28 <- ggdid(capital_toxic_controls_2019_ind_agg.es, title = "capital_toxic_controls_2019_ind")
ggsave("capital_toxic_controls_2019_ind.png", f28, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_capital_hosp_2019_ind,
)
summary(capital_intestinal_controls_2019_ind)
capital_intestinal_controls_2019_ind_agg.es <- aggte(capital_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_agg.es)
f29 <- ggdid(capital_intestinal_controls_2019_ind_agg.es, title = "capital_intestinal_controls_2019_ind")
ggsave("capital_intestinal_controls_2019_ind.png", f29, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_capital_hosp_2019_ind,
)
summary(capital_pregnancy_controls_2019_ind)
capital_pregnancy_controls_2019_ind_agg.es <- aggte(capital_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_agg.es)
f30 <- ggdid(capital_pregnancy_controls_2019_ind_agg.es, title = "capital_pregnancy_controls_2019_ind")
ggsave("capital_pregnancy_controls_2019_ind.png", f30, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_combined_hosp_2019_ind,
)
summary(combined_toxic_controls_2019_ind)
combined_toxic_controls_2019_ind_agg.es <- aggte(combined_toxic_controls_2019_ind, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_agg.es)
f31 <- ggdid(combined_toxic_controls_2019_ind_agg.es, title = "combined_toxic_controls_2019_ind")
ggsave("combined_toxic_controls_2019_ind.png", f31, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_combined_hosp_2019_ind,
)
summary(combined_intestinal_controls_2019_ind)
combined_intestinal_controls_2019_ind_agg.es <- aggte(combined_intestinal_controls_2019_ind, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_agg.es)
f32 <- ggdid(combined_intestinal_controls_2019_ind_agg.es, title = "combined_intestinal_controls_2019_ind")
ggsave("combined_intestinal_controls_2019_ind.png", f32, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_combined_hosp_2019_ind,
)
summary(combined_pregnancy_controls_2019_ind)
combined_pregnancy_controls_2019_ind_agg.es <- aggte(combined_pregnancy_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_agg.es)
f33 <- ggdid(combined_pregnancy_controls_2019_ind_agg.es, title = "combined_pregnancy_controls_2019_ind")
ggsave("combined_pregnancy_controls_2019_ind.png", f33, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_toxic_controls_2019_ind)
overlap_toxic_controls_2019_ind_agg.es <- aggte(overlap_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_agg.es)
f34 <- ggdid(overlap_toxic_controls_2019_ind_agg.es, title = "overlap_toxic_controls_2019_ind")
ggsave("overlap_toxic_controls_2019_ind.png", f34, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_intestinal_controls_2019_ind)
overlap_intestinal_controls_2019_ind_agg.es <- aggte(overlap_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_agg.es)
f35 <- ggdid(overlap_intestinal_controls_2019_ind_agg.es, title = "overlap_intestinal_controls_2019_ind")
ggsave("overlap_intestinal_controls_2019_ind.png", f35, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_pregnancy_controls_2019_ind)
overlap_pregnancy_controls_2019_ind_agg.es <- aggte(overlap_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_agg.es)
f36 <- ggdid(overlap_pregnancy_controls_2019_ind_agg.es, title = "overlap_pregnancy_controls_2019_ind")
ggsave("overlap_pregnancy_controls_2019_ind.png", f36, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_toxic_controls_2019_ind_share)
capital_toxic_controls_2019_ind_share_agg.es <- aggte(capital_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_agg.es)
f37 <- ggdid(capital_toxic_controls_2019_ind_share_agg.es, title = "capital_toxic_controls_2019_ind_share")
ggsave("capital_toxic_controls_2019_ind_share.png", f37, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_intestinal_controls_2019_ind_share)
capital_intestinal_controls_2019_ind_share_agg.es <- aggte(capital_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_agg.es)
f38 <-ggdid(capital_intestinal_controls_2019_ind_share_agg.es, title = "capital_intestinal_controls_2019_ind_share")
ggsave("capital_intestinal_controls_2019_ind_share.png", f38, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_pregnancy_controls_2019_ind_share)
capital_pregnancy_controls_2019_ind_share_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_agg.es)
f39 <- ggdid(capital_pregnancy_controls_2019_ind_share_agg.es, title = "capital_pregnancy_controls_2019_ind_share")
ggsave("capital_pregnancy_controls_2019_ind_share.png", f39, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_toxic_controls_2019_ind_share)
combined_toxic_controls_2019_ind_share_agg.es <- aggte(combined_toxic_controls_2019_ind_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_agg.es)
f40 <- ggdid(combined_toxic_controls_2019_ind_share_agg.es, title = "combined_toxic_controls_2019_ind_share")
ggsave("combined_toxic_controls_2019_ind_share.png", f40, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                      data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_intestinal_controls_2019_ind_share)
combined_intestinal_controls_2019_ind_share_agg.es <- aggte(combined_intestinal_controls_2019_ind_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_agg.es)
f41 <- ggdid(combined_intestinal_controls_2019_ind_share_agg.es, title = "combined_intestinal_controls_2019_ind_share")
ggsave("combined_intestinal_controls_2019_ind_share.png", f41, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_pregnancy_controls_2019_ind_share)
combined_pregnancy_controls_2019_ind_share_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_agg.es)
f42 <- ggdid(combined_pregnancy_controls_2019_ind_share_agg.es, title = "combined_pregnancy_controls_2019_ind_share")
ggsave("combined_pregnancy_controls_2019_ind_share.png", f42, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_toxic_controls_2019_ind_share)
overlap_toxic_controls_2019_ind_share_agg.es <- aggte(overlap_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_agg.es)
f43 <- ggdid(overlap_toxic_controls_2019_ind_share_agg.es, title = "overlap_toxic_controls_2019_ind_share")
ggsave("overlap_toxic_controls_2019_ind_share.png", f43, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_intestinal_controls_2019_ind_share)
overlap_intestinal_controls_2019_ind_share_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_agg.es)
f44 <- ggdid(overlap_intestinal_controls_2019_ind_share_agg.es, title = "overlap_intestinal_controls_2019_ind_share")
ggsave("overlap_intestinal_controls_2019_ind_share.png", f44, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_pregnancy_controls_2019_ind_share)
overlap_pregnancy_controls_2019_ind_share_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_agg.es)
f45 <- ggdid(overlap_pregnancy_controls_2019_ind_share_agg.es, title = "overlap_pregnancy_controls_2019_ind_share")
ggsave("overlap_pregnancy_controls_2019_ind_share.png", f45, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                             data = treatment_capital_hosp_2019_amazon,
)
summary(capital_toxic_controls_2019_amazon)
capital_toxic_controls_2019_amazon_agg.es <- aggte(capital_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_agg.es)
f46 <- ggdid(capital_toxic_controls_2019_amazon_agg.es, title = "capital_toxic_controls_2019_amazon")
ggsave("capital_toxic_controls_2019_amazon.png", f46, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                  data = treatment_capital_hosp_2019_amazon,
)
summary(capital_intestinal_controls_2019_amazon)
capital_intestinal_controls_2019_amazon_agg.es <- aggte(capital_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_agg.es)
f47 <- ggdid(capital_intestinal_controls_2019_amazon_agg.es, title = "capital_intestinal_controls_2019_amazon")
ggsave("capital_intestinal_controls_2019_amazon.png", f47, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_capital_hosp_2019_amazon,
)
summary(capital_pregnancy_controls_2019_amazon)
capital_pregnancy_controls_2019_amazon_agg.es <- aggte(capital_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_agg.es)
f48 <- ggdid(capital_pregnancy_controls_2019_amazon_agg.es , title = "capital_pregnancy_controls_2019_amazon")
ggsave("capital_pregnancy_controls_2019_amazon.png", f48, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_combined_hosp_2019_amazon,
)
summary(combined_toxic_controls_2019_amazon)
combined_toxic_controls_2019_amazon_agg.es <- aggte(combined_toxic_controls_2019_amazon, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_agg.es)
f49 <- ggdid(combined_toxic_controls_2019_amazon_agg.es, title = "combined_toxic_controls_2019_amazon")
ggsave("combined_toxic_controls_2019_amazon.png", f49, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_combined_hosp_2019_amazon,
)
summary(combined_intestinal_controls_2019_amazon)
combined_intestinal_controls_2019_amazon_agg.es <- aggte(combined_intestinal_controls_2019_amazon, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_agg.es)
f50 <- ggdid(combined_intestinal_controls_2019_amazon_agg.es, title = "combined_intestinal_controls_2019_amazon")
ggsave("combined_intestinal_controls_2019_amazon.png", f50, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                  data = treatment_combined_hosp_2019_amazon,
)
summary(combined_pregnancy_controls_2019_amazon)
combined_pregnancy_controls_2019_amazon_agg.es <- aggte(combined_pregnancy_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_agg.es)
f51 <- ggdid(combined_pregnancy_controls_2019_amazon_agg.es, title = "combined_pregnancy_controls_2019_amazon")
ggsave("combined_pregnancy_controls_2019_amazon.png", f51, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                             data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_toxic_controls_2019_amazon)
overlap_toxic_controls_2019_amazon_agg.es <- aggte(overlap_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_agg.es)
f52 <- ggdid(overlap_toxic_controls_2019_amazon_agg.es, title = "overlap_toxic_controls_2019_amazon")
ggsave("overlap_toxic_controls_2019_amazon.png", f52, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                  data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_intestinal_controls_2019_amazon)
overlap_intestinal_controls_2019_amazon_agg.es <- aggte(overlap_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_agg.es)
f53 <- ggdid(overlap_intestinal_controls_2019_amazon_agg.es, title = "overlap_intestinal_controls_2019_amazon")
ggsave("overlap_intestinal_controls_2019_amazon.png", f53, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_pregnancy_controls_2019_amazon)
overlap_pregnancy_controls_2019_amazon_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_agg.es)
f54 <- ggdid(overlap_pregnancy_controls_2019_amazon_agg.es, title = "overlap_pregnancy_controls_2019_amazon")
ggsave("overlap_pregnancy_controls_2019_amazon.png", f54, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_capital_child_mort_2019,
)
summary(capital_child_0_2_controls_2019)
capital_child_0_2_controls_2019_agg.es <- aggte(capital_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_agg.es)
f55 <- ggdid(capital_child_0_2_controls_2019_agg.es, title = "capital_child_0_2_controls_2019")
ggsave("capital_child_0_2_controls_2019.png", f55, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                           data = treatment_combined_child_mort_2019,
)
summary(combined_child_0_2_controls_2019)
combined_child_0_2_controls_2019_agg.es <- aggte(combined_child_0_2_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_agg.es)
f56 <- ggdid(combined_child_0_2_controls_2019_agg.es, title = "combined_child_0_2_controls_2019")
ggsave("combined_child_0_2_controls_2019.png", f56, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                          data = treatment_overlap_child_mort_2019,
)
summary(overlap_child_0_2_controls_2019)
overlap_child_0_2_controls_2019_agg.es <- aggte(overlap_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_agg.es)
f57 <- ggdid(overlap_child_0_2_controls_2019_agg.es, title = "overlap_child_0_2_controls_2019")
ggsave("overlap_child_0_2_controls_2019.png", f57, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_capital_child_mort_2019_art,
)
summary(capital_child_0_2_controls_2019_art)
capital_child_0_2_controls_2019_art_agg.es <- aggte(capital_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_agg.es)
f58 <- ggdid(capital_child_0_2_controls_2019_art_agg.es, title = "capital_child_0_2_controls_2019_art")
ggsave("capital_child_0_2_controls_2019_art.png", f58, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_combined_child_mort_2019_art,
)
summary(combined_child_0_2_controls_2019_art)
combined_child_0_2_controls_2019_art_agg.es <- aggte(combined_child_0_2_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_agg.es)
f59 <- ggdid(combined_child_0_2_controls_2019_art_agg.es, title = "combined_child_0_2_controls_2019_art")
ggsave("combined_child_0_2_controls_2019_art.png", f59, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_overlap_child_mort_2019_art,
)
summary(overlap_child_0_2_controls_2019_art)
overlap_child_0_2_controls_2019_art_agg.es <- aggte(overlap_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_agg.es)
f60 <- ggdid(overlap_child_0_2_controls_2019_art_agg.es, title = "overlap_child_0_2_controls_2019_art")
ggsave("overlap_child_0_2_controls_2019_art.png", f60, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_capital_child_mort_2019_art_share,
)
summary(capital_child_0_2_controls_2019_art_share)
capital_child_0_2_controls_2019_art_share_agg.es <- aggte(capital_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_agg.es)
f61 <- ggdid(capital_child_0_2_controls_2019_art_share_agg.es, title = "capital_child_0_2_controls_2019_art_share")
ggsave("capital_child_0_2_controls_2019_art_share.png", f61, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_combined_child_mort_2019_art_share,
)
summary(combined_child_0_2_controls_2019_art_share)
combined_child_0_2_controls_2019_art_share_agg.es <- aggte(combined_child_0_2_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_agg.es)
f62 <- ggdid(combined_child_0_2_controls_2019_art_share_agg.es, title = "combined_child_0_2_controls_2019_art_share")
ggsave("combined_child_0_2_controls_2019_art_share.png", f62, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_overlap_child_mort_2019_art_share,
)
summary(overlap_child_0_2_controls_2019_art_share)
overlap_child_0_2_controls_2019_art_share_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_agg.es)
f63 <- ggdid(overlap_child_0_2_controls_2019_art_share_agg.es, title = "overlap_child_0_2_controls_2019_art_share")
ggsave("overlap_child_0_2_controls_2019_art_share.png", f63, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_capital_child_mort_2019_ind,
)
summary(capital_child_0_2_controls_2019_ind)
capital_child_0_2_controls_2019_ind_agg.es <- aggte(capital_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_agg.es)
f64 <- ggdid(capital_child_0_2_controls_2019_ind_agg.es, title = "capital_child_0_2_controls_2019_ind")
ggsave("capital_child_0_2_controls_2019_ind.png", f64, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_combined_child_mort_2019_ind,
)
summary(combined_child_0_2_controls_2019_ind)
combined_child_0_2_controls_2019_ind_agg.es <- aggte(combined_child_0_2_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_agg.es)
f65 <- ggdid(combined_child_0_2_controls_2019_ind_agg.es, title = "combined_child_0_2_controls_2019_ind")
ggsave("combined_child_0_2_controls_2019_ind.png", f65, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                              data = treatment_overlap_child_mort_2019_ind,
)
summary(overlap_child_0_2_controls_2019_ind)
overlap_child_0_2_controls_2019_ind_agg.es <- aggte(overlap_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_agg.es)
f66 <- ggdid(overlap_child_0_2_controls_2019_ind_agg.es, title = "overlap_child_0_2_controls_2019_ind")
ggsave("overlap_child_0_2_controls_2019_ind.png", f66, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_capital_child_mort_2019_ind_share,
)
summary(capital_child_0_2_controls_2019_ind_share)
capital_child_0_2_controls_2019_ind_share_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_agg.es)
f67 <- ggdid(capital_child_0_2_controls_2019_ind_share_agg.es, title = "capital_child_0_2_controls_2019_ind_share")
ggsave("capital_child_0_2_controls_2019_ind_share.png", f67, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_combined_child_mort_2019_ind_share,
)
summary(combined_child_0_2_controls_2019_ind_share)
combined_child_0_2_controls_2019_ind_share_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_agg.es)
f68 <- ggdid(combined_child_0_2_controls_2019_ind_share_agg.es, title = "combined_child_0_2_controls_2019_ind_share")
ggsave("combined_child_0_2_controls_2019_ind_share.png", f68, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_overlap_child_mort_2019_ind_share,
)
summary(overlap_child_0_2_controls_2019_ind_share)
overlap_child_0_2_controls_2019_ind_share_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_agg.es)
f69 <- ggdid(overlap_child_0_2_controls_2019_ind_share_agg.es, title = "overlap_child_0_2_controls_2019_ind_share")
ggsave("overlap_child_0_2_controls_2019_ind_share.png", f69, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_capital_child_mort_2019_amazon,
)
summary(capital_child_0_2_controls_2019_amazon)
capital_child_0_2_controls_2019_amazon_agg.es <- aggte(capital_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_agg.es)
f70 <- ggdid(capital_child_0_2_controls_2019_amazon_agg.es, title = "capital_child_0_2_controls_2019_amazon")
ggsave("capital_child_0_2_controls_2019_amazon.png", f70, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                  data = treatment_combined_child_mort_2019_amazon,
)
summary(combined_child_0_2_controls_2019_amazon)
combined_child_0_2_controls_2019_amazon_agg.es <- aggte(combined_child_0_2_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_agg.es)
f71 <- ggdid(combined_child_0_2_controls_2019_amazon_agg.es, title = "combined_child_0_2_controls_2019_amazon")
ggsave("combined_child_0_2_controls_2019_amazon.png", f71, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                 data = treatment_overlap_child_mort_2019_amazon,
)
summary(overlap_child_0_2_controls_2019_amazon)
overlap_child_0_2_controls_2019_amazon_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_agg.es)
f72 <- ggdid(overlap_child_0_2_controls_2019_amazon_agg.es, title = "overlap_child_0_2_controls_2019_amazon")
ggsave("overlap_child_0_2_controls_2019_amazon.png", f72, width = 8, height = 5, dpi = 300)


### with weights
setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_1998_weights/")
#toxic effects
capital_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_capital_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019_weighted)
capital_toxic_controls_2019_weighted_agg.es <- aggte(capital_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_toxic_controls_2019_weighted_agg.es)
f1a <- ggdid(capital_toxic_controls_2019_weighted_agg.es, title = "capital_toxic_controls_2019_weighted")
ggsave("capital_toxic_controls_2019_weighted.png", f1a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                data = treatment_combined_hosp_2019,
                                                weightsname = "pop"
                                                #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019_weighted)
combined_toxic_controls_2019_weighted_agg.es <- aggte(combined_toxic_controls_2019_weighted, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_toxic_controls_2019_weighted_agg.es)
f2a <- ggdid(combined_toxic_controls_2019_weighted_agg.es, title = "combined_toxic_controls_2019_weighted")
ggsave("combined_toxic_controls_2019_weighted.png", f2a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                               data = treatment_overlap_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019_weighted)
overlap_toxic_controls_2019_weighted_agg.es <- aggte(overlap_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_toxic_controls_2019_weighted_agg.es)
f3a <- ggdid(overlap_toxic_controls_2019_weighted_agg.es, title = "overlap_toxic_controls_2019_weighted")
ggsave("overlap_toxic_controls_2019_weighted.png", f3a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_capital_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_weighted)
capital_toxic_controls_2019_art_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_art_weighted_agg.es)
f4a <- ggdid(capital_toxic_controls_2019_art_weighted_agg.es, title = "capital_toxic_controls_2019_art_weighted")
ggsave("capital_toxic_controls_2019_art_weighted.png", f4a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_combined_hosp_2019_art,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_weighted)
combined_toxic_controls_2019_art_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_art_weighted_agg.es)
f5a <- ggdid(combined_toxic_controls_2019_art_weighted_agg.es, title = "combined_toxic_controls_2019_art_weighted")
ggsave("combined_toxic_controls_2019_art_weighted.png", f5a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_overlap_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_weighted)
overlap_toxic_controls_2019_art_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_weighted_agg.es)
f6a <- ggdid(overlap_toxic_controls_2019_art_weighted_agg.es, title = "overlap_toxic_controls_2019_art_weighted")
ggsave("overlap_toxic_controls_2019_art_weighted.png", f6a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_capital_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_share_weighted)
capital_toxic_controls_2019_art_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_weighted_agg.es)
f7a <- ggdid(capital_toxic_controls_2019_art_share_weighted_agg.es, title = "capital_toxic_controls_2019_art_share_weighted")
ggsave("capital_toxic_controls_2019_art_share_weighted.png", f7a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_combined_hosp_2019_art_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_share_weighted)
combined_toxic_controls_2019_art_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_weighted_agg.es)
f8a <- ggdid(combined_toxic_controls_2019_art_share_weighted_agg.es, title = "combined_toxic_controls_2019_art_share_weighted")
ggsave("combined_toxic_controls_2019_art_share_weighted.png", f8a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_overlap_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_share_weighted)
overlap_toxic_controls_2019_art_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_weighted_agg.es)
f9a <- ggdid(overlap_toxic_controls_2019_art_share_weighted_agg.es, title = "overlap_toxic_controls_2019_art_share_weighted")
ggsave("overlap_toxic_controls_2019_art_share_weighted.png", f9a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_capital_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_weighted)
capital_toxic_controls_2019_ind_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_weighted_agg.es)
f10a <-ggdid(capital_toxic_controls_2019_ind_weighted_agg.es, title = "capital_toxic_controls_2019_ind_weighted")
ggsave("capital_toxic_controls_2019_ind_weighted.png", f10a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_combined_hosp_2019_ind,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_weighted)
combined_toxic_controls_2019_ind_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_weighted_agg.es)
f11a <-ggdid(combined_toxic_controls_2019_ind_weighted_agg.es, title = "combined_toxic_controls_2019_ind_weighted")
ggsave("combined_toxic_controls_2019_ind_weighted.png", f11a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_overlap_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_weighted)
overlap_toxic_controls_2019_ind_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_weighted_agg.es)
f12a <- ggdid(overlap_toxic_controls_2019_ind_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_weighted")
ggsave("overlap_toxic_controls_2019_ind_weighted.png", f12a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_capital_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_share_weighted)
capital_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_weighted_agg.es)
f13a <- ggdid(capital_toxic_controls_2019_ind_share_weighted_agg.es, title = "capital_toxic_controls_2019_ind_share_weighted")
ggsave("capital_toxic_controls_2019_ind_share_weighted.png", f13a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_combined_hosp_2019_ind_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_share_weighted)
combined_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_weighted_agg.es)
f14a <- ggdid(combined_toxic_controls_2019_ind_share_weighted_agg.es, title = "combined_toxic_controls_2019_ind_share_weighted")
ggsave("combined_toxic_controls_2019_ind_share_weighted.png", f14a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_overlap_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_share_weighted)
overlap_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_weighted_agg.es)
f15a <- ggdid(overlap_toxic_controls_2019_ind_share_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_share_weighted")
ggsave("overlap_toxic_controls_2019_ind_share_weighted.png", f15a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                      data = treatment_capital_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(capital_toxic_controls_2019_amazon_weighted)
capital_toxic_controls_2019_amazon_weighted_agg.es <- aggte(capital_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_weighted_agg.es)
f16a <- ggdid(capital_toxic_controls_2019_amazon_weighted_agg.es, title = "capital_toxic_controls_2019_amazon_weighted")
ggsave("capital_toxic_controls_2019_amazon_weighted.png", f16a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_combined_hosp_2019_amazon,
                                                       weightsname = "pop"
)
summary(combined_toxic_controls_2019_amazon_weighted)
combined_toxic_controls_2019_amazon_weighted_agg.es <- aggte(combined_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_weighted_agg.es)
f17a <- ggdid(combined_toxic_controls_2019_amazon_weighted_agg.es, title = "combined_toxic_controls_2019_amazon_weighted")
ggsave("combined_toxic_controls_2019_amazon_weighted.png", f17a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                      data = treatment_overlap_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(overlap_toxic_controls_2019_amazon_weighted)
overlap_toxic_controls_2019_amazon_weighted_agg.es <- aggte(overlap_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_weighted_agg.es)
f18a <- ggdid(overlap_toxic_controls_2019_amazon_weighted_agg.es, title = "overlap_toxic_controls_2019_amazon_weighted")
ggsave("overlap_toxic_controls_2019_amazon_weighted.png", f18a, width = 8, height = 5, dpi = 300)

#intestinal
capital_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_capital_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019_weighted)
capital_intestinal_controls_2019_weighted_agg.es <- aggte(capital_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_intestinal_controls_2019_weighted_agg.es)
f19a <- ggdid(capital_intestinal_controls_2019_weighted_agg.es, title = "capital_intestinal_controls_2019_weighted")
ggsave("capital_intestinal_controls_2019_weighted.png", f19a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                     data = treatment_combined_hosp_2019,
                                                     weightsname = "pop"
                                                     #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019_weighted)
combined_intestinal_controls_2019_weighted_agg.es <- aggte(combined_intestinal_controls_2019_weighted, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_intestinal_controls_2019_weighted_agg.es)
f20a <- ggdid(combined_intestinal_controls_2019_weighted_agg.es, title = "combined_intestinal_controls_2019_weighted")
ggsave("combined_intestinal_controls_2019_weighted.png", f20a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_overlap_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019_weighted)
overlap_intestinal_controls_2019_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_intestinal_controls_2019_weighted_agg.es)
f21a <- ggdid(overlap_intestinal_controls_2019_weighted_agg.es, title = "overlap_intestinal_controls_2019_weighted")
ggsave("overlap_intestinal_controls_2019_weighted.png", f21a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_capital_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_weighted)
capital_intestinal_controls_2019_art_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_weighted_agg.es)
f22a <- ggdid(capital_intestinal_controls_2019_art_weighted_agg.es, title = "capital_intestinal_controls_2019_art_weighted")
ggsave("capital_intestinal_controls_2019_art_weighted.png", f22a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_combined_hosp_2019_art,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_weighted)
combined_intestinal_controls_2019_art_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_weighted_agg.es)
f23a <- ggdid(combined_intestinal_controls_2019_art_weighted_agg.es, title = "combined_intestinal_controls_2019_art_weighted")
ggsave("combined_intestinal_controls_2019_art_weighted.png", f23a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_overlap_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_weighted)
overlap_intestinal_controls_2019_art_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_weighted_agg.es)
f24a <- ggdid(overlap_intestinal_controls_2019_art_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_weighted")
ggsave("overlap_intestinal_controls_2019_art_weighted.png", f24a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_capital_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_share_weighted)
capital_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_weighted_agg.es)
f25a <- ggdid(capital_intestinal_controls_2019_art_share_weighted_agg.es, title = "capital_intestinal_controls_2019_art_share_weighted")
ggsave("capital_intestinal_controls_2019_art_share_weighted.png", f25a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                               data = treatment_combined_hosp_2019_art_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_share_weighted)
combined_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_weighted_agg.es)
f26a <- ggdid(combined_intestinal_controls_2019_art_share_weighted_agg.es, title = "combined_intestinal_controls_2019_art_share_weighted")
ggsave("combined_intestinal_controls_2019_art_share_weighted.png", f26a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_overlap_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_share_weighted)
overlap_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_weighted_agg.es)
f27a <- ggdid(overlap_intestinal_controls_2019_art_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_share_weighted")
ggsave("overlap_intestinal_controls_2019_art_share_weighted.png", f27a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_capital_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_weighted)
capital_intestinal_controls_2019_ind_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_weighted_agg.es)
f28a <- ggdid(capital_intestinal_controls_2019_ind_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_weighted")
ggsave("capital_intestinal_controls_2019_ind_weighted.png", f28a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                         data = treatment_combined_hosp_2019_ind,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_weighted)
combined_intestinal_controls_2019_ind_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_weighted_agg.es)
f29a <- ggdid(combined_intestinal_controls_2019_ind_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_weighted")
ggsave("combined_intestinal_controls_2019_ind_weighted.png", f29a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_overlap_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_weighted)
overlap_intestinal_controls_2019_ind_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_weighted_agg.es)
f30a <- ggdid(overlap_intestinal_controls_2019_ind_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_weighted")
ggsave("overlap_intestinal_controls_2019_ind_weighted.png", f30a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_capital_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_share_weighted)
capital_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_weighted_agg.es)
f31a <- ggdid(capital_intestinal_controls_2019_ind_share_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_share_weighted")
ggsave("capital_intestinal_controls_2019_ind_share_weighted.png", f31a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                               data = treatment_combined_hosp_2019_ind_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_share_weighted)
combined_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_weighted_agg.es)
f32a <- ggdid(combined_intestinal_controls_2019_ind_share_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_share_weighted")
ggsave("combined_intestinal_controls_2019_ind_share_weighted.png", f32a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_overlap_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_share_weighted)
overlap_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_weighted_agg.es)
f33a <- ggdid(overlap_intestinal_controls_2019_ind_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_share_weighted")
ggsave("overlap_intestinal_controls_2019_ind_share_weighted.png", f33a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                           data = treatment_capital_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(capital_intestinal_controls_2019_amazon_weighted)
capital_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(capital_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_weighted_agg.es)
f34a <- ggdid(capital_intestinal_controls_2019_amazon_weighted_agg.es, title = "capital_intestinal_controls_2019_amazon_weighted")
ggsave("capital_intestinal_controls_2019_amazon_weighted.png", f34a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                            tname = "year",
                                                            idname = "muni_id",
                                                            gname = "G",
                                                            xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                            data = treatment_combined_hosp_2019_amazon,
                                                            weightsname = "pop"
)
summary(combined_intestinal_controls_2019_amazon_weighted)
combined_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(combined_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                  min_e = -10, max_e = 10,
                                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_weighted_agg.es)
f35a <- ggdid(combined_intestinal_controls_2019_amazon_weighted_agg.es, title = "combined_intestinal_controls_2019_amazon_weighted")
ggsave("combined_intestinal_controls_2019_amazon_weighted.png", f35a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                           data = treatment_overlap_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_amazon_weighted)
overlap_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_weighted_agg.es)
f36a <- ggdid(overlap_intestinal_controls_2019_amazon_weighted_agg.es, title = "overlap_intestinal_controls_2019_amazon_weighted")
ggsave("overlap_intestinal_controls_2019_amazon_weighted.png", f36a, width = 8, height = 5, dpi = 300)

#pregnancy
capital_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_capital_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019_weighted)
capital_pregnancy_controls_2019_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_pregnancy_controls_2019_weighted_agg.es)
f37a <- ggdid(capital_pregnancy_controls_2019_weighted_agg.es, title = "capital_pregnancy_controls_2019_weighted")
ggsave("capital_pregnancy_controls_2019_weighted.png", f37a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_combined_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019_weighted)
combined_pregnancy_controls_2019_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_pregnancy_controls_2019_weighted_agg.es)
f38a <- ggdid(combined_pregnancy_controls_2019_weighted_agg.es, title = "combined_pregnancy_controls_2019_weighted")
ggsave("combined_pregnancy_controls_2019_weighted.png", f38a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_overlap_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019_weighted)
overlap_pregnancy_controls_2019_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_weighted_agg.es)
f39a <- ggdid(overlap_pregnancy_controls_2019_weighted_agg.es, title = "overlap_pregnancy_controls_2019_weighted")
ggsave("overlap_pregnancy_controls_2019_weighted.png", f39a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_capital_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_weighted)
capital_pregnancy_controls_2019_art_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_weighted_agg.es)
f40a <- ggdid(capital_pregnancy_controls_2019_art_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_weighted")
ggsave("capital_pregnancy_controls_2019_art_weighted.png", f40a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_combined_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_weighted)
combined_pregnancy_controls_2019_art_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_weighted_agg.es)
f41a <- ggdid(combined_pregnancy_controls_2019_art_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_weighted")
ggsave("combined_pregnancy_controls_2019_art_weighted.png", f41a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_overlap_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_weighted)
overlap_pregnancy_controls_2019_art_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_weighted_agg.es)
f42a <- ggdid(overlap_pregnancy_controls_2019_art_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_weighted")
ggsave("overlap_pregnancy_controls_2019_art_weighted.png", f42a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_capital_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_share_weighted)
capital_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_weighted_agg.es)
f43a <- ggdid(capital_pregnancy_controls_2019_art_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_share_weighted")
ggsave("capital_pregnancy_controls_2019_art_share_weighted.png", f43a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_combined_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_share_weighted)
combined_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_weighted_agg.es)
f44a <- ggdid(combined_pregnancy_controls_2019_art_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_share_weighted")
ggsave("combined_pregnancy_controls_2019_art_share_weighted.png", f44a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_overlap_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_share_weighted)
overlap_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_weighted_agg.es)
f45a <- ggdid(overlap_pregnancy_controls_2019_art_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_share_weighted")
ggsave("overlap_pregnancy_controls_2019_art_share_weighted.png", f45a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_capital_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_weighted)
capital_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_weighted_agg.es)
f46a <- ggdid(capital_pregnancy_controls_2019_ind_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_weighted")
ggsave("capital_pregnancy_controls_2019_ind_weighted.png", f46a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_combined_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_weighted)
combined_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_weighted_agg.es)
f47a <- ggdid(combined_pregnancy_controls_2019_ind_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_weighted")
ggsave("combined_pregnancy_controls_2019_ind_weighted.png", f47a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_overlap_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_weighted)
overlap_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_weighted_agg.es)
f48a <- ggdid(overlap_pregnancy_controls_2019_ind_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_weighted.png", f48a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_capital_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_share_weighted)
capital_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_weighted_agg.es)
f49a <- ggdid(capital_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_share_weighted")
ggsave("capital_pregnancy_controls_2019_ind_share_weighted.png", f49a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_combined_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_share_weighted)
combined_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_weighted_agg.es)
f50a <- ggdid(combined_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_share_weighted")
ggsave("combined_pregnancy_controls_2019_ind_share_weighted.png", f50a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_overlap_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_share_weighted)
overlap_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es)
f51a <- ggdid(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_share_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_share_weighted.png", f51a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_capital_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_amazon_weighted)
capital_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_weighted_agg.es)
f52a <- ggdid(capital_pregnancy_controls_2019_amazon_weighted_agg.es, title = "capital_pregnancy_controls_2019_amazon_weighted")
ggsave("capital_pregnancy_controls_2019_amazon_weighted.png", f52a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                           data = treatment_combined_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_amazon_weighted)
combined_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_weighted_agg.es)
f53a <- ggdid(combined_pregnancy_controls_2019_amazon_weighted_agg.es, title = "combined_pregnancy_controls_2019_amazon_weighted")
ggsave("combined_pregnancy_controls_2019_amazon_weighted.png", f53a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_overlap_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_amazon_weighted)
overlap_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_weighted_agg.es)
f54a <- ggdid(overlap_pregnancy_controls_2019_amazon_weighted_agg.es, title = "overlap_pregnancy_controls_2019_amazon_weighted")
ggsave("overlap_pregnancy_controls_2019_amazon_weighted.png", f54a, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_capital_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_child_0_2_controls_2019_weighted)
capital_child_0_2_controls_2019_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_child_0_2_controls_2019_weighted_agg.es)
f55a <- ggdid(capital_child_0_2_controls_2019_weighted_agg.es, title = "capital_child_0_2_controls_2019_weighted")
ggsave("capital_child_0_2_controls_2019_weighted.png", f55a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                    data = treatment_combined_child_mort_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_child_0_2_controls_2019_weighted)
combined_child_0_2_controls_2019_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_child_0_2_controls_2019_weighted_agg.es)
f56a <- ggdid(combined_child_0_2_controls_2019_weighted_agg.es, title = "combined_child_0_2_controls_2019_weighted")
ggsave("combined_child_0_2_controls_2019_weighted.png", f56a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                   data = treatment_overlap_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_child_0_2_controls_2019_weighted)
overlap_child_0_2_controls_2019_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_weighted_agg.es)
f57a <- ggdid(overlap_child_0_2_controls_2019_weighted_agg.es, title = "overlap_child_0_2_controls_2019_weighted")
ggsave("overlap_child_0_2_controls_2019_weighted.png", f57a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_capital_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_weighted)
capital_child_0_2_controls_2019_art_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_weighted_agg.es)
f58a <- ggdid(capital_child_0_2_controls_2019_art_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_weighted")
ggsave("capital_child_0_2_controls_2019_art_weighted.png", f58a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_combined_child_mort_2019_art,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_weighted)
combined_child_0_2_controls_2019_art_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_weighted_agg.es)
f59a <- ggdid(combined_child_0_2_controls_2019_art_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_weighted")
ggsave("combined_child_0_2_controls_2019_art_weighted.png", f59a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_overlap_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_weighted)
overlap_child_0_2_controls_2019_art_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_weighted_agg.es)
f60a <- ggdid(overlap_child_0_2_controls_2019_art_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_weighted")
ggsave("overlap_child_0_2_controls_2019_art_weighted.png", f60a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_capital_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_share_weighted)
capital_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_weighted_agg.es)
f61a <- ggdid(capital_child_0_2_controls_2019_art_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_share_weighted")
ggsave("capital_child_0_2_controls_2019_art_share_weighted.png", f61a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_combined_child_mort_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_share_weighted)
combined_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_weighted_agg.es)
f62a <- ggdid(combined_child_0_2_controls_2019_art_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_share_weighted")
ggsave("combined_child_0_2_controls_2019_art_share_weighted.png", f62a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_overlap_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_share_weighted)
overlap_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_weighted_agg.es)
f63a <- ggdid(overlap_child_0_2_controls_2019_art_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_share_weighted")
ggsave("overlap_child_0_2_controls_2019_art_share_weighted.png", f63a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_capital_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_weighted)
capital_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_weighted_agg.es)
f64a <- ggdid(capital_child_0_2_controls_2019_ind_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_weighted")
ggsave("capital_child_0_2_controls_2019_ind_weighted.png", f64a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                        data = treatment_combined_child_mort_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_weighted)
combined_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_weighted_agg.es)
f65a <- ggdid(combined_child_0_2_controls_2019_ind_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_weighted")
ggsave("combined_child_0_2_controls_2019_ind_weighted.png", f65a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                       data = treatment_overlap_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_weighted)
overlap_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_weighted_agg.es)
f66a <- ggdid(overlap_child_0_2_controls_2019_ind_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_weighted.png", f66a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_capital_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_share_weighted)
capital_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_weighted_agg.es)
f67a <- ggdid(capital_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_share_weighted")
ggsave("capital_child_0_2_controls_2019_ind_share_weighted.png", f67a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                              data = treatment_combined_child_mort_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_share_weighted)
combined_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_weighted_agg.es)
f68a <- ggdid(combined_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_share_weighted")
ggsave("combined_child_0_2_controls_2019_ind_share_weighted.png", f68a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                             data = treatment_overlap_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_share_weighted)
overlap_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es)
f69a <- ggdid(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_share_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_share_weighted.png", f69a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_capital_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_amazon_weighted)
capital_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_weighted_agg.es)
f70a <- ggdid(capital_child_0_2_controls_2019_amazon_weighted_agg.es, title = "capital_child_0_2_controls_2019_amazon_weighted")
ggsave("capital_child_0_2_controls_2019_amazon_weighted.png", f70a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                           data = treatment_combined_child_mort_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_amazon_weighted)
combined_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_weighted_agg.es)
f71a <- ggdid(combined_child_0_2_controls_2019_amazon_weighted_agg.es, title = "combined_child_0_2_controls_2019_amazon_weighted")
ggsave("combined_child_0_2_controls_2019_amazon_weighted.png", f71a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_1991 + illiteracy_1991 + share_rural_1991 + share_electricity_1991,
                                                          data = treatment_overlap_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_amazon_weighted)
overlap_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_weighted_agg.es)
f72a <- ggdid(overlap_child_0_2_controls_2019_amazon_weighted_agg.es, title = "overlap_child_0_2_controls_2019_amazon_weighted")
ggsave("overlap_child_0_2_controls_2019_amazon_weighted.png", f72a, width = 8, height = 5, dpi = 300)

#2001
rm(list = ls())
library(readr)
library(sf)
library(did)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyselect)

# only with control group
# here G=0 if upstream, G=NA if never part of a basin chian, G=min_year if either mine or downstream
process_treatment_data <- function(path) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  # 1. Compute global median cutoff (mine basin–year level)
  mining_median_mine <- data_raw %>%
    filter(status == "mine") %>%
    group_by(mine_basin, year) %>%
    summarise(
      mining_ha_total = first(mining_ha_total),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining_ha_total, na.rm = TRUE)
    ) %>%
    pull(median)
  
  # 2. Apply cutoff and construct G
  data_processed <- data_raw %>% 
    group_by(muni_id) %>% 
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>% 
    ungroup() %>% 
    
    mutate(
      mining_median_mine = mining_median_mine,
      crossed = mining_ha_total > mining_median_mine
    ) %>%
    
    group_by(muni_id) %>% 
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>% 
    ungroup() %>% 
    filter(!is.na(G))
  
  return(data_processed)
}
treatment_capital_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg"
)

treatment_combined_child_mort <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg"
)

treatment_overlap_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg"
)

treatment_capital_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg"
)

treatment_combined_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg"
)

treatment_overlap_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg"
)

compute_median <- function(data, mining_var) {
  data %>%
    filter(status == "mine") %>%
    st_set_geometry(NULL) %>%  # drop geometry before summarising
    group_by(mine_basin, year) %>%
    summarise(
      mining = first(.data[[mining_var]]),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining, na.rm = TRUE)
    ) %>%
    pull(median)
}

build_treatment <- function(
    path,
    mining_var,
    year_min = 2001,
    year_max = 2019,
    mine_type_filter = NULL,
    share_var = NULL,
    share_cutoff = NULL
) {
  
  # Read data and drop geometry immediately
  data_raw <- st_read(path) %>%
    # st_set_geometry(NULL) %>%   <--- drop geometry
    filter(between(year, year_min, year_max))
  
  # Optional filters
  if (!is.null(mine_type_filter)) {
    data_raw <- data_raw %>% filter(mine_type == mine_type_filter)
  }
  
  if (!is.null(share_var)) {
    data_raw <- data_raw %>%
      group_by(muni_id) %>%
      filter(any(.data[[share_var]] > share_cutoff)) %>%
      ungroup()
  }
  
  # Compute cutoff ONCE
  median <- compute_median(data_raw, mining_var)
  
  # Apply treatment logic
  data_raw %>%
    group_by(muni_id) %>%
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      crossed = .data[[mining_var]] > median
    ) %>%
    group_by(muni_id) %>%
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(G))
}
# artisanal only
treatment_capital_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_capital_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

#artisanal share >0.5
treatment_capital_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_capital_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

#industrial only
treatment_capital_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_capital_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)

#industrial share >0.5
treatment_capital_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_capital_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)


### from 2002-2019 with controls
treatment_capital_child_mort_2019 <- treatment_capital_child_mort %>%
  filter(between(year, 2001, 2019))
treatment_combined_child_mort_2019 <- treatment_combined_child_mort %>%
  filter(between(year, 2001, 2019))
treatment_overlap_child_mort_2019 <- treatment_overlap_child_mort %>%
  filter(between(year, 2001, 2019))
treatment_capital_hosp_2019 <- treatment_capital_hosp %>%
  filter(between(year, 2001, 2019))
treatment_combined_hosp_2019 <- treatment_combined_hosp %>%
  filter(between(year, 2001, 2019))
treatment_overlap_hosp_2019 <- treatment_overlap_hosp %>%
  filter(between(year, 2001, 2019))

### from 2002-2019 with controls only legal amazon
treatment_capital_child_mort_2019_amazon <- treatment_capital_child_mort %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_child_mort_2019_amazon <- treatment_combined_child_mort %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_child_mort_2019_amazon <- treatment_overlap_child_mort %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)
treatment_capital_hosp_2019_amazon <- treatment_capital_hosp %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_hosp_2019_amazon <- treatment_combined_hosp %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_hosp_2019_amazon <- treatment_overlap_hosp %>%
  filter(between(year, 2001, 2019)) %>%
  filter(legal_amazon == 1)


setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_2001/")


#capital
capital_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                      data = treatment_capital_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019)
capital_toxic_controls_2019_agg.es <- aggte(capital_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_agg.es)
f1 <- ggdid(capital_toxic_controls_2019_agg.es, title = "capital_toxic_controls_2019")
ggsave("capital_toxic_controls_2019.png", f1, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_capital_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019)
capital_intestinal_controls_2019_agg.es <- aggte(capital_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_agg.es)
f2 <- ggdid(capital_intestinal_controls_2019_agg.es, title = "capital_intestinal_controls_2019")
ggsave("capital_intestinal_controls_2019.png", f2, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_capital_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019)
capital_pregnancy_controls_2019_agg.es <- aggte(capital_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_agg.es)
f3 <- ggdid(capital_pregnancy_controls_2019_agg.es, title = "capital_pregnancy_controls_2019")
ggsave("capital_pregnancy_controls_2019.png", f3, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                       tname = "year",
                                       idname = "muni_id",
                                       gname = "G",
                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                       data = treatment_combined_hosp_2019,
                                       #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019)
combined_toxic_controls_2019_agg.es <- aggte(combined_toxic_controls_2019, type = "dynamic",
                                             min_e = -10, max_e = 10,
                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_agg.es)
f4 <- ggdid(combined_toxic_controls_2019_agg.es, title = "combined_toxic_controls_2019")
ggsave("combined_toxic_controls_2019.png", f4, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                            tname = "year",
                                            idname = "muni_id",
                                            gname = "G",
                                            xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                            data = treatment_combined_hosp_2019,
                                            #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019)
combined_intestinal_controls_2019_agg.es <- aggte(combined_intestinal_controls_2019, type = "dynamic",
                                                  min_e = -10, max_e = 10,
                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_agg.es)
f5 <- ggdid(combined_intestinal_controls_2019_agg.es, title = "combined_intestinal_controls_2019")
ggsave("combined_intestinal_controls_2019.png", f5, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_combined_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019)
combined_pregnancy_controls_2019_agg.es <- aggte(combined_pregnancy_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_agg.es)
f6 <- ggdid(combined_pregnancy_controls_2019_agg.es, title = "combined_pregnancy_controls_2019")
ggsave("combined_pregnancy_controls_2019.png", f6, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                      data = treatment_overlap_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019)
overlap_toxic_controls_2019_agg.es <- aggte(overlap_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_agg.es)
f7 <- ggdid(overlap_toxic_controls_2019_agg.es, title = "overlap_toxic_controls_2019")
ggsave("overlap_toxic_controls_2019.png", f7, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_overlap_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019)
overlap_intestinal_controls_2019_agg.es <- aggte(overlap_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_agg.es)
f8 <- ggdid(overlap_intestinal_controls_2019_agg.es, title = "overlap_intestinal_controls_2019")
ggsave("overlap_intestinal_controls_2019.png", f8, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_overlap_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019)
overlap_pregnancy_controls_2019_agg.es <- aggte(overlap_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_agg.es)
f9 <- ggdid(overlap_pregnancy_controls_2019_agg.es, title = "overlap_pregnancy_controls_2019")
ggsave("overlap_pregnancy_controls_2019.png", f9, width = 8, height = 5, dpi = 300)


#capital
capital_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_capital_hosp_2019_art,
)
summary(capital_toxic_controls_2019_art)
capital_toxic_controls_2019_art_agg.es <- aggte(capital_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_art_agg.es)
f10 <- ggdid(capital_toxic_controls_2019_art_agg.es, title = "capital_toxic_controls_2019_art")
ggsave("capital_toxic_controls_2019_art.png", f10, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_capital_hosp_2019_art,
)
summary(capital_intestinal_controls_2019_art)
capital_intestinal_controls_2019_art_agg.es <- aggte(capital_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_agg.es)
f11 <- ggdid(capital_intestinal_controls_2019_art_agg.es, title = "capital_intestinal_controls_2019_art")
ggsave("capital_intestinal_controls_2019_art.png", f11, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_capital_hosp_2019_art,
)
summary(capital_pregnancy_controls_2019_art)
capital_pregnancy_controls_2019_art_agg.es <- aggte(capital_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_agg.es)
f12 <- ggdid(capital_pregnancy_controls_2019_art_agg.es, title = "capital_pregnancy_controls_2019_art")
ggsave("capital_pregnancy_controls_2019_art.png", f12, width = 8, height = 5, dpi = 300)


#combined
combined_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_combined_hosp_2019_art,
)
summary(combined_toxic_controls_2019_art)
combined_toxic_controls_2019_art_agg.es <- aggte(combined_toxic_controls_2019_art, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_art_agg.es)
f13 <- ggdid(combined_toxic_controls_2019_art_agg.es, title = "combined_toxic_controls_2019_art")
ggsave("combined_toxic_controls_2019_art.png", f13, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_combined_hosp_2019_art,
)
summary(combined_intestinal_controls_2019_art)
combined_intestinal_controls_2019_art_agg.es <- aggte(combined_intestinal_controls_2019_art, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_agg.es)
f14 <- ggdid(combined_intestinal_controls_2019_art_agg.es, title = "combined_intestinal_controls_2019_art")
ggsave("combined_intestinal_controls_2019_art.png", f14, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_combined_hosp_2019_art,
)
summary(combined_pregnancy_controls_2019_art)
combined_pregnancy_controls_2019_art_agg.es <- aggte(combined_pregnancy_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_agg.es)
f15 <- ggdid(combined_pregnancy_controls_2019_art_agg.es, title = "combined_pregnancy_controls_2019_art")
ggsave("combined_pregnancy_controls_2019_art.png", f15, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_overlap_hosp_2019_art,
)
summary(overlap_toxic_controls_2019_art)
overlap_toxic_controls_2019_art_agg.es <- aggte(overlap_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_agg.es)
f16 <- ggdid(overlap_toxic_controls_2019_art_agg.es, title = "overlap_toxic_controls_2019_art")
ggsave("overlap_toxic_controls_2019_art.png", f16, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_overlap_hosp_2019_art,
)
summary(overlap_intestinal_controls_2019_art)
overlap_intestinal_controls_2019_art_agg.es <- aggte(overlap_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_agg.es)
f17 <- ggdid(overlap_intestinal_controls_2019_art_agg.es, title = "overlap_intestinal_controls_2019_art")
ggsave("overlap_intestinal_controls_2019_art.png", f17, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_overlap_hosp_2019_art,
)
summary(overlap_pregnancy_controls_2019_art)
overlap_pregnancy_controls_2019_art_agg.es <- aggte(overlap_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_agg.es)
f18 <- ggdid(overlap_pregnancy_controls_2019_art_agg.es, title = "overlap_pregnancy_controls_2019_art")
ggsave("overlap_pregnancy_controls_2019_art.png", f18, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_capital_hosp_2019_art_share,
)
summary(capital_toxic_controls_2019_art_share)
capital_toxic_controls_2019_art_share_agg.es <- aggte(capital_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_agg.es)
f19 <- ggdid(capital_toxic_controls_2019_art_share_agg.es, title = "capital_toxic_controls_2019_art_share")
ggsave("capital_toxic_controls_2019_art_share.png", f19, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_capital_hosp_2019_art_share,
)
summary(capital_intestinal_controls_2019_art_share)
capital_intestinal_controls_2019_art_share_agg.es <- aggte(capital_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_agg.es)
f20 <- ggdid(capital_intestinal_controls_2019_art_share_agg.es, title = "capital_intestinal_controls_2019_art_share")
ggsave("capital_intestinal_controls_2019_art_share.png", f20, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_capital_hosp_2019_art_share,
)
summary(capital_pregnancy_controls_2019_art_share)
capital_pregnancy_controls_2019_art_share_agg.es <- aggte(capital_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_agg.es)
f21 <- ggdid(capital_pregnancy_controls_2019_art_share_agg.es, title = "capital_pregnancy_controls_2019_art_share")
ggsave("capital_pregnancy_controls_2019_art_share.png", f21, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_combined_hosp_2019_art_share,
)
summary(combined_toxic_controls_2019_art_share)
combined_toxic_controls_2019_art_share_agg.es <- aggte(combined_toxic_controls_2019_art_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_agg.es)
f22 <- ggdid(combined_toxic_controls_2019_art_share_agg.es, title = "combined_toxic_controls_2019_art_share")
ggsave("combined_toxic_controls_2019_art_share.png", f22, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                      data = treatment_combined_hosp_2019_art_share,
)
summary(combined_intestinal_controls_2019_art_share)
combined_intestinal_controls_2019_art_share_agg.es <- aggte(combined_intestinal_controls_2019_art_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_agg.es)
f23 <- ggdid(combined_intestinal_controls_2019_art_share_agg.es, title = "combined_intestinal_controls_2019_art_share")
ggsave("combined_intestinal_controls_2019_art_share.png", f23, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_combined_hosp_2019_art_share,
)
summary(combined_pregnancy_controls_2019_art_share)
combined_pregnancy_controls_2019_art_share_agg.es <- aggte(combined_pregnancy_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_agg.es)
f24 <- ggdid(combined_pregnancy_controls_2019_art_share_agg.es, title = "combined_pregnancy_controls_2019_art_share")
ggsave("combined_pregnancy_controls_2019_art_share.png", f24, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_toxic_controls_2019_art_share)
overlap_toxic_controls_2019_art_share_agg.es <- aggte(overlap_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_agg.es)
f25 <- ggdid(overlap_toxic_controls_2019_art_share_agg.es, title = "overlap_toxic_controls_2019_art_share")
ggsave("overlap_toxic_controls_2019_art_share.png", f25, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_intestinal_controls_2019_art_share)
overlap_intestinal_controls_2019_art_share_agg.es <- aggte(overlap_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_agg.es)
f26 <- ggdid(overlap_intestinal_controls_2019_art_share_agg.es, title = "overlap_intestinal_controls_2019_art_share")
ggsave("overlap_intestinal_controls_2019_art_share.png", f26, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_pregnancy_controls_2019_art_share)
overlap_pregnancy_controls_2019_art_share_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_agg.es)
f27 <- ggdid(overlap_pregnancy_controls_2019_art_share_agg.es, title = "overlap_pregnancy_controls_2019_art_share")
ggsave("overlap_pregnancy_controls_2019_art_share.png", f27, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_capital_hosp_2019_ind,
)
summary(capital_toxic_controls_2019_ind)
capital_toxic_controls_2019_ind_agg.es <- aggte(capital_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_agg.es)
f28 <- ggdid(capital_toxic_controls_2019_ind_agg.es, title = "capital_toxic_controls_2019_ind")
ggsave("capital_toxic_controls_2019_ind.png", f28, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_capital_hosp_2019_ind,
)
summary(capital_intestinal_controls_2019_ind)
capital_intestinal_controls_2019_ind_agg.es <- aggte(capital_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_agg.es)
f29 <- ggdid(capital_intestinal_controls_2019_ind_agg.es, title = "capital_intestinal_controls_2019_ind")
ggsave("capital_intestinal_controls_2019_ind.png", f29, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_capital_hosp_2019_ind,
)
summary(capital_pregnancy_controls_2019_ind)
capital_pregnancy_controls_2019_ind_agg.es <- aggte(capital_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_agg.es)
f30 <- ggdid(capital_pregnancy_controls_2019_ind_agg.es, title = "capital_pregnancy_controls_2019_ind")
ggsave("capital_pregnancy_controls_2019_ind.png", f30, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_combined_hosp_2019_ind,
)
summary(combined_toxic_controls_2019_ind)
combined_toxic_controls_2019_ind_agg.es <- aggte(combined_toxic_controls_2019_ind, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_agg.es)
f31 <- ggdid(combined_toxic_controls_2019_ind_agg.es, title = "combined_toxic_controls_2019_ind")
ggsave("combined_toxic_controls_2019_ind.png", f31, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_combined_hosp_2019_ind,
)
summary(combined_intestinal_controls_2019_ind)
combined_intestinal_controls_2019_ind_agg.es <- aggte(combined_intestinal_controls_2019_ind, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_agg.es)
f32 <- ggdid(combined_intestinal_controls_2019_ind_agg.es, title = "combined_intestinal_controls_2019_ind")
ggsave("combined_intestinal_controls_2019_ind.png", f32, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_combined_hosp_2019_ind,
)
summary(combined_pregnancy_controls_2019_ind)
combined_pregnancy_controls_2019_ind_agg.es <- aggte(combined_pregnancy_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_agg.es)
f33 <- ggdid(combined_pregnancy_controls_2019_ind_agg.es, title = "combined_pregnancy_controls_2019_ind")
ggsave("combined_pregnancy_controls_2019_ind.png", f33, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_toxic_controls_2019_ind)
overlap_toxic_controls_2019_ind_agg.es <- aggte(overlap_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_agg.es)
f34 <- ggdid(overlap_toxic_controls_2019_ind_agg.es, title = "overlap_toxic_controls_2019_ind")
ggsave("overlap_toxic_controls_2019_ind.png", f34, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_intestinal_controls_2019_ind)
overlap_intestinal_controls_2019_ind_agg.es <- aggte(overlap_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_agg.es)
f35 <- ggdid(overlap_intestinal_controls_2019_ind_agg.es, title = "overlap_intestinal_controls_2019_ind")
ggsave("overlap_intestinal_controls_2019_ind.png", f35, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_pregnancy_controls_2019_ind)
overlap_pregnancy_controls_2019_ind_agg.es <- aggte(overlap_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_agg.es)
f36 <- ggdid(overlap_pregnancy_controls_2019_ind_agg.es, title = "overlap_pregnancy_controls_2019_ind")
ggsave("overlap_pregnancy_controls_2019_ind.png", f36, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_toxic_controls_2019_ind_share)
capital_toxic_controls_2019_ind_share_agg.es <- aggte(capital_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_agg.es)
f37 <- ggdid(capital_toxic_controls_2019_ind_share_agg.es, title = "capital_toxic_controls_2019_ind_share")
ggsave("capital_toxic_controls_2019_ind_share.png", f37, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_intestinal_controls_2019_ind_share)
capital_intestinal_controls_2019_ind_share_agg.es <- aggte(capital_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_agg.es)
f38 <-ggdid(capital_intestinal_controls_2019_ind_share_agg.es, title = "capital_intestinal_controls_2019_ind_share")
ggsave("capital_intestinal_controls_2019_ind_share.png", f38, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_pregnancy_controls_2019_ind_share)
capital_pregnancy_controls_2019_ind_share_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_agg.es)
f39 <- ggdid(capital_pregnancy_controls_2019_ind_share_agg.es, title = "capital_pregnancy_controls_2019_ind_share")
ggsave("capital_pregnancy_controls_2019_ind_share.png", f39, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_toxic_controls_2019_ind_share)
combined_toxic_controls_2019_ind_share_agg.es <- aggte(combined_toxic_controls_2019_ind_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_agg.es)
f40 <- ggdid(combined_toxic_controls_2019_ind_share_agg.es, title = "combined_toxic_controls_2019_ind_share")
ggsave("combined_toxic_controls_2019_ind_share.png", f40, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                      data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_intestinal_controls_2019_ind_share)
combined_intestinal_controls_2019_ind_share_agg.es <- aggte(combined_intestinal_controls_2019_ind_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_agg.es)
f41 <- ggdid(combined_intestinal_controls_2019_ind_share_agg.es, title = "combined_intestinal_controls_2019_ind_share")
ggsave("combined_intestinal_controls_2019_ind_share.png", f41, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_pregnancy_controls_2019_ind_share)
combined_pregnancy_controls_2019_ind_share_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_agg.es)
f42 <- ggdid(combined_pregnancy_controls_2019_ind_share_agg.es, title = "combined_pregnancy_controls_2019_ind_share")
ggsave("combined_pregnancy_controls_2019_ind_share.png", f42, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_toxic_controls_2019_ind_share)
overlap_toxic_controls_2019_ind_share_agg.es <- aggte(overlap_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_agg.es)
f43 <- ggdid(overlap_toxic_controls_2019_ind_share_agg.es, title = "overlap_toxic_controls_2019_ind_share")
ggsave("overlap_toxic_controls_2019_ind_share.png", f43, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_intestinal_controls_2019_ind_share)
overlap_intestinal_controls_2019_ind_share_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_agg.es)
f44 <- ggdid(overlap_intestinal_controls_2019_ind_share_agg.es, title = "overlap_intestinal_controls_2019_ind_share")
ggsave("overlap_intestinal_controls_2019_ind_share.png", f44, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_pregnancy_controls_2019_ind_share)
overlap_pregnancy_controls_2019_ind_share_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_agg.es)
f45 <- ggdid(overlap_pregnancy_controls_2019_ind_share_agg.es, title = "overlap_pregnancy_controls_2019_ind_share")
ggsave("overlap_pregnancy_controls_2019_ind_share.png", f45, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                             data = treatment_capital_hosp_2019_amazon,
)
summary(capital_toxic_controls_2019_amazon)
capital_toxic_controls_2019_amazon_agg.es <- aggte(capital_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_agg.es)
f46 <- ggdid(capital_toxic_controls_2019_amazon_agg.es, title = "capital_toxic_controls_2019_amazon")
ggsave("capital_toxic_controls_2019_amazon.png", f46, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                  data = treatment_capital_hosp_2019_amazon,
)
summary(capital_intestinal_controls_2019_amazon)
capital_intestinal_controls_2019_amazon_agg.es <- aggte(capital_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_agg.es)
f47 <- ggdid(capital_intestinal_controls_2019_amazon_agg.es, title = "capital_intestinal_controls_2019_amazon")
ggsave("capital_intestinal_controls_2019_amazon.png", f47, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_capital_hosp_2019_amazon,
)
summary(capital_pregnancy_controls_2019_amazon)
capital_pregnancy_controls_2019_amazon_agg.es <- aggte(capital_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_agg.es)
f48 <- ggdid(capital_pregnancy_controls_2019_amazon_agg.es , title = "capital_pregnancy_controls_2019_amazon")
ggsave("capital_pregnancy_controls_2019_amazon.png", f48, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_combined_hosp_2019_amazon,
)
summary(combined_toxic_controls_2019_amazon)
combined_toxic_controls_2019_amazon_agg.es <- aggte(combined_toxic_controls_2019_amazon, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_agg.es)
f49 <- ggdid(combined_toxic_controls_2019_amazon_agg.es, title = "combined_toxic_controls_2019_amazon")
ggsave("combined_toxic_controls_2019_amazon.png", f49, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_combined_hosp_2019_amazon,
)
summary(combined_intestinal_controls_2019_amazon)
combined_intestinal_controls_2019_amazon_agg.es <- aggte(combined_intestinal_controls_2019_amazon, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_agg.es)
f50 <- ggdid(combined_intestinal_controls_2019_amazon_agg.es, title = "combined_intestinal_controls_2019_amazon")
ggsave("combined_intestinal_controls_2019_amazon.png", f50, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                  data = treatment_combined_hosp_2019_amazon,
)
summary(combined_pregnancy_controls_2019_amazon)
combined_pregnancy_controls_2019_amazon_agg.es <- aggte(combined_pregnancy_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_agg.es)
f51 <- ggdid(combined_pregnancy_controls_2019_amazon_agg.es, title = "combined_pregnancy_controls_2019_amazon")
ggsave("combined_pregnancy_controls_2019_amazon.png", f51, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                             data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_toxic_controls_2019_amazon)
overlap_toxic_controls_2019_amazon_agg.es <- aggte(overlap_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_agg.es)
f52 <- ggdid(overlap_toxic_controls_2019_amazon_agg.es, title = "overlap_toxic_controls_2019_amazon")
ggsave("overlap_toxic_controls_2019_amazon.png", f52, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                  data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_intestinal_controls_2019_amazon)
overlap_intestinal_controls_2019_amazon_agg.es <- aggte(overlap_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_agg.es)
f53 <- ggdid(overlap_intestinal_controls_2019_amazon_agg.es, title = "overlap_intestinal_controls_2019_amazon")
ggsave("overlap_intestinal_controls_2019_amazon.png", f53, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_pregnancy_controls_2019_amazon)
overlap_pregnancy_controls_2019_amazon_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_agg.es)
f54 <- ggdid(overlap_pregnancy_controls_2019_amazon_agg.es, title = "overlap_pregnancy_controls_2019_amazon")
ggsave("overlap_pregnancy_controls_2019_amazon.png", f54, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_capital_child_mort_2019,
)
summary(capital_child_0_2_controls_2019)
capital_child_0_2_controls_2019_agg.es <- aggte(capital_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_agg.es)
f55 <- ggdid(capital_child_0_2_controls_2019_agg.es, title = "capital_child_0_2_controls_2019")
ggsave("capital_child_0_2_controls_2019.png", f55, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                           data = treatment_combined_child_mort_2019,
)
summary(combined_child_0_2_controls_2019)
combined_child_0_2_controls_2019_agg.es <- aggte(combined_child_0_2_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_agg.es)
f56 <- ggdid(combined_child_0_2_controls_2019_agg.es, title = "combined_child_0_2_controls_2019")
ggsave("combined_child_0_2_controls_2019.png", f56, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                          data = treatment_overlap_child_mort_2019,
)
summary(overlap_child_0_2_controls_2019)
overlap_child_0_2_controls_2019_agg.es <- aggte(overlap_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_agg.es)
f57 <- ggdid(overlap_child_0_2_controls_2019_agg.es, title = "overlap_child_0_2_controls_2019")
ggsave("overlap_child_0_2_controls_2019.png", f57, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_capital_child_mort_2019_art,
)
summary(capital_child_0_2_controls_2019_art)
capital_child_0_2_controls_2019_art_agg.es <- aggte(capital_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_agg.es)
f58 <- ggdid(capital_child_0_2_controls_2019_art_agg.es, title = "capital_child_0_2_controls_2019_art")
ggsave("capital_child_0_2_controls_2019_art.png", f58, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_combined_child_mort_2019_art,
)
summary(combined_child_0_2_controls_2019_art)
combined_child_0_2_controls_2019_art_agg.es <- aggte(combined_child_0_2_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_agg.es)
f59 <- ggdid(combined_child_0_2_controls_2019_art_agg.es, title = "combined_child_0_2_controls_2019_art")
ggsave("combined_child_0_2_controls_2019_art.png", f59, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_overlap_child_mort_2019_art,
)
summary(overlap_child_0_2_controls_2019_art)
overlap_child_0_2_controls_2019_art_agg.es <- aggte(overlap_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_agg.es)
f60 <- ggdid(overlap_child_0_2_controls_2019_art_agg.es, title = "overlap_child_0_2_controls_2019_art")
ggsave("overlap_child_0_2_controls_2019_art.png", f60, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_capital_child_mort_2019_art_share,
)
summary(capital_child_0_2_controls_2019_art_share)
capital_child_0_2_controls_2019_art_share_agg.es <- aggte(capital_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_agg.es)
f61 <- ggdid(capital_child_0_2_controls_2019_art_share_agg.es, title = "capital_child_0_2_controls_2019_art_share")
ggsave("capital_child_0_2_controls_2019_art_share.png", f61, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_combined_child_mort_2019_art_share,
)
summary(combined_child_0_2_controls_2019_art_share)
combined_child_0_2_controls_2019_art_share_agg.es <- aggte(combined_child_0_2_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_agg.es)
f62 <- ggdid(combined_child_0_2_controls_2019_art_share_agg.es, title = "combined_child_0_2_controls_2019_art_share")
ggsave("combined_child_0_2_controls_2019_art_share.png", f62, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_overlap_child_mort_2019_art_share,
)
summary(overlap_child_0_2_controls_2019_art_share)
overlap_child_0_2_controls_2019_art_share_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_agg.es)
f63 <- ggdid(overlap_child_0_2_controls_2019_art_share_agg.es, title = "overlap_child_0_2_controls_2019_art_share")
ggsave("overlap_child_0_2_controls_2019_art_share.png", f63, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_capital_child_mort_2019_ind,
)
summary(capital_child_0_2_controls_2019_ind)
capital_child_0_2_controls_2019_ind_agg.es <- aggte(capital_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_agg.es)
f64 <- ggdid(capital_child_0_2_controls_2019_ind_agg.es, title = "capital_child_0_2_controls_2019_ind")
ggsave("capital_child_0_2_controls_2019_ind.png", f64, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_combined_child_mort_2019_ind,
)
summary(combined_child_0_2_controls_2019_ind)
combined_child_0_2_controls_2019_ind_agg.es <- aggte(combined_child_0_2_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_agg.es)
f65 <- ggdid(combined_child_0_2_controls_2019_ind_agg.es, title = "combined_child_0_2_controls_2019_ind")
ggsave("combined_child_0_2_controls_2019_ind.png", f65, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                              data = treatment_overlap_child_mort_2019_ind,
)
summary(overlap_child_0_2_controls_2019_ind)
overlap_child_0_2_controls_2019_ind_agg.es <- aggte(overlap_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_agg.es)
f66 <- ggdid(overlap_child_0_2_controls_2019_ind_agg.es, title = "overlap_child_0_2_controls_2019_ind")
ggsave("overlap_child_0_2_controls_2019_ind.png", f66, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_capital_child_mort_2019_ind_share,
)
summary(capital_child_0_2_controls_2019_ind_share)
capital_child_0_2_controls_2019_ind_share_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_agg.es)
f67 <- ggdid(capital_child_0_2_controls_2019_ind_share_agg.es, title = "capital_child_0_2_controls_2019_ind_share")
ggsave("capital_child_0_2_controls_2019_ind_share.png", f67, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_combined_child_mort_2019_ind_share,
)
summary(combined_child_0_2_controls_2019_ind_share)
combined_child_0_2_controls_2019_ind_share_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_agg.es)
f68 <- ggdid(combined_child_0_2_controls_2019_ind_share_agg.es, title = "combined_child_0_2_controls_2019_ind_share")
ggsave("combined_child_0_2_controls_2019_ind_share.png", f68, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_overlap_child_mort_2019_ind_share,
)
summary(overlap_child_0_2_controls_2019_ind_share)
overlap_child_0_2_controls_2019_ind_share_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_agg.es)
f69 <- ggdid(overlap_child_0_2_controls_2019_ind_share_agg.es, title = "overlap_child_0_2_controls_2019_ind_share")
ggsave("overlap_child_0_2_controls_2019_ind_share.png", f69, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_capital_child_mort_2019_amazon,
)
summary(capital_child_0_2_controls_2019_amazon)
capital_child_0_2_controls_2019_amazon_agg.es <- aggte(capital_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_agg.es)
f70 <- ggdid(capital_child_0_2_controls_2019_amazon_agg.es, title = "capital_child_0_2_controls_2019_amazon")
ggsave("capital_child_0_2_controls_2019_amazon.png", f70, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                  data = treatment_combined_child_mort_2019_amazon,
)
summary(combined_child_0_2_controls_2019_amazon)
combined_child_0_2_controls_2019_amazon_agg.es <- aggte(combined_child_0_2_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_agg.es)
f71 <- ggdid(combined_child_0_2_controls_2019_amazon_agg.es, title = "combined_child_0_2_controls_2019_amazon")
ggsave("combined_child_0_2_controls_2019_amazon.png", f71, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                 data = treatment_overlap_child_mort_2019_amazon,
)
summary(overlap_child_0_2_controls_2019_amazon)
overlap_child_0_2_controls_2019_amazon_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_agg.es)
f72 <- ggdid(overlap_child_0_2_controls_2019_amazon_agg.es, title = "overlap_child_0_2_controls_2019_amazon")
ggsave("overlap_child_0_2_controls_2019_amazon.png", f72, width = 8, height = 5, dpi = 300)


### with weights
setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_2001_weights/")
#toxic effects
capital_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_capital_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019_weighted)
capital_toxic_controls_2019_weighted_agg.es <- aggte(capital_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_toxic_controls_2019_weighted_agg.es)
f1a <- ggdid(capital_toxic_controls_2019_weighted_agg.es, title = "capital_toxic_controls_2019_weighted")
ggsave("capital_toxic_controls_2019_weighted.png", f1a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                data = treatment_combined_hosp_2019,
                                                weightsname = "pop"
                                                #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019_weighted)
combined_toxic_controls_2019_weighted_agg.es <- aggte(combined_toxic_controls_2019_weighted, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_toxic_controls_2019_weighted_agg.es)
f2a <- ggdid(combined_toxic_controls_2019_weighted_agg.es, title = "combined_toxic_controls_2019_weighted")
ggsave("combined_toxic_controls_2019_weighted.png", f2a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                               data = treatment_overlap_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019_weighted)
overlap_toxic_controls_2019_weighted_agg.es <- aggte(overlap_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_toxic_controls_2019_weighted_agg.es)
f3a <- ggdid(overlap_toxic_controls_2019_weighted_agg.es, title = "overlap_toxic_controls_2019_weighted")
ggsave("overlap_toxic_controls_2019_weighted.png", f3a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_capital_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_weighted)
capital_toxic_controls_2019_art_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_art_weighted_agg.es)
f4a <- ggdid(capital_toxic_controls_2019_art_weighted_agg.es, title = "capital_toxic_controls_2019_art_weighted")
ggsave("capital_toxic_controls_2019_art_weighted.png", f4a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_combined_hosp_2019_art,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_weighted)
combined_toxic_controls_2019_art_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_art_weighted_agg.es)
f5a <- ggdid(combined_toxic_controls_2019_art_weighted_agg.es, title = "combined_toxic_controls_2019_art_weighted")
ggsave("combined_toxic_controls_2019_art_weighted.png", f5a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_overlap_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_weighted)
overlap_toxic_controls_2019_art_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_weighted_agg.es)
f6a <- ggdid(overlap_toxic_controls_2019_art_weighted_agg.es, title = "overlap_toxic_controls_2019_art_weighted")
ggsave("overlap_toxic_controls_2019_art_weighted.png", f6a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_capital_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_share_weighted)
capital_toxic_controls_2019_art_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_weighted_agg.es)
f7a <- ggdid(capital_toxic_controls_2019_art_share_weighted_agg.es, title = "capital_toxic_controls_2019_art_share_weighted")
ggsave("capital_toxic_controls_2019_art_share_weighted.png", f7a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_combined_hosp_2019_art_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_share_weighted)
combined_toxic_controls_2019_art_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_weighted_agg.es)
f8a <- ggdid(combined_toxic_controls_2019_art_share_weighted_agg.es, title = "combined_toxic_controls_2019_art_share_weighted")
ggsave("combined_toxic_controls_2019_art_share_weighted.png", f8a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_overlap_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_share_weighted)
overlap_toxic_controls_2019_art_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_weighted_agg.es)
f9a <- ggdid(overlap_toxic_controls_2019_art_share_weighted_agg.es, title = "overlap_toxic_controls_2019_art_share_weighted")
ggsave("overlap_toxic_controls_2019_art_share_weighted.png", f9a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_capital_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_weighted)
capital_toxic_controls_2019_ind_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_weighted_agg.es)
f10a <-ggdid(capital_toxic_controls_2019_ind_weighted_agg.es, title = "capital_toxic_controls_2019_ind_weighted")
ggsave("capital_toxic_controls_2019_ind_weighted.png", f10a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_combined_hosp_2019_ind,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_weighted)
combined_toxic_controls_2019_ind_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_weighted_agg.es)
f11a <-ggdid(combined_toxic_controls_2019_ind_weighted_agg.es, title = "combined_toxic_controls_2019_ind_weighted")
ggsave("combined_toxic_controls_2019_ind_weighted.png", f11a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_overlap_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_weighted)
overlap_toxic_controls_2019_ind_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_weighted_agg.es)
f12a <- ggdid(overlap_toxic_controls_2019_ind_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_weighted")
ggsave("overlap_toxic_controls_2019_ind_weighted.png", f12a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_capital_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_share_weighted)
capital_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_weighted_agg.es)
f13a <- ggdid(capital_toxic_controls_2019_ind_share_weighted_agg.es, title = "capital_toxic_controls_2019_ind_share_weighted")
ggsave("capital_toxic_controls_2019_ind_share_weighted.png", f13a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_combined_hosp_2019_ind_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_share_weighted)
combined_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_weighted_agg.es)
f14a <- ggdid(combined_toxic_controls_2019_ind_share_weighted_agg.es, title = "combined_toxic_controls_2019_ind_share_weighted")
ggsave("combined_toxic_controls_2019_ind_share_weighted.png", f14a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_overlap_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_share_weighted)
overlap_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_weighted_agg.es)
f15a <- ggdid(overlap_toxic_controls_2019_ind_share_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_share_weighted")
ggsave("overlap_toxic_controls_2019_ind_share_weighted.png", f15a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                      data = treatment_capital_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(capital_toxic_controls_2019_amazon_weighted)
capital_toxic_controls_2019_amazon_weighted_agg.es <- aggte(capital_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_weighted_agg.es)
f16a <- ggdid(capital_toxic_controls_2019_amazon_weighted_agg.es, title = "capital_toxic_controls_2019_amazon_weighted")
ggsave("capital_toxic_controls_2019_amazon_weighted.png", f16a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_combined_hosp_2019_amazon,
                                                       weightsname = "pop"
)
summary(combined_toxic_controls_2019_amazon_weighted)
combined_toxic_controls_2019_amazon_weighted_agg.es <- aggte(combined_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_weighted_agg.es)
f17a <- ggdid(combined_toxic_controls_2019_amazon_weighted_agg.es, title = "combined_toxic_controls_2019_amazon_weighted")
ggsave("combined_toxic_controls_2019_amazon_weighted.png", f17a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                      data = treatment_overlap_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(overlap_toxic_controls_2019_amazon_weighted)
overlap_toxic_controls_2019_amazon_weighted_agg.es <- aggte(overlap_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_weighted_agg.es)
f18a <- ggdid(overlap_toxic_controls_2019_amazon_weighted_agg.es, title = "overlap_toxic_controls_2019_amazon_weighted")
ggsave("overlap_toxic_controls_2019_amazon_weighted.png", f18a, width = 8, height = 5, dpi = 300)

#intestinal
capital_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_capital_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019_weighted)
capital_intestinal_controls_2019_weighted_agg.es <- aggte(capital_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_intestinal_controls_2019_weighted_agg.es)
f19a <- ggdid(capital_intestinal_controls_2019_weighted_agg.es, title = "capital_intestinal_controls_2019_weighted")
ggsave("capital_intestinal_controls_2019_weighted.png", f19a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                     data = treatment_combined_hosp_2019,
                                                     weightsname = "pop"
                                                     #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019_weighted)
combined_intestinal_controls_2019_weighted_agg.es <- aggte(combined_intestinal_controls_2019_weighted, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_intestinal_controls_2019_weighted_agg.es)
f20a <- ggdid(combined_intestinal_controls_2019_weighted_agg.es, title = "combined_intestinal_controls_2019_weighted")
ggsave("combined_intestinal_controls_2019_weighted.png", f20a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_overlap_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019_weighted)
overlap_intestinal_controls_2019_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_intestinal_controls_2019_weighted_agg.es)
f21a <- ggdid(overlap_intestinal_controls_2019_weighted_agg.es, title = "overlap_intestinal_controls_2019_weighted")
ggsave("overlap_intestinal_controls_2019_weighted.png", f21a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_capital_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_weighted)
capital_intestinal_controls_2019_art_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_weighted_agg.es)
f22a <- ggdid(capital_intestinal_controls_2019_art_weighted_agg.es, title = "capital_intestinal_controls_2019_art_weighted")
ggsave("capital_intestinal_controls_2019_art_weighted.png", f22a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_combined_hosp_2019_art,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_weighted)
combined_intestinal_controls_2019_art_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_weighted_agg.es)
f23a <- ggdid(combined_intestinal_controls_2019_art_weighted_agg.es, title = "combined_intestinal_controls_2019_art_weighted")
ggsave("combined_intestinal_controls_2019_art_weighted.png", f23a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_overlap_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_weighted)
overlap_intestinal_controls_2019_art_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_weighted_agg.es)
f24a <- ggdid(overlap_intestinal_controls_2019_art_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_weighted")
ggsave("overlap_intestinal_controls_2019_art_weighted.png", f24a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_capital_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_share_weighted)
capital_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_weighted_agg.es)
f25a <- ggdid(capital_intestinal_controls_2019_art_share_weighted_agg.es, title = "capital_intestinal_controls_2019_art_share_weighted")
ggsave("capital_intestinal_controls_2019_art_share_weighted.png", f25a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                               data = treatment_combined_hosp_2019_art_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_share_weighted)
combined_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_weighted_agg.es)
f26a <- ggdid(combined_intestinal_controls_2019_art_share_weighted_agg.es, title = "combined_intestinal_controls_2019_art_share_weighted")
ggsave("combined_intestinal_controls_2019_art_share_weighted.png", f26a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_overlap_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_share_weighted)
overlap_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_weighted_agg.es)
f27a <- ggdid(overlap_intestinal_controls_2019_art_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_share_weighted")
ggsave("overlap_intestinal_controls_2019_art_share_weighted.png", f27a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_capital_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_weighted)
capital_intestinal_controls_2019_ind_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_weighted_agg.es)
f28a <- ggdid(capital_intestinal_controls_2019_ind_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_weighted")
ggsave("capital_intestinal_controls_2019_ind_weighted.png", f28a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                         data = treatment_combined_hosp_2019_ind,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_weighted)
combined_intestinal_controls_2019_ind_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_weighted_agg.es)
f29a <- ggdid(combined_intestinal_controls_2019_ind_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_weighted")
ggsave("combined_intestinal_controls_2019_ind_weighted.png", f29a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_overlap_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_weighted)
overlap_intestinal_controls_2019_ind_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_weighted_agg.es)
f30a <- ggdid(overlap_intestinal_controls_2019_ind_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_weighted")
ggsave("overlap_intestinal_controls_2019_ind_weighted.png", f30a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_capital_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_share_weighted)
capital_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_weighted_agg.es)
f31a <- ggdid(capital_intestinal_controls_2019_ind_share_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_share_weighted")
ggsave("capital_intestinal_controls_2019_ind_share_weighted.png", f31a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                               data = treatment_combined_hosp_2019_ind_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_share_weighted)
combined_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_weighted_agg.es)
f32a <- ggdid(combined_intestinal_controls_2019_ind_share_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_share_weighted")
ggsave("combined_intestinal_controls_2019_ind_share_weighted.png", f32a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_overlap_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_share_weighted)
overlap_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_weighted_agg.es)
f33a <- ggdid(overlap_intestinal_controls_2019_ind_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_share_weighted")
ggsave("overlap_intestinal_controls_2019_ind_share_weighted.png", f33a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                           data = treatment_capital_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(capital_intestinal_controls_2019_amazon_weighted)
capital_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(capital_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_weighted_agg.es)
f34a <- ggdid(capital_intestinal_controls_2019_amazon_weighted_agg.es, title = "capital_intestinal_controls_2019_amazon_weighted")
ggsave("capital_intestinal_controls_2019_amazon_weighted.png", f34a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                            tname = "year",
                                                            idname = "muni_id",
                                                            gname = "G",
                                                            xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                            data = treatment_combined_hosp_2019_amazon,
                                                            weightsname = "pop"
)
summary(combined_intestinal_controls_2019_amazon_weighted)
combined_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(combined_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                  min_e = -10, max_e = 10,
                                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_weighted_agg.es)
f35a <- ggdid(combined_intestinal_controls_2019_amazon_weighted_agg.es, title = "combined_intestinal_controls_2019_amazon_weighted")
ggsave("combined_intestinal_controls_2019_amazon_weighted.png", f35a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                           data = treatment_overlap_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_amazon_weighted)
overlap_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_weighted_agg.es)
f36a <- ggdid(overlap_intestinal_controls_2019_amazon_weighted_agg.es, title = "overlap_intestinal_controls_2019_amazon_weighted")
ggsave("overlap_intestinal_controls_2019_amazon_weighted.png", f36a, width = 8, height = 5, dpi = 300)

#pregnancy
capital_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_capital_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019_weighted)
capital_pregnancy_controls_2019_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_pregnancy_controls_2019_weighted_agg.es)
f37a <- ggdid(capital_pregnancy_controls_2019_weighted_agg.es, title = "capital_pregnancy_controls_2019_weighted")
ggsave("capital_pregnancy_controls_2019_weighted.png", f37a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_combined_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019_weighted)
combined_pregnancy_controls_2019_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_pregnancy_controls_2019_weighted_agg.es)
f38a <- ggdid(combined_pregnancy_controls_2019_weighted_agg.es, title = "combined_pregnancy_controls_2019_weighted")
ggsave("combined_pregnancy_controls_2019_weighted.png", f38a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_overlap_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019_weighted)
overlap_pregnancy_controls_2019_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_weighted_agg.es)
f39a <- ggdid(overlap_pregnancy_controls_2019_weighted_agg.es, title = "overlap_pregnancy_controls_2019_weighted")
ggsave("overlap_pregnancy_controls_2019_weighted.png", f39a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_capital_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_weighted)
capital_pregnancy_controls_2019_art_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_weighted_agg.es)
f40a <- ggdid(capital_pregnancy_controls_2019_art_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_weighted")
ggsave("capital_pregnancy_controls_2019_art_weighted.png", f40a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_combined_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_weighted)
combined_pregnancy_controls_2019_art_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_weighted_agg.es)
f41a <- ggdid(combined_pregnancy_controls_2019_art_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_weighted")
ggsave("combined_pregnancy_controls_2019_art_weighted.png", f41a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_overlap_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_weighted)
overlap_pregnancy_controls_2019_art_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_weighted_agg.es)
f42a <- ggdid(overlap_pregnancy_controls_2019_art_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_weighted")
ggsave("overlap_pregnancy_controls_2019_art_weighted.png", f42a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_capital_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_share_weighted)
capital_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_weighted_agg.es)
f43a <- ggdid(capital_pregnancy_controls_2019_art_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_share_weighted")
ggsave("capital_pregnancy_controls_2019_art_share_weighted.png", f43a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_combined_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_share_weighted)
combined_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_weighted_agg.es)
f44a <- ggdid(combined_pregnancy_controls_2019_art_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_share_weighted")
ggsave("combined_pregnancy_controls_2019_art_share_weighted.png", f44a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_overlap_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_share_weighted)
overlap_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_weighted_agg.es)
f45a <- ggdid(overlap_pregnancy_controls_2019_art_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_share_weighted")
ggsave("overlap_pregnancy_controls_2019_art_share_weighted.png", f45a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_capital_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_weighted)
capital_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_weighted_agg.es)
f46a <- ggdid(capital_pregnancy_controls_2019_ind_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_weighted")
ggsave("capital_pregnancy_controls_2019_ind_weighted.png", f46a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_combined_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_weighted)
combined_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_weighted_agg.es)
f47a <- ggdid(combined_pregnancy_controls_2019_ind_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_weighted")
ggsave("combined_pregnancy_controls_2019_ind_weighted.png", f47a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_overlap_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_weighted)
overlap_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_weighted_agg.es)
f48a <- ggdid(overlap_pregnancy_controls_2019_ind_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_weighted.png", f48a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_capital_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_share_weighted)
capital_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_weighted_agg.es)
f49a <- ggdid(capital_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_share_weighted")
ggsave("capital_pregnancy_controls_2019_ind_share_weighted.png", f49a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_combined_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_share_weighted)
combined_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_weighted_agg.es)
f50a <- ggdid(combined_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_share_weighted")
ggsave("combined_pregnancy_controls_2019_ind_share_weighted.png", f50a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_overlap_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_share_weighted)
overlap_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es)
f51a <- ggdid(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_share_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_share_weighted.png", f51a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_capital_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_amazon_weighted)
capital_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_weighted_agg.es)
f52a <- ggdid(capital_pregnancy_controls_2019_amazon_weighted_agg.es, title = "capital_pregnancy_controls_2019_amazon_weighted")
ggsave("capital_pregnancy_controls_2019_amazon_weighted.png", f52a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                           data = treatment_combined_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_amazon_weighted)
combined_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_weighted_agg.es)
f53a <- ggdid(combined_pregnancy_controls_2019_amazon_weighted_agg.es, title = "combined_pregnancy_controls_2019_amazon_weighted")
ggsave("combined_pregnancy_controls_2019_amazon_weighted.png", f53a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_overlap_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_amazon_weighted)
overlap_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_weighted_agg.es)
f54a <- ggdid(overlap_pregnancy_controls_2019_amazon_weighted_agg.es, title = "overlap_pregnancy_controls_2019_amazon_weighted")
ggsave("overlap_pregnancy_controls_2019_amazon_weighted.png", f54a, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_capital_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_child_0_2_controls_2019_weighted)
capital_child_0_2_controls_2019_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_child_0_2_controls_2019_weighted_agg.es)
f55a <- ggdid(capital_child_0_2_controls_2019_weighted_agg.es, title = "capital_child_0_2_controls_2019_weighted")
ggsave("capital_child_0_2_controls_2019_weighted.png", f55a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                    data = treatment_combined_child_mort_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_child_0_2_controls_2019_weighted)
combined_child_0_2_controls_2019_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_child_0_2_controls_2019_weighted_agg.es)
f56a <- ggdid(combined_child_0_2_controls_2019_weighted_agg.es, title = "combined_child_0_2_controls_2019_weighted")
ggsave("combined_child_0_2_controls_2019_weighted.png", f56a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                   data = treatment_overlap_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_child_0_2_controls_2019_weighted)
overlap_child_0_2_controls_2019_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_weighted_agg.es)
f57a <- ggdid(overlap_child_0_2_controls_2019_weighted_agg.es, title = "overlap_child_0_2_controls_2019_weighted")
ggsave("overlap_child_0_2_controls_2019_weighted.png", f57a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_capital_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_weighted)
capital_child_0_2_controls_2019_art_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_weighted_agg.es)
f58a <- ggdid(capital_child_0_2_controls_2019_art_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_weighted")
ggsave("capital_child_0_2_controls_2019_art_weighted.png", f58a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_combined_child_mort_2019_art,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_weighted)
combined_child_0_2_controls_2019_art_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_weighted_agg.es)
f59a <- ggdid(combined_child_0_2_controls_2019_art_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_weighted")
ggsave("combined_child_0_2_controls_2019_art_weighted.png", f59a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_overlap_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_weighted)
overlap_child_0_2_controls_2019_art_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_weighted_agg.es)
f60a <- ggdid(overlap_child_0_2_controls_2019_art_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_weighted")
ggsave("overlap_child_0_2_controls_2019_art_weighted.png", f60a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_capital_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_share_weighted)
capital_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_weighted_agg.es)
f61a <- ggdid(capital_child_0_2_controls_2019_art_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_share_weighted")
ggsave("capital_child_0_2_controls_2019_art_share_weighted.png", f61a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_combined_child_mort_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_share_weighted)
combined_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_weighted_agg.es)
f62a <- ggdid(combined_child_0_2_controls_2019_art_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_share_weighted")
ggsave("combined_child_0_2_controls_2019_art_share_weighted.png", f62a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_overlap_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_share_weighted)
overlap_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_weighted_agg.es)
f63a <- ggdid(overlap_child_0_2_controls_2019_art_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_share_weighted")
ggsave("overlap_child_0_2_controls_2019_art_share_weighted.png", f63a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_capital_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_weighted)
capital_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_weighted_agg.es)
f64a <- ggdid(capital_child_0_2_controls_2019_ind_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_weighted")
ggsave("capital_child_0_2_controls_2019_ind_weighted.png", f64a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                        data = treatment_combined_child_mort_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_weighted)
combined_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_weighted_agg.es)
f65a <- ggdid(combined_child_0_2_controls_2019_ind_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_weighted")
ggsave("combined_child_0_2_controls_2019_ind_weighted.png", f65a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                       data = treatment_overlap_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_weighted)
overlap_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_weighted_agg.es)
f66a <- ggdid(overlap_child_0_2_controls_2019_ind_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_weighted.png", f66a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_capital_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_share_weighted)
capital_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_weighted_agg.es)
f67a <- ggdid(capital_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_share_weighted")
ggsave("capital_child_0_2_controls_2019_ind_share_weighted.png", f67a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                              data = treatment_combined_child_mort_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_share_weighted)
combined_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_weighted_agg.es)
f68a <- ggdid(combined_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_share_weighted")
ggsave("combined_child_0_2_controls_2019_ind_share_weighted.png", f68a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                             data = treatment_overlap_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_share_weighted)
overlap_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es)
f69a <- ggdid(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_share_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_share_weighted.png", f69a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_capital_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_amazon_weighted)
capital_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_weighted_agg.es)
f70a <- ggdid(capital_child_0_2_controls_2019_amazon_weighted_agg.es, title = "capital_child_0_2_controls_2019_amazon_weighted")
ggsave("capital_child_0_2_controls_2019_amazon_weighted.png", f70a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                           data = treatment_combined_child_mort_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_amazon_weighted)
combined_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_weighted_agg.es)
f71a <- ggdid(combined_child_0_2_controls_2019_amazon_weighted_agg.es, title = "combined_child_0_2_controls_2019_amazon_weighted")
ggsave("combined_child_0_2_controls_2019_amazon_weighted.png", f71a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~ share_poor_2000 + illiteracy_2000 + share_rural_2000 + share_electricity_2000,
                                                          data = treatment_overlap_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_amazon_weighted)
overlap_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_weighted_agg.es)
f72a <- ggdid(overlap_child_0_2_controls_2019_amazon_weighted_agg.es, title = "overlap_child_0_2_controls_2019_amazon_weighted")
ggsave("overlap_child_0_2_controls_2019_amazon_weighted.png", f72a, width = 8, height = 5, dpi = 300)

#2003
rm(list = ls())
library(readr)
library(sf)
library(did)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyselect)

# only with control group
# here G=0 if upstream, G=NA if never part of a basin chian, G=min_year if either mine or downstream
process_treatment_data <- function(path) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  # 1. Compute global median cutoff (mine basin–year level)
  mining_median_mine <- data_raw %>%
    filter(status == "mine") %>%
    group_by(mine_basin, year) %>%
    summarise(
      mining_ha_total = first(mining_ha_total),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining_ha_total, na.rm = TRUE)
    ) %>%
    pull(median)
  
  # 2. Apply cutoff and construct G
  data_processed <- data_raw %>% 
    group_by(muni_id) %>% 
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>% 
    ungroup() %>% 
    
    mutate(
      mining_median_mine = mining_median_mine,
      crossed = mining_ha_total > mining_median_mine
    ) %>%
    
    group_by(muni_id) %>% 
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>% 
    ungroup() %>% 
    filter(!is.na(G))
  
  return(data_processed)
}
treatment_capital_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg"
)

treatment_combined_child_mort <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg"
)

treatment_overlap_child_mort  <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg"
)

treatment_capital_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg"
)

treatment_combined_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg"
)

treatment_overlap_hosp <- process_treatment_data(
  "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg"
)

compute_median <- function(data, mining_var) {
  data %>%
    filter(status == "mine") %>%
    st_set_geometry(NULL) %>%  # drop geometry before summarising
    group_by(mine_basin, year) %>%
    summarise(
      mining = first(.data[[mining_var]]),
      .groups = "drop"
    ) %>%
    summarise(
      median = median(mining, na.rm = TRUE)
    ) %>%
    pull(median)
}

build_treatment <- function(
    path,
    mining_var,
    year_min = 2003,
    year_max = 2019,
    mine_type_filter = NULL,
    share_var = NULL,
    share_cutoff = NULL
) {
  
  # Read data and drop geometry immediately
  data_raw <- st_read(path) %>%
    # st_set_geometry(NULL) %>%   <--- drop geometry
    filter(between(year, year_min, year_max))
  
  # Optional filters
  if (!is.null(mine_type_filter)) {
    data_raw <- data_raw %>% filter(mine_type == mine_type_filter)
  }
  
  if (!is.null(share_var)) {
    data_raw <- data_raw %>%
      group_by(muni_id) %>%
      filter(any(.data[[share_var]] > share_cutoff)) %>%
      ungroup()
  }
  
  # Compute cutoff ONCE
  median <- compute_median(data_raw, mining_var)
  
  # Apply treatment logic
  data_raw %>%
    group_by(muni_id) %>%
    filter(!any(dist_n >= 3, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      crossed = .data[[mining_var]] > median
    ) %>%
    group_by(muni_id) %>%
    mutate(
      G = case_when(
        any(status == "upstream", na.rm = TRUE) ~ 0,
        all(is.na(basin_id)) ~ NA_real_,
        all(!crossed, na.rm = TRUE) ~ NA_real_,
        TRUE ~ min(year[crossed], na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(G))
}

# artisanal only
treatment_capital_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_child_mort_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_capital_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_combined_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

treatment_overlap_hosp_2019_art <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  mine_type_filter = "artisanal"
)

#artisanal share >0.5
treatment_capital_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_child_mort_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_capital_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_combined_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

treatment_overlap_hosp_2019_art_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_artisanal",
  share_var = "mining_ha_artisanal_share",
  share_cutoff = 0.5
)

#industrial only
treatment_capital_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_child_mort_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_capital_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_combined_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)
treatment_overlap_hosp_2019_ind <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  mine_type_filter = "industrial"
)

#industrial share >0.5
treatment_capital_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_child_mort_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_capital_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_combined_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)
treatment_overlap_hosp_2019_ind_share <- build_treatment(
  path = "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp.gpkg",
  mining_var = "mining_ha_industrial",
  share_var = "mining_ha_industrial_share",
  share_cutoff = 0.5
)


### from 2002-2019 with controls
treatment_capital_child_mort_2019 <- treatment_capital_child_mort %>%
  filter(between(year, 2003, 2019))
treatment_combined_child_mort_2019 <- treatment_combined_child_mort %>%
  filter(between(year, 2003, 2019))
treatment_overlap_child_mort_2019 <- treatment_overlap_child_mort %>%
  filter(between(year, 2003, 2019))
treatment_capital_hosp_2019 <- treatment_capital_hosp %>%
  filter(between(year, 2003, 2019))
treatment_combined_hosp_2019 <- treatment_combined_hosp %>%
  filter(between(year, 2003, 2019))
treatment_overlap_hosp_2019 <- treatment_overlap_hosp %>%
  filter(between(year, 2003, 2019))

### from 2002-2019 with controls only legal amazon
treatment_capital_child_mort_2019_amazon <- treatment_capital_child_mort %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_child_mort_2019_amazon <- treatment_combined_child_mort %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_child_mort_2019_amazon <- treatment_overlap_child_mort %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)
treatment_capital_hosp_2019_amazon <- treatment_capital_hosp %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)
treatment_combined_hosp_2019_amazon <- treatment_combined_hosp %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)
treatment_overlap_hosp_2019_amazon <- treatment_overlap_hosp %>%
  filter(between(year, 2003, 2019)) %>%
  filter(legal_amazon == 1)


setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_2003/")

#capital
capital_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                      data = treatment_capital_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019)
capital_toxic_controls_2019_agg.es <- aggte(capital_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_agg.es)
f1 <- ggdid(capital_toxic_controls_2019_agg.es, title = "capital_toxic_controls_2019")
ggsave("capital_toxic_controls_2019.png", f1, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_capital_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019)
capital_intestinal_controls_2019_agg.es <- aggte(capital_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_agg.es)
f2 <- ggdid(capital_intestinal_controls_2019_agg.es, title = "capital_intestinal_controls_2019")
ggsave("capital_intestinal_controls_2019.png", f2, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_capital_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019)
capital_pregnancy_controls_2019_agg.es <- aggte(capital_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_agg.es)
f3 <- ggdid(capital_pregnancy_controls_2019_agg.es, title = "capital_pregnancy_controls_2019")
ggsave("capital_pregnancy_controls_2019.png", f3, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                       tname = "year",
                                       idname = "muni_id",
                                       gname = "G",
                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                       data = treatment_combined_hosp_2019,
                                       #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019)
combined_toxic_controls_2019_agg.es <- aggte(combined_toxic_controls_2019, type = "dynamic",
                                             min_e = -10, max_e = 10,
                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_agg.es)
f4 <- ggdid(combined_toxic_controls_2019_agg.es, title = "combined_toxic_controls_2019")
ggsave("combined_toxic_controls_2019.png", f4, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                            tname = "year",
                                            idname = "muni_id",
                                            gname = "G",
                                            xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                            data = treatment_combined_hosp_2019,
                                            #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019)
combined_intestinal_controls_2019_agg.es <- aggte(combined_intestinal_controls_2019, type = "dynamic",
                                                  min_e = -10, max_e = 10,
                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_agg.es)
f5 <- ggdid(combined_intestinal_controls_2019_agg.es, title = "combined_intestinal_controls_2019")
ggsave("combined_intestinal_controls_2019.png", f5, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_combined_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019)
combined_pregnancy_controls_2019_agg.es <- aggte(combined_pregnancy_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_agg.es)
f6 <- ggdid(combined_pregnancy_controls_2019_agg.es, title = "combined_pregnancy_controls_2019")
ggsave("combined_pregnancy_controls_2019.png", f6, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019 <- att_gt(yname = "toxic_effects_pc",
                                      tname = "year",
                                      idname = "muni_id",
                                      gname = "G",
                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                      data = treatment_overlap_hosp_2019,
                                      #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019)
overlap_toxic_controls_2019_agg.es <- aggte(overlap_toxic_controls_2019, type = "dynamic",
                                            min_e = -10, max_e = 10,
                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_agg.es)
f7 <- ggdid(overlap_toxic_controls_2019_agg.es, title = "overlap_toxic_controls_2019")
ggsave("overlap_toxic_controls_2019.png", f7, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019 <- att_gt(yname = "intestinal_diseases_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_overlap_hosp_2019,
                                           #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019)
overlap_intestinal_controls_2019_agg.es <- aggte(overlap_intestinal_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_agg.es)
f8 <- ggdid(overlap_intestinal_controls_2019_agg.es, title = "overlap_intestinal_controls_2019")
ggsave("overlap_intestinal_controls_2019.png", f8, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019 <- att_gt(yname = "pregnancy_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_overlap_hosp_2019,
                                          #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019)
overlap_pregnancy_controls_2019_agg.es <- aggte(overlap_pregnancy_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_agg.es)
f9 <- ggdid(overlap_pregnancy_controls_2019_agg.es, title = "overlap_pregnancy_controls_2019")
ggsave("overlap_pregnancy_controls_2019.png", f9, width = 8, height = 5, dpi = 300)


#capital
capital_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_capital_hosp_2019_art,
)
summary(capital_toxic_controls_2019_art)
capital_toxic_controls_2019_art_agg.es <- aggte(capital_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_art_agg.es)
f10 <- ggdid(capital_toxic_controls_2019_art_agg.es, title = "capital_toxic_controls_2019_art")
ggsave("capital_toxic_controls_2019_art.png", f10, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_capital_hosp_2019_art,
)
summary(capital_intestinal_controls_2019_art)
capital_intestinal_controls_2019_art_agg.es <- aggte(capital_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_agg.es)
f11 <- ggdid(capital_intestinal_controls_2019_art_agg.es, title = "capital_intestinal_controls_2019_art")
ggsave("capital_intestinal_controls_2019_art.png", f11, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_capital_hosp_2019_art,
)
summary(capital_pregnancy_controls_2019_art)
capital_pregnancy_controls_2019_art_agg.es <- aggte(capital_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_agg.es)
f12 <- ggdid(capital_pregnancy_controls_2019_art_agg.es, title = "capital_pregnancy_controls_2019_art")
ggsave("capital_pregnancy_controls_2019_art.png", f12, width = 8, height = 5, dpi = 300)


#combined
combined_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_combined_hosp_2019_art,
)
summary(combined_toxic_controls_2019_art)
combined_toxic_controls_2019_art_agg.es <- aggte(combined_toxic_controls_2019_art, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_art_agg.es)
f13 <- ggdid(combined_toxic_controls_2019_art_agg.es, title = "combined_toxic_controls_2019_art")
ggsave("combined_toxic_controls_2019_art.png", f13, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_combined_hosp_2019_art,
)
summary(combined_intestinal_controls_2019_art)
combined_intestinal_controls_2019_art_agg.es <- aggte(combined_intestinal_controls_2019_art, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_agg.es)
f14 <- ggdid(combined_intestinal_controls_2019_art_agg.es, title = "combined_intestinal_controls_2019_art")
ggsave("combined_intestinal_controls_2019_art.png", f14, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_combined_hosp_2019_art,
)
summary(combined_pregnancy_controls_2019_art)
combined_pregnancy_controls_2019_art_agg.es <- aggte(combined_pregnancy_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_agg.es)
f15 <- ggdid(combined_pregnancy_controls_2019_art_agg.es, title = "combined_pregnancy_controls_2019_art")
ggsave("combined_pregnancy_controls_2019_art.png", f15, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_overlap_hosp_2019_art,
)
summary(overlap_toxic_controls_2019_art)
overlap_toxic_controls_2019_art_agg.es <- aggte(overlap_toxic_controls_2019_art, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_agg.es)
f16 <- ggdid(overlap_toxic_controls_2019_art_agg.es, title = "overlap_toxic_controls_2019_art")
ggsave("overlap_toxic_controls_2019_art.png", f16, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_overlap_hosp_2019_art,
)
summary(overlap_intestinal_controls_2019_art)
overlap_intestinal_controls_2019_art_agg.es <- aggte(overlap_intestinal_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_agg.es)
f17 <- ggdid(overlap_intestinal_controls_2019_art_agg.es, title = "overlap_intestinal_controls_2019_art")
ggsave("overlap_intestinal_controls_2019_art.png", f17, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_overlap_hosp_2019_art,
)
summary(overlap_pregnancy_controls_2019_art)
overlap_pregnancy_controls_2019_art_agg.es <- aggte(overlap_pregnancy_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_agg.es)
f18 <- ggdid(overlap_pregnancy_controls_2019_art_agg.es, title = "overlap_pregnancy_controls_2019_art")
ggsave("overlap_pregnancy_controls_2019_art.png", f18, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_capital_hosp_2019_art_share,
)
summary(capital_toxic_controls_2019_art_share)
capital_toxic_controls_2019_art_share_agg.es <- aggte(capital_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_agg.es)
f19 <- ggdid(capital_toxic_controls_2019_art_share_agg.es, title = "capital_toxic_controls_2019_art_share")
ggsave("capital_toxic_controls_2019_art_share.png", f19, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_capital_hosp_2019_art_share,
)
summary(capital_intestinal_controls_2019_art_share)
capital_intestinal_controls_2019_art_share_agg.es <- aggte(capital_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_agg.es)
f20 <- ggdid(capital_intestinal_controls_2019_art_share_agg.es, title = "capital_intestinal_controls_2019_art_share")
ggsave("capital_intestinal_controls_2019_art_share.png", f20, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_capital_hosp_2019_art_share,
)
summary(capital_pregnancy_controls_2019_art_share)
capital_pregnancy_controls_2019_art_share_agg.es <- aggte(capital_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_agg.es)
f21 <- ggdid(capital_pregnancy_controls_2019_art_share_agg.es, title = "capital_pregnancy_controls_2019_art_share")
ggsave("capital_pregnancy_controls_2019_art_share.png", f21, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_combined_hosp_2019_art_share,
)
summary(combined_toxic_controls_2019_art_share)
combined_toxic_controls_2019_art_share_agg.es <- aggte(combined_toxic_controls_2019_art_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_agg.es)
f22 <- ggdid(combined_toxic_controls_2019_art_share_agg.es, title = "combined_toxic_controls_2019_art_share")
ggsave("combined_toxic_controls_2019_art_share.png", f22, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                      data = treatment_combined_hosp_2019_art_share,
)
summary(combined_intestinal_controls_2019_art_share)
combined_intestinal_controls_2019_art_share_agg.es <- aggte(combined_intestinal_controls_2019_art_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_agg.es)
f23 <- ggdid(combined_intestinal_controls_2019_art_share_agg.es, title = "combined_intestinal_controls_2019_art_share")
ggsave("combined_intestinal_controls_2019_art_share.png", f23, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_combined_hosp_2019_art_share,
)
summary(combined_pregnancy_controls_2019_art_share)
combined_pregnancy_controls_2019_art_share_agg.es <- aggte(combined_pregnancy_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_agg.es)
f24 <- ggdid(combined_pregnancy_controls_2019_art_share_agg.es, title = "combined_pregnancy_controls_2019_art_share")
ggsave("combined_pregnancy_controls_2019_art_share.png", f24, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_art_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_toxic_controls_2019_art_share)
overlap_toxic_controls_2019_art_share_agg.es <- aggte(overlap_toxic_controls_2019_art_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_agg.es)
f25 <- ggdid(overlap_toxic_controls_2019_art_share_agg.es, title = "overlap_toxic_controls_2019_art_share")
ggsave("overlap_toxic_controls_2019_art_share.png", f25, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_intestinal_controls_2019_art_share)
overlap_intestinal_controls_2019_art_share_agg.es <- aggte(overlap_intestinal_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_agg.es)
f26 <- ggdid(overlap_intestinal_controls_2019_art_share_agg.es, title = "overlap_intestinal_controls_2019_art_share")
ggsave("overlap_intestinal_controls_2019_art_share.png", f26, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_overlap_hosp_2019_art_share,
)
summary(overlap_pregnancy_controls_2019_art_share)
overlap_pregnancy_controls_2019_art_share_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_agg.es)
f27 <- ggdid(overlap_pregnancy_controls_2019_art_share_agg.es, title = "overlap_pregnancy_controls_2019_art_share")
ggsave("overlap_pregnancy_controls_2019_art_share.png", f27, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_capital_hosp_2019_ind,
)
summary(capital_toxic_controls_2019_ind)
capital_toxic_controls_2019_ind_agg.es <- aggte(capital_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_agg.es)
f28 <- ggdid(capital_toxic_controls_2019_ind_agg.es, title = "capital_toxic_controls_2019_ind")
ggsave("capital_toxic_controls_2019_ind.png", f28, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_capital_hosp_2019_ind,
)
summary(capital_intestinal_controls_2019_ind)
capital_intestinal_controls_2019_ind_agg.es <- aggte(capital_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_agg.es)
f29 <- ggdid(capital_intestinal_controls_2019_ind_agg.es, title = "capital_intestinal_controls_2019_ind")
ggsave("capital_intestinal_controls_2019_ind.png", f29, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_capital_hosp_2019_ind,
)
summary(capital_pregnancy_controls_2019_ind)
capital_pregnancy_controls_2019_ind_agg.es <- aggte(capital_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_agg.es)
f30 <- ggdid(capital_pregnancy_controls_2019_ind_agg.es, title = "capital_pregnancy_controls_2019_ind")
ggsave("capital_pregnancy_controls_2019_ind.png", f30, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_combined_hosp_2019_ind,
)
summary(combined_toxic_controls_2019_ind)
combined_toxic_controls_2019_ind_agg.es <- aggte(combined_toxic_controls_2019_ind, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_agg.es)
f31 <- ggdid(combined_toxic_controls_2019_ind_agg.es, title = "combined_toxic_controls_2019_ind")
ggsave("combined_toxic_controls_2019_ind.png", f31, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_combined_hosp_2019_ind,
)
summary(combined_intestinal_controls_2019_ind)
combined_intestinal_controls_2019_ind_agg.es <- aggte(combined_intestinal_controls_2019_ind, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_agg.es)
f32 <- ggdid(combined_intestinal_controls_2019_ind_agg.es, title = "combined_intestinal_controls_2019_ind")
ggsave("combined_intestinal_controls_2019_ind.png", f32, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_combined_hosp_2019_ind,
)
summary(combined_pregnancy_controls_2019_ind)
combined_pregnancy_controls_2019_ind_agg.es <- aggte(combined_pregnancy_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_agg.es)
f33 <- ggdid(combined_pregnancy_controls_2019_ind_agg.es, title = "combined_pregnancy_controls_2019_ind")
ggsave("combined_pregnancy_controls_2019_ind.png", f33, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind <- att_gt(yname = "toxic_effects_pc",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_toxic_controls_2019_ind)
overlap_toxic_controls_2019_ind_agg.es <- aggte(overlap_toxic_controls_2019_ind, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_agg.es)
f34 <- ggdid(overlap_toxic_controls_2019_ind_agg.es, title = "overlap_toxic_controls_2019_ind")
ggsave("overlap_toxic_controls_2019_ind.png", f34, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind <- att_gt(yname = "intestinal_diseases_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_intestinal_controls_2019_ind)
overlap_intestinal_controls_2019_ind_agg.es <- aggte(overlap_intestinal_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_agg.es)
f35 <- ggdid(overlap_intestinal_controls_2019_ind_agg.es, title = "overlap_intestinal_controls_2019_ind")
ggsave("overlap_intestinal_controls_2019_ind.png", f35, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind <- att_gt(yname = "pregnancy_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_overlap_hosp_2019_ind,
)
summary(overlap_pregnancy_controls_2019_ind)
overlap_pregnancy_controls_2019_ind_agg.es <- aggte(overlap_pregnancy_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_agg.es)
f36 <- ggdid(overlap_pregnancy_controls_2019_ind_agg.es, title = "overlap_pregnancy_controls_2019_ind")
ggsave("overlap_pregnancy_controls_2019_ind.png", f36, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_toxic_controls_2019_ind_share)
capital_toxic_controls_2019_ind_share_agg.es <- aggte(capital_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_agg.es)
f37 <- ggdid(capital_toxic_controls_2019_ind_share_agg.es, title = "capital_toxic_controls_2019_ind_share")
ggsave("capital_toxic_controls_2019_ind_share.png", f37, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_intestinal_controls_2019_ind_share)
capital_intestinal_controls_2019_ind_share_agg.es <- aggte(capital_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_agg.es)
f38 <-ggdid(capital_intestinal_controls_2019_ind_share_agg.es, title = "capital_intestinal_controls_2019_ind_share")
ggsave("capital_intestinal_controls_2019_ind_share.png", f38, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_capital_hosp_2019_ind_share,
)
summary(capital_pregnancy_controls_2019_ind_share)
capital_pregnancy_controls_2019_ind_share_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_agg.es)
f39 <- ggdid(capital_pregnancy_controls_2019_ind_share_agg.es, title = "capital_pregnancy_controls_2019_ind_share")
ggsave("capital_pregnancy_controls_2019_ind_share.png", f39, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_toxic_controls_2019_ind_share)
combined_toxic_controls_2019_ind_share_agg.es <- aggte(combined_toxic_controls_2019_ind_share, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_agg.es)
f40 <- ggdid(combined_toxic_controls_2019_ind_share_agg.es, title = "combined_toxic_controls_2019_ind_share")
ggsave("combined_toxic_controls_2019_ind_share.png", f40, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                      data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_intestinal_controls_2019_ind_share)
combined_intestinal_controls_2019_ind_share_agg.es <- aggte(combined_intestinal_controls_2019_ind_share, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_agg.es)
f41 <- ggdid(combined_intestinal_controls_2019_ind_share_agg.es, title = "combined_intestinal_controls_2019_ind_share")
ggsave("combined_intestinal_controls_2019_ind_share.png", f41, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_combined_hosp_2019_ind_share,
)
summary(combined_pregnancy_controls_2019_ind_share)
combined_pregnancy_controls_2019_ind_share_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_agg.es)
f42 <- ggdid(combined_pregnancy_controls_2019_ind_share_agg.es, title = "combined_pregnancy_controls_2019_ind_share")
ggsave("combined_pregnancy_controls_2019_ind_share.png", f42, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_ind_share <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_toxic_controls_2019_ind_share)
overlap_toxic_controls_2019_ind_share_agg.es <- aggte(overlap_toxic_controls_2019_ind_share, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_agg.es)
f43 <- ggdid(overlap_toxic_controls_2019_ind_share_agg.es, title = "overlap_toxic_controls_2019_ind_share")
ggsave("overlap_toxic_controls_2019_ind_share.png", f43, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share <- att_gt(yname = "intestinal_diseases_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_intestinal_controls_2019_ind_share)
overlap_intestinal_controls_2019_ind_share_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_agg.es)
f44 <- ggdid(overlap_intestinal_controls_2019_ind_share_agg.es, title = "overlap_intestinal_controls_2019_ind_share")
ggsave("overlap_intestinal_controls_2019_ind_share.png", f44, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share <- att_gt(yname = "pregnancy_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_overlap_hosp_2019_ind_share,
)
summary(overlap_pregnancy_controls_2019_ind_share)
overlap_pregnancy_controls_2019_ind_share_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_agg.es)
f45 <- ggdid(overlap_pregnancy_controls_2019_ind_share_agg.es, title = "overlap_pregnancy_controls_2019_ind_share")
ggsave("overlap_pregnancy_controls_2019_ind_share.png", f45, width = 8, height = 5, dpi = 300)

#capital
capital_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                             data = treatment_capital_hosp_2019_amazon,
)
summary(capital_toxic_controls_2019_amazon)
capital_toxic_controls_2019_amazon_agg.es <- aggte(capital_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_agg.es)
f46 <- ggdid(capital_toxic_controls_2019_amazon_agg.es, title = "capital_toxic_controls_2019_amazon")
ggsave("capital_toxic_controls_2019_amazon.png", f46, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                  data = treatment_capital_hosp_2019_amazon,
)
summary(capital_intestinal_controls_2019_amazon)
capital_intestinal_controls_2019_amazon_agg.es <- aggte(capital_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_agg.es)
f47 <- ggdid(capital_intestinal_controls_2019_amazon_agg.es, title = "capital_intestinal_controls_2019_amazon")
ggsave("capital_intestinal_controls_2019_amazon.png", f47, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_capital_hosp_2019_amazon,
)
summary(capital_pregnancy_controls_2019_amazon)
capital_pregnancy_controls_2019_amazon_agg.es <- aggte(capital_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_agg.es)
f48 <- ggdid(capital_pregnancy_controls_2019_amazon_agg.es , title = "capital_pregnancy_controls_2019_amazon")
ggsave("capital_pregnancy_controls_2019_amazon.png", f48, width = 8, height = 5, dpi = 300)

#combined
combined_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_combined_hosp_2019_amazon,
)
summary(combined_toxic_controls_2019_amazon)
combined_toxic_controls_2019_amazon_agg.es <- aggte(combined_toxic_controls_2019_amazon, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_agg.es)
f49 <- ggdid(combined_toxic_controls_2019_amazon_agg.es, title = "combined_toxic_controls_2019_amazon")
ggsave("combined_toxic_controls_2019_amazon.png", f49, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_combined_hosp_2019_amazon,
)
summary(combined_intestinal_controls_2019_amazon)
combined_intestinal_controls_2019_amazon_agg.es <- aggte(combined_intestinal_controls_2019_amazon, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_agg.es)
f50 <- ggdid(combined_intestinal_controls_2019_amazon_agg.es, title = "combined_intestinal_controls_2019_amazon")
ggsave("combined_intestinal_controls_2019_amazon.png", f50, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                  data = treatment_combined_hosp_2019_amazon,
)
summary(combined_pregnancy_controls_2019_amazon)
combined_pregnancy_controls_2019_amazon_agg.es <- aggte(combined_pregnancy_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_agg.es)
f51 <- ggdid(combined_pregnancy_controls_2019_amazon_agg.es, title = "combined_pregnancy_controls_2019_amazon")
ggsave("combined_pregnancy_controls_2019_amazon.png", f51, width = 8, height = 5, dpi = 300)

#overlap
overlap_toxic_controls_2019_amazon <- att_gt(yname = "toxic_effects_pc",
                                             tname = "year",
                                             idname = "muni_id",
                                             gname = "G",
                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                             data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_toxic_controls_2019_amazon)
overlap_toxic_controls_2019_amazon_agg.es <- aggte(overlap_toxic_controls_2019_amazon, type = "dynamic",
                                                   min_e = -10, max_e = 10,
                                                   na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_agg.es)
f52 <- ggdid(overlap_toxic_controls_2019_amazon_agg.es, title = "overlap_toxic_controls_2019_amazon")
ggsave("overlap_toxic_controls_2019_amazon.png", f52, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon <- att_gt(yname = "intestinal_diseases_pc",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                  data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_intestinal_controls_2019_amazon)
overlap_intestinal_controls_2019_amazon_agg.es <- aggte(overlap_intestinal_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_agg.es)
f53 <- ggdid(overlap_intestinal_controls_2019_amazon_agg.es, title = "overlap_intestinal_controls_2019_amazon")
ggsave("overlap_intestinal_controls_2019_amazon.png", f53, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon <- att_gt(yname = "pregnancy_pc",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_overlap_hosp_2019_amazon,
)
summary(overlap_pregnancy_controls_2019_amazon)
overlap_pregnancy_controls_2019_amazon_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_agg.es)
f54 <- ggdid(overlap_pregnancy_controls_2019_amazon_agg.es, title = "overlap_pregnancy_controls_2019_amazon")
ggsave("overlap_pregnancy_controls_2019_amazon.png", f54, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_capital_child_mort_2019,
)
summary(capital_child_0_2_controls_2019)
capital_child_0_2_controls_2019_agg.es <- aggte(capital_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_agg.es)
f55 <- ggdid(capital_child_0_2_controls_2019_agg.es, title = "capital_child_0_2_controls_2019")
ggsave("capital_child_0_2_controls_2019.png", f55, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                           tname = "year",
                                           idname = "muni_id",
                                           gname = "G",
                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                           data = treatment_combined_child_mort_2019,
)
summary(combined_child_0_2_controls_2019)
combined_child_0_2_controls_2019_agg.es <- aggte(combined_child_0_2_controls_2019, type = "dynamic",
                                                 min_e = -10, max_e = 10,
                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_agg.es)
f56 <- ggdid(combined_child_0_2_controls_2019_agg.es, title = "combined_child_0_2_controls_2019")
ggsave("combined_child_0_2_controls_2019.png", f56, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019 <- att_gt(yname = "child_mort_pc_0_2",
                                          tname = "year",
                                          idname = "muni_id",
                                          gname = "G",
                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                          data = treatment_overlap_child_mort_2019,
)
summary(overlap_child_0_2_controls_2019)
overlap_child_0_2_controls_2019_agg.es <- aggte(overlap_child_0_2_controls_2019, type = "dynamic",
                                                min_e = -10, max_e = 10,
                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_agg.es)
f57 <- ggdid(overlap_child_0_2_controls_2019_agg.es, title = "overlap_child_0_2_controls_2019")
ggsave("overlap_child_0_2_controls_2019.png", f57, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_capital_child_mort_2019_art,
)
summary(capital_child_0_2_controls_2019_art)
capital_child_0_2_controls_2019_art_agg.es <- aggte(capital_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_agg.es)
f58 <- ggdid(capital_child_0_2_controls_2019_art_agg.es, title = "capital_child_0_2_controls_2019_art")
ggsave("capital_child_0_2_controls_2019_art.png", f58, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_combined_child_mort_2019_art,
)
summary(combined_child_0_2_controls_2019_art)
combined_child_0_2_controls_2019_art_agg.es <- aggte(combined_child_0_2_controls_2019_art, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_agg.es)
f59 <- ggdid(combined_child_0_2_controls_2019_art_agg.es, title = "combined_child_0_2_controls_2019_art")
ggsave("combined_child_0_2_controls_2019_art.png", f59, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_overlap_child_mort_2019_art,
)
summary(overlap_child_0_2_controls_2019_art)
overlap_child_0_2_controls_2019_art_agg.es <- aggte(overlap_child_0_2_controls_2019_art, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_agg.es)
f60 <- ggdid(overlap_child_0_2_controls_2019_art_agg.es, title = "overlap_child_0_2_controls_2019_art")
ggsave("overlap_child_0_2_controls_2019_art.png", f60, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_capital_child_mort_2019_art_share,
)
summary(capital_child_0_2_controls_2019_art_share)
capital_child_0_2_controls_2019_art_share_agg.es <- aggte(capital_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_agg.es)
f61 <- ggdid(capital_child_0_2_controls_2019_art_share_agg.es, title = "capital_child_0_2_controls_2019_art_share")
ggsave("capital_child_0_2_controls_2019_art_share.png", f61, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_combined_child_mort_2019_art_share,
)
summary(combined_child_0_2_controls_2019_art_share)
combined_child_0_2_controls_2019_art_share_agg.es <- aggte(combined_child_0_2_controls_2019_art_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_agg.es)
f62 <- ggdid(combined_child_0_2_controls_2019_art_share_agg.es, title = "combined_child_0_2_controls_2019_art_share")
ggsave("combined_child_0_2_controls_2019_art_share.png", f62, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_overlap_child_mort_2019_art_share,
)
summary(overlap_child_0_2_controls_2019_art_share)
overlap_child_0_2_controls_2019_art_share_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_agg.es)
f63 <- ggdid(overlap_child_0_2_controls_2019_art_share_agg.es, title = "overlap_child_0_2_controls_2019_art_share")
ggsave("overlap_child_0_2_controls_2019_art_share.png", f63, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_capital_child_mort_2019_ind,
)
summary(capital_child_0_2_controls_2019_ind)
capital_child_0_2_controls_2019_ind_agg.es <- aggte(capital_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_agg.es)
f64 <- ggdid(capital_child_0_2_controls_2019_ind_agg.es, title = "capital_child_0_2_controls_2019_ind")
ggsave("capital_child_0_2_controls_2019_ind.png", f64, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_combined_child_mort_2019_ind,
)
summary(combined_child_0_2_controls_2019_ind)
combined_child_0_2_controls_2019_ind_agg.es <- aggte(combined_child_0_2_controls_2019_ind, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_agg.es)
f65 <- ggdid(combined_child_0_2_controls_2019_ind_agg.es, title = "combined_child_0_2_controls_2019_ind")
ggsave("combined_child_0_2_controls_2019_ind.png", f65, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind <- att_gt(yname = "child_mort_pc_0_2",
                                              tname = "year",
                                              idname = "muni_id",
                                              gname = "G",
                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                              data = treatment_overlap_child_mort_2019_ind,
)
summary(overlap_child_0_2_controls_2019_ind)
overlap_child_0_2_controls_2019_ind_agg.es <- aggte(overlap_child_0_2_controls_2019_ind, type = "dynamic",
                                                    min_e = -10, max_e = 10,
                                                    na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_agg.es)
f66 <- ggdid(overlap_child_0_2_controls_2019_ind_agg.es, title = "overlap_child_0_2_controls_2019_ind")
ggsave("overlap_child_0_2_controls_2019_ind.png", f66, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_capital_child_mort_2019_ind_share,
)
summary(capital_child_0_2_controls_2019_ind_share)
capital_child_0_2_controls_2019_ind_share_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_agg.es)
f67 <- ggdid(capital_child_0_2_controls_2019_ind_share_agg.es, title = "capital_child_0_2_controls_2019_ind_share")
ggsave("capital_child_0_2_controls_2019_ind_share.png", f67, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_combined_child_mort_2019_ind_share,
)
summary(combined_child_0_2_controls_2019_ind_share)
combined_child_0_2_controls_2019_ind_share_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_agg.es)
f68 <- ggdid(combined_child_0_2_controls_2019_ind_share_agg.es, title = "combined_child_0_2_controls_2019_ind_share")
ggsave("combined_child_0_2_controls_2019_ind_share.png", f68, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_overlap_child_mort_2019_ind_share,
)
summary(overlap_child_0_2_controls_2019_ind_share)
overlap_child_0_2_controls_2019_ind_share_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_agg.es)
f69 <- ggdid(overlap_child_0_2_controls_2019_ind_share_agg.es, title = "overlap_child_0_2_controls_2019_ind_share")
ggsave("overlap_child_0_2_controls_2019_ind_share.png", f69, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_capital_child_mort_2019_amazon,
)
summary(capital_child_0_2_controls_2019_amazon)
capital_child_0_2_controls_2019_amazon_agg.es <- aggte(capital_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_agg.es)
f70 <- ggdid(capital_child_0_2_controls_2019_amazon_agg.es, title = "capital_child_0_2_controls_2019_amazon")
ggsave("capital_child_0_2_controls_2019_amazon.png", f70, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                  tname = "year",
                                                  idname = "muni_id",
                                                  gname = "G",
                                                  xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                  data = treatment_combined_child_mort_2019_amazon,
)
summary(combined_child_0_2_controls_2019_amazon)
combined_child_0_2_controls_2019_amazon_agg.es <- aggte(combined_child_0_2_controls_2019_amazon, type = "dynamic",
                                                        min_e = -10, max_e = 10,
                                                        na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_agg.es)
f71 <- ggdid(combined_child_0_2_controls_2019_amazon_agg.es, title = "combined_child_0_2_controls_2019_amazon")
ggsave("combined_child_0_2_controls_2019_amazon.png", f71, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon <- att_gt(yname = "child_mort_pc_0_2",
                                                 tname = "year",
                                                 idname = "muni_id",
                                                 gname = "G",
                                                 xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                 data = treatment_overlap_child_mort_2019_amazon,
)
summary(overlap_child_0_2_controls_2019_amazon)
overlap_child_0_2_controls_2019_amazon_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon, type = "dynamic",
                                                       min_e = -10, max_e = 10,
                                                       na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_agg.es)
f72 <- ggdid(overlap_child_0_2_controls_2019_amazon_agg.es, title = "overlap_child_0_2_controls_2019_amazon")
ggsave("overlap_child_0_2_controls_2019_amazon.png", f72, width = 8, height = 5, dpi = 300)


### with weights
setwd("/home/francesca/brazil_mining/mining_health/figures/mining_ha_median/stagged_did_2003_weights/")
#toxic effects
capital_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_capital_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(capital_toxic_controls_2019_weighted)
capital_toxic_controls_2019_weighted_agg.es <- aggte(capital_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(capital_toxic_controls_2019_weighted_agg.es)
f1a <- ggdid(capital_toxic_controls_2019_weighted_agg.es, title = "capital_toxic_controls_2019_weighted")
ggsave("capital_toxic_controls_2019_weighted.png", f1a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                tname = "year",
                                                idname = "muni_id",
                                                gname = "G",
                                                xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                data = treatment_combined_hosp_2019,
                                                weightsname = "pop"
                                                #control_group = "not_yet_treated" #?
)
summary(combined_toxic_controls_2019_weighted)
combined_toxic_controls_2019_weighted_agg.es <- aggte(combined_toxic_controls_2019_weighted, type = "dynamic",
                                                      min_e = -10, max_e = 10,
                                                      na.rm = TRUE)
summary(combined_toxic_controls_2019_weighted_agg.es)
f2a <- ggdid(combined_toxic_controls_2019_weighted_agg.es, title = "combined_toxic_controls_2019_weighted")
ggsave("combined_toxic_controls_2019_weighted.png", f2a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                               tname = "year",
                                               idname = "muni_id",
                                               gname = "G",
                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                               data = treatment_overlap_hosp_2019,
                                               weightsname = "pop"
                                               #control_group = "not_yet_treated" #?
)
summary(overlap_toxic_controls_2019_weighted)
overlap_toxic_controls_2019_weighted_agg.es <- aggte(overlap_toxic_controls_2019_weighted, type = "dynamic",
                                                     min_e = -10, max_e = 10,
                                                     na.rm = TRUE)
summary(overlap_toxic_controls_2019_weighted_agg.es)
f3a <- ggdid(overlap_toxic_controls_2019_weighted_agg.es, title = "overlap_toxic_controls_2019_weighted")
ggsave("overlap_toxic_controls_2019_weighted.png", f3a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_capital_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_weighted)
capital_toxic_controls_2019_art_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_art_weighted_agg.es)
f4a <- ggdid(capital_toxic_controls_2019_art_weighted_agg.es, title = "capital_toxic_controls_2019_art_weighted")
ggsave("capital_toxic_controls_2019_art_weighted.png", f4a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_combined_hosp_2019_art,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_weighted)
combined_toxic_controls_2019_art_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_art_weighted_agg.es)
f5a <- ggdid(combined_toxic_controls_2019_art_weighted_agg.es, title = "combined_toxic_controls_2019_art_weighted")
ggsave("combined_toxic_controls_2019_art_weighted.png", f5a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_overlap_hosp_2019_art,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_weighted)
overlap_toxic_controls_2019_art_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_weighted_agg.es)
f6a <- ggdid(overlap_toxic_controls_2019_art_weighted_agg.es, title = "overlap_toxic_controls_2019_art_weighted")
ggsave("overlap_toxic_controls_2019_art_weighted.png", f6a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_capital_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_art_share_weighted)
capital_toxic_controls_2019_art_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_art_share_weighted_agg.es)
f7a <- ggdid(capital_toxic_controls_2019_art_share_weighted_agg.es, title = "capital_toxic_controls_2019_art_share_weighted")
ggsave("capital_toxic_controls_2019_art_share_weighted.png", f7a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_combined_hosp_2019_art_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_art_share_weighted)
combined_toxic_controls_2019_art_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_art_share_weighted_agg.es)
f8a <- ggdid(combined_toxic_controls_2019_art_share_weighted_agg.es, title = "combined_toxic_controls_2019_art_share_weighted")
ggsave("combined_toxic_controls_2019_art_share_weighted.png", f8a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_overlap_hosp_2019_art_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_art_share_weighted)
overlap_toxic_controls_2019_art_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_art_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_art_share_weighted_agg.es)
f9a <- ggdid(overlap_toxic_controls_2019_art_share_weighted_agg.es, title = "overlap_toxic_controls_2019_art_share_weighted")
ggsave("overlap_toxic_controls_2019_art_share_weighted.png", f9a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_capital_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_weighted)
capital_toxic_controls_2019_ind_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_weighted_agg.es)
f10a <-ggdid(capital_toxic_controls_2019_ind_weighted_agg.es, title = "capital_toxic_controls_2019_ind_weighted")
ggsave("capital_toxic_controls_2019_ind_weighted.png", f10a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_combined_hosp_2019_ind,
                                                    weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_weighted)
combined_toxic_controls_2019_ind_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_weighted_agg.es)
f11a <-ggdid(combined_toxic_controls_2019_ind_weighted_agg.es, title = "combined_toxic_controls_2019_ind_weighted")
ggsave("combined_toxic_controls_2019_ind_weighted.png", f11a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_overlap_hosp_2019_ind,
                                                   weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_weighted)
overlap_toxic_controls_2019_ind_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_weighted_agg.es)
f12a <- ggdid(overlap_toxic_controls_2019_ind_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_weighted")
ggsave("overlap_toxic_controls_2019_ind_weighted.png", f12a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_capital_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(capital_toxic_controls_2019_ind_share_weighted)
capital_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(capital_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(capital_toxic_controls_2019_ind_share_weighted_agg.es)
f13a <- ggdid(capital_toxic_controls_2019_ind_share_weighted_agg.es, title = "capital_toxic_controls_2019_ind_share_weighted")
ggsave("capital_toxic_controls_2019_ind_share_weighted.png", f13a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_combined_hosp_2019_ind_share,
                                                          weightsname = "pop"
)
summary(combined_toxic_controls_2019_ind_share_weighted)
combined_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(combined_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(combined_toxic_controls_2019_ind_share_weighted_agg.es)
f14a <- ggdid(combined_toxic_controls_2019_ind_share_weighted_agg.es, title = "combined_toxic_controls_2019_ind_share_weighted")
ggsave("combined_toxic_controls_2019_ind_share_weighted.png", f14a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_overlap_hosp_2019_ind_share,
                                                         weightsname = "pop"
)
summary(overlap_toxic_controls_2019_ind_share_weighted)
overlap_toxic_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_toxic_controls_2019_ind_share_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(overlap_toxic_controls_2019_ind_share_weighted_agg.es)
f15a <- ggdid(overlap_toxic_controls_2019_ind_share_weighted_agg.es, title = "overlap_toxic_controls_2019_ind_share_weighted")
ggsave("overlap_toxic_controls_2019_ind_share_weighted.png", f15a, width = 8, height = 5, dpi = 300)

capital_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                      data = treatment_capital_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(capital_toxic_controls_2019_amazon_weighted)
capital_toxic_controls_2019_amazon_weighted_agg.es <- aggte(capital_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(capital_toxic_controls_2019_amazon_weighted_agg.es)
f16a <- ggdid(capital_toxic_controls_2019_amazon_weighted_agg.es, title = "capital_toxic_controls_2019_amazon_weighted")
ggsave("capital_toxic_controls_2019_amazon_weighted.png", f16a, width = 8, height = 5, dpi = 300)

combined_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_combined_hosp_2019_amazon,
                                                       weightsname = "pop"
)
summary(combined_toxic_controls_2019_amazon_weighted)
combined_toxic_controls_2019_amazon_weighted_agg.es <- aggte(combined_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(combined_toxic_controls_2019_amazon_weighted_agg.es)
f17a <- ggdid(combined_toxic_controls_2019_amazon_weighted_agg.es, title = "combined_toxic_controls_2019_amazon_weighted")
ggsave("combined_toxic_controls_2019_amazon_weighted.png", f17a, width = 8, height = 5, dpi = 300)

overlap_toxic_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                      tname = "year",
                                                      idname = "muni_id",
                                                      gname = "G",
                                                      xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                      data = treatment_overlap_hosp_2019_amazon,
                                                      weightsname = "pop"
)
summary(overlap_toxic_controls_2019_amazon_weighted)
overlap_toxic_controls_2019_amazon_weighted_agg.es <- aggte(overlap_toxic_controls_2019_amazon_weighted, type = "dynamic",
                                                            min_e = -10, max_e = 10,
                                                            na.rm = TRUE)
summary(overlap_toxic_controls_2019_amazon_weighted_agg.es)
f18a <- ggdid(overlap_toxic_controls_2019_amazon_weighted_agg.es, title = "overlap_toxic_controls_2019_amazon_weighted")
ggsave("overlap_toxic_controls_2019_amazon_weighted.png", f18a, width = 8, height = 5, dpi = 300)

#intestinal
capital_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_capital_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(capital_intestinal_controls_2019_weighted)
capital_intestinal_controls_2019_weighted_agg.es <- aggte(capital_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(capital_intestinal_controls_2019_weighted_agg.es)
f19a <- ggdid(capital_intestinal_controls_2019_weighted_agg.es, title = "capital_intestinal_controls_2019_weighted")
ggsave("capital_intestinal_controls_2019_weighted.png", f19a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                     tname = "year",
                                                     idname = "muni_id",
                                                     gname = "G",
                                                     xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                     data = treatment_combined_hosp_2019,
                                                     weightsname = "pop"
                                                     #control_group = "not_yet_treated" #?
)
summary(combined_intestinal_controls_2019_weighted)
combined_intestinal_controls_2019_weighted_agg.es <- aggte(combined_intestinal_controls_2019_weighted, type = "dynamic",
                                                           min_e = -10, max_e = 10,
                                                           na.rm = TRUE)
summary(combined_intestinal_controls_2019_weighted_agg.es)
f20a <- ggdid(combined_intestinal_controls_2019_weighted_agg.es, title = "combined_intestinal_controls_2019_weighted")
ggsave("combined_intestinal_controls_2019_weighted.png", f20a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_overlap_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(overlap_intestinal_controls_2019_weighted)
overlap_intestinal_controls_2019_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(overlap_intestinal_controls_2019_weighted_agg.es)
f21a <- ggdid(overlap_intestinal_controls_2019_weighted_agg.es, title = "overlap_intestinal_controls_2019_weighted")
ggsave("overlap_intestinal_controls_2019_weighted.png", f21a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_capital_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_weighted)
capital_intestinal_controls_2019_art_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_weighted_agg.es)
f22a <- ggdid(capital_intestinal_controls_2019_art_weighted_agg.es, title = "capital_intestinal_controls_2019_art_weighted")
ggsave("capital_intestinal_controls_2019_art_weighted.png", f22a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_combined_hosp_2019_art,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_weighted)
combined_intestinal_controls_2019_art_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_weighted_agg.es)
f23a <- ggdid(combined_intestinal_controls_2019_art_weighted_agg.es, title = "combined_intestinal_controls_2019_art_weighted")
ggsave("combined_intestinal_controls_2019_art_weighted.png", f23a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_overlap_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_weighted)
overlap_intestinal_controls_2019_art_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_weighted_agg.es)
f24a <- ggdid(overlap_intestinal_controls_2019_art_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_weighted")
ggsave("overlap_intestinal_controls_2019_art_weighted.png", f24a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_capital_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_art_share_weighted)
capital_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_art_share_weighted_agg.es)
f25a <- ggdid(capital_intestinal_controls_2019_art_share_weighted_agg.es, title = "capital_intestinal_controls_2019_art_share_weighted")
ggsave("capital_intestinal_controls_2019_art_share_weighted.png", f25a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                               data = treatment_combined_hosp_2019_art_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_art_share_weighted)
combined_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_art_share_weighted_agg.es)
f26a <- ggdid(combined_intestinal_controls_2019_art_share_weighted_agg.es, title = "combined_intestinal_controls_2019_art_share_weighted")
ggsave("combined_intestinal_controls_2019_art_share_weighted.png", f26a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_overlap_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_art_share_weighted)
overlap_intestinal_controls_2019_art_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_art_share_weighted_agg.es)
f27a <- ggdid(overlap_intestinal_controls_2019_art_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_art_share_weighted")
ggsave("overlap_intestinal_controls_2019_art_share_weighted.png", f27a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_capital_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_weighted)
capital_intestinal_controls_2019_ind_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_weighted_agg.es)
f28a <- ggdid(capital_intestinal_controls_2019_ind_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_weighted")
ggsave("capital_intestinal_controls_2019_ind_weighted.png", f28a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                         tname = "year",
                                                         idname = "muni_id",
                                                         gname = "G",
                                                         xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                         data = treatment_combined_hosp_2019_ind,
                                                         weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_weighted)
combined_intestinal_controls_2019_ind_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                               min_e = -10, max_e = 10,
                                                               na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_weighted_agg.es)
f29a <- ggdid(combined_intestinal_controls_2019_ind_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_weighted")
ggsave("combined_intestinal_controls_2019_ind_weighted.png", f29a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_overlap_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_weighted)
overlap_intestinal_controls_2019_ind_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_weighted_agg.es)
f30a <- ggdid(overlap_intestinal_controls_2019_ind_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_weighted")
ggsave("overlap_intestinal_controls_2019_ind_weighted.png", f30a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_capital_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(capital_intestinal_controls_2019_ind_share_weighted)
capital_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(capital_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(capital_intestinal_controls_2019_ind_share_weighted_agg.es)
f31a <- ggdid(capital_intestinal_controls_2019_ind_share_weighted_agg.es, title = "capital_intestinal_controls_2019_ind_share_weighted")
ggsave("capital_intestinal_controls_2019_ind_share_weighted.png", f31a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                               tname = "year",
                                                               idname = "muni_id",
                                                               gname = "G",
                                                               xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                               data = treatment_combined_hosp_2019_ind_share,
                                                               weightsname = "pop"
)
summary(combined_intestinal_controls_2019_ind_share_weighted)
combined_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(combined_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                     min_e = -10, max_e = 10,
                                                                     na.rm = TRUE)
summary(combined_intestinal_controls_2019_ind_share_weighted_agg.es)
f32a <- ggdid(combined_intestinal_controls_2019_ind_share_weighted_agg.es, title = "combined_intestinal_controls_2019_ind_share_weighted")
ggsave("combined_intestinal_controls_2019_ind_share_weighted.png", f32a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_overlap_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_ind_share_weighted)
overlap_intestinal_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(overlap_intestinal_controls_2019_ind_share_weighted_agg.es)
f33a <- ggdid(overlap_intestinal_controls_2019_ind_share_weighted_agg.es, title = "overlap_intestinal_controls_2019_ind_share_weighted")
ggsave("overlap_intestinal_controls_2019_ind_share_weighted.png", f33a, width = 8, height = 5, dpi = 300)

capital_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                           data = treatment_capital_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(capital_intestinal_controls_2019_amazon_weighted)
capital_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(capital_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(capital_intestinal_controls_2019_amazon_weighted_agg.es)
f34a <- ggdid(capital_intestinal_controls_2019_amazon_weighted_agg.es, title = "capital_intestinal_controls_2019_amazon_weighted")
ggsave("capital_intestinal_controls_2019_amazon_weighted.png", f34a, width = 8, height = 5, dpi = 300)

combined_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                            tname = "year",
                                                            idname = "muni_id",
                                                            gname = "G",
                                                            xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                            data = treatment_combined_hosp_2019_amazon,
                                                            weightsname = "pop"
)
summary(combined_intestinal_controls_2019_amazon_weighted)
combined_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(combined_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                  min_e = -10, max_e = 10,
                                                                  na.rm = TRUE)
summary(combined_intestinal_controls_2019_amazon_weighted_agg.es)
f35a <- ggdid(combined_intestinal_controls_2019_amazon_weighted_agg.es, title = "combined_intestinal_controls_2019_amazon_weighted")
ggsave("combined_intestinal_controls_2019_amazon_weighted.png", f35a, width = 8, height = 5, dpi = 300)

overlap_intestinal_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                           data = treatment_overlap_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(overlap_intestinal_controls_2019_amazon_weighted)
overlap_intestinal_controls_2019_amazon_weighted_agg.es <- aggte(overlap_intestinal_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(overlap_intestinal_controls_2019_amazon_weighted_agg.es)
f36a <- ggdid(overlap_intestinal_controls_2019_amazon_weighted_agg.es, title = "overlap_intestinal_controls_2019_amazon_weighted")
ggsave("overlap_intestinal_controls_2019_amazon_weighted.png", f36a, width = 8, height = 5, dpi = 300)

#pregnancy
capital_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_capital_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_pregnancy_controls_2019_weighted)
capital_pregnancy_controls_2019_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_pregnancy_controls_2019_weighted_agg.es)
f37a <- ggdid(capital_pregnancy_controls_2019_weighted_agg.es, title = "capital_pregnancy_controls_2019_weighted")
ggsave("capital_pregnancy_controls_2019_weighted.png", f37a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_combined_hosp_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_pregnancy_controls_2019_weighted)
combined_pregnancy_controls_2019_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_pregnancy_controls_2019_weighted_agg.es)
f38a <- ggdid(combined_pregnancy_controls_2019_weighted_agg.es, title = "combined_pregnancy_controls_2019_weighted")
ggsave("combined_pregnancy_controls_2019_weighted.png", f38a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_weighted <- att_gt(yname = "toxic_effects_pc",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_overlap_hosp_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_pregnancy_controls_2019_weighted)
overlap_pregnancy_controls_2019_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_weighted_agg.es)
f39a <- ggdid(overlap_pregnancy_controls_2019_weighted_agg.es, title = "overlap_pregnancy_controls_2019_weighted")
ggsave("overlap_pregnancy_controls_2019_weighted.png", f39a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_capital_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_weighted)
capital_pregnancy_controls_2019_art_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_weighted_agg.es)
f40a <- ggdid(capital_pregnancy_controls_2019_art_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_weighted")
ggsave("capital_pregnancy_controls_2019_art_weighted.png", f40a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_combined_hosp_2019_art,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_weighted)
combined_pregnancy_controls_2019_art_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_weighted_agg.es)
f41a <- ggdid(combined_pregnancy_controls_2019_art_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_weighted")
ggsave("combined_pregnancy_controls_2019_art_weighted.png", f41a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_overlap_hosp_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_weighted)
overlap_pregnancy_controls_2019_art_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_weighted_agg.es)
f42a <- ggdid(overlap_pregnancy_controls_2019_art_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_weighted")
ggsave("overlap_pregnancy_controls_2019_art_weighted.png", f42a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_capital_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_art_share_weighted)
capital_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_art_share_weighted_agg.es)
f43a <- ggdid(capital_pregnancy_controls_2019_art_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_art_share_weighted")
ggsave("capital_pregnancy_controls_2019_art_share_weighted.png", f43a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_combined_hosp_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_art_share_weighted)
combined_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_art_share_weighted_agg.es)
f44a <- ggdid(combined_pregnancy_controls_2019_art_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_art_share_weighted")
ggsave("combined_pregnancy_controls_2019_art_share_weighted.png", f44a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_art_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_overlap_hosp_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_art_share_weighted)
overlap_pregnancy_controls_2019_art_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_art_share_weighted_agg.es)
f45a <- ggdid(overlap_pregnancy_controls_2019_art_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_art_share_weighted")
ggsave("overlap_pregnancy_controls_2019_art_share_weighted.png", f45a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_capital_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_weighted)
capital_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_weighted_agg.es)
f46a <- ggdid(capital_pregnancy_controls_2019_ind_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_weighted")
ggsave("capital_pregnancy_controls_2019_ind_weighted.png", f46a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_combined_hosp_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_weighted)
combined_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_weighted_agg.es)
f47a <- ggdid(combined_pregnancy_controls_2019_ind_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_weighted")
ggsave("combined_pregnancy_controls_2019_ind_weighted.png", f47a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_weighted <- att_gt(yname = "toxic_effects_pc",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_overlap_hosp_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_weighted)
overlap_pregnancy_controls_2019_ind_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_weighted_agg.es)
f48a <- ggdid(overlap_pregnancy_controls_2019_ind_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_weighted.png", f48a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_capital_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_ind_share_weighted)
capital_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_pregnancy_controls_2019_ind_share_weighted_agg.es)
f49a <- ggdid(capital_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "capital_pregnancy_controls_2019_ind_share_weighted")
ggsave("capital_pregnancy_controls_2019_ind_share_weighted.png", f49a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_combined_hosp_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_ind_share_weighted)
combined_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_pregnancy_controls_2019_ind_share_weighted_agg.es)
f50a <- ggdid(combined_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "combined_pregnancy_controls_2019_ind_share_weighted")
ggsave("combined_pregnancy_controls_2019_ind_share_weighted.png", f50a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_ind_share_weighted <- att_gt(yname = "toxic_effects_pc",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_overlap_hosp_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_ind_share_weighted)
overlap_pregnancy_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es)
f51a <- ggdid(overlap_pregnancy_controls_2019_ind_share_weighted_agg.es, title = "overlap_pregnancy_controls_2019_ind_share_weighted")
ggsave("overlap_pregnancy_controls_2019_ind_share_weighted.png", f51a, width = 8, height = 5, dpi = 300)

capital_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_capital_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_pregnancy_controls_2019_amazon_weighted)
capital_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(capital_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_pregnancy_controls_2019_amazon_weighted_agg.es)
f52a <- ggdid(capital_pregnancy_controls_2019_amazon_weighted_agg.es, title = "capital_pregnancy_controls_2019_amazon_weighted")
ggsave("capital_pregnancy_controls_2019_amazon_weighted.png", f52a, width = 8, height = 5, dpi = 300)

combined_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                           data = treatment_combined_hosp_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_pregnancy_controls_2019_amazon_weighted)
combined_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(combined_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_pregnancy_controls_2019_amazon_weighted_agg.es)
f53a <- ggdid(combined_pregnancy_controls_2019_amazon_weighted_agg.es, title = "combined_pregnancy_controls_2019_amazon_weighted")
ggsave("combined_pregnancy_controls_2019_amazon_weighted.png", f53a, width = 8, height = 5, dpi = 300)

overlap_pregnancy_controls_2019_amazon_weighted <- att_gt(yname = "toxic_effects_pc",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_overlap_hosp_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_pregnancy_controls_2019_amazon_weighted)
overlap_pregnancy_controls_2019_amazon_weighted_agg.es <- aggte(overlap_pregnancy_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_pregnancy_controls_2019_amazon_weighted_agg.es)
f54a <- ggdid(overlap_pregnancy_controls_2019_amazon_weighted_agg.es, title = "overlap_pregnancy_controls_2019_amazon_weighted")
ggsave("overlap_pregnancy_controls_2019_amazon_weighted.png", f54a, width = 8, height = 5, dpi = 300)

#child_mortality_0_2
capital_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_capital_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(capital_child_0_2_controls_2019_weighted)
capital_child_0_2_controls_2019_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(capital_child_0_2_controls_2019_weighted_agg.es)
f55a <- ggdid(capital_child_0_2_controls_2019_weighted_agg.es, title = "capital_child_0_2_controls_2019_weighted")
ggsave("capital_child_0_2_controls_2019_weighted.png", f55a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                    tname = "year",
                                                    idname = "muni_id",
                                                    gname = "G",
                                                    xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                    data = treatment_combined_child_mort_2019,
                                                    weightsname = "pop"
                                                    #control_group = "not_yet_treated" #?
)
summary(combined_child_0_2_controls_2019_weighted)
combined_child_0_2_controls_2019_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_weighted, type = "dynamic",
                                                          min_e = -10, max_e = 10,
                                                          na.rm = TRUE)
summary(combined_child_0_2_controls_2019_weighted_agg.es)
f56a <- ggdid(combined_child_0_2_controls_2019_weighted_agg.es, title = "combined_child_0_2_controls_2019_weighted")
ggsave("combined_child_0_2_controls_2019_weighted.png", f56a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                   tname = "year",
                                                   idname = "muni_id",
                                                   gname = "G",
                                                   xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                   data = treatment_overlap_child_mort_2019,
                                                   weightsname = "pop"
                                                   #control_group = "not_yet_treated" #?
)
summary(overlap_child_0_2_controls_2019_weighted)
overlap_child_0_2_controls_2019_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_weighted, type = "dynamic",
                                                         min_e = -10, max_e = 10,
                                                         na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_weighted_agg.es)
f57a <- ggdid(overlap_child_0_2_controls_2019_weighted_agg.es, title = "overlap_child_0_2_controls_2019_weighted")
ggsave("overlap_child_0_2_controls_2019_weighted.png", f57a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_capital_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_weighted)
capital_child_0_2_controls_2019_art_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_weighted_agg.es)
f58a <- ggdid(capital_child_0_2_controls_2019_art_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_weighted")
ggsave("capital_child_0_2_controls_2019_art_weighted.png", f58a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_combined_child_mort_2019_art,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_weighted)
combined_child_0_2_controls_2019_art_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_weighted_agg.es)
f59a <- ggdid(combined_child_0_2_controls_2019_art_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_weighted")
ggsave("combined_child_0_2_controls_2019_art_weighted.png", f59a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_overlap_child_mort_2019_art,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_weighted)
overlap_child_0_2_controls_2019_art_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_weighted_agg.es)
f60a <- ggdid(overlap_child_0_2_controls_2019_art_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_weighted")
ggsave("overlap_child_0_2_controls_2019_art_weighted.png", f60a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_capital_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_art_share_weighted)
capital_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_art_share_weighted_agg.es)
f61a <- ggdid(capital_child_0_2_controls_2019_art_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_art_share_weighted")
ggsave("capital_child_0_2_controls_2019_art_share_weighted.png", f61a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_combined_child_mort_2019_art_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_art_share_weighted)
combined_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_art_share_weighted_agg.es)
f62a <- ggdid(combined_child_0_2_controls_2019_art_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_art_share_weighted")
ggsave("combined_child_0_2_controls_2019_art_share_weighted.png", f62a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_art_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_overlap_child_mort_2019_art_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_art_share_weighted)
overlap_child_0_2_controls_2019_art_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_art_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_art_share_weighted_agg.es)
f63a <- ggdid(overlap_child_0_2_controls_2019_art_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_art_share_weighted")
ggsave("overlap_child_0_2_controls_2019_art_share_weighted.png", f63a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_capital_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_weighted)
capital_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_weighted_agg.es)
f64a <- ggdid(capital_child_0_2_controls_2019_ind_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_weighted")
ggsave("capital_child_0_2_controls_2019_ind_weighted.png", f64a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                        tname = "year",
                                                        idname = "muni_id",
                                                        gname = "G",
                                                        xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                        data = treatment_combined_child_mort_2019_ind,
                                                        weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_weighted)
combined_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                              min_e = -10, max_e = 10,
                                                              na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_weighted_agg.es)
f65a <- ggdid(combined_child_0_2_controls_2019_ind_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_weighted")
ggsave("combined_child_0_2_controls_2019_ind_weighted.png", f65a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                       tname = "year",
                                                       idname = "muni_id",
                                                       gname = "G",
                                                       xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                       data = treatment_overlap_child_mort_2019_ind,
                                                       weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_weighted)
overlap_child_0_2_controls_2019_ind_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_weighted, type = "dynamic",
                                                             min_e = -10, max_e = 10,
                                                             na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_weighted_agg.es)
f66a <- ggdid(overlap_child_0_2_controls_2019_ind_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_weighted.png", f66a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_capital_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_ind_share_weighted)
capital_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(capital_child_0_2_controls_2019_ind_share_weighted_agg.es)
f67a <- ggdid(capital_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "capital_child_0_2_controls_2019_ind_share_weighted")
ggsave("capital_child_0_2_controls_2019_ind_share_weighted.png", f67a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                              tname = "year",
                                                              idname = "muni_id",
                                                              gname = "G",
                                                              xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                              data = treatment_combined_child_mort_2019_ind_share,
                                                              weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_ind_share_weighted)
combined_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                    min_e = -10, max_e = 10,
                                                                    na.rm = TRUE)
summary(combined_child_0_2_controls_2019_ind_share_weighted_agg.es)
f68a <- ggdid(combined_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "combined_child_0_2_controls_2019_ind_share_weighted")
ggsave("combined_child_0_2_controls_2019_ind_share_weighted.png", f68a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_ind_share_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                             tname = "year",
                                                             idname = "muni_id",
                                                             gname = "G",
                                                             xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                             data = treatment_overlap_child_mort_2019_ind_share,
                                                             weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_ind_share_weighted)
overlap_child_0_2_controls_2019_ind_share_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_ind_share_weighted, type = "dynamic",
                                                                   min_e = -10, max_e = 10,
                                                                   na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es)
f69a <- ggdid(overlap_child_0_2_controls_2019_ind_share_weighted_agg.es, title = "overlap_child_0_2_controls_2019_ind_share_weighted")
ggsave("overlap_child_0_2_controls_2019_ind_share_weighted.png", f69a, width = 8, height = 5, dpi = 300)

capital_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_capital_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(capital_child_0_2_controls_2019_amazon_weighted)
capital_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(capital_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(capital_child_0_2_controls_2019_amazon_weighted_agg.es)
f70a <- ggdid(capital_child_0_2_controls_2019_amazon_weighted_agg.es, title = "capital_child_0_2_controls_2019_amazon_weighted")
ggsave("capital_child_0_2_controls_2019_amazon_weighted.png", f70a, width = 8, height = 5, dpi = 300)

combined_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                           tname = "year",
                                                           idname = "muni_id",
                                                           gname = "G",
                                                           xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                           data = treatment_combined_child_mort_2019_amazon,
                                                           weightsname = "pop"
)
summary(combined_child_0_2_controls_2019_amazon_weighted)
combined_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(combined_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                 min_e = -10, max_e = 10,
                                                                 na.rm = TRUE)
summary(combined_child_0_2_controls_2019_amazon_weighted_agg.es)
f71a <- ggdid(combined_child_0_2_controls_2019_amazon_weighted_agg.es, title = "combined_child_0_2_controls_2019_amazon_weighted")
ggsave("combined_child_0_2_controls_2019_amazon_weighted.png", f71a, width = 8, height = 5, dpi = 300)

overlap_child_0_2_controls_2019_amazon_weighted <- att_gt(yname = "child_mort_pc_0_2",
                                                          tname = "year",
                                                          idname = "muni_id",
                                                          gname = "G",
                                                          xformla = ~gdp_pc_2002 + gva_agric_pc_2002 + vaccine_2002,
                                                          data = treatment_overlap_child_mort_2019_amazon,
                                                          weightsname = "pop"
)
summary(overlap_child_0_2_controls_2019_amazon_weighted)
overlap_child_0_2_controls_2019_amazon_weighted_agg.es <- aggte(overlap_child_0_2_controls_2019_amazon_weighted, type = "dynamic",
                                                                min_e = -10, max_e = 10,
                                                                na.rm = TRUE)
summary(overlap_child_0_2_controls_2019_amazon_weighted_agg.es)
f72a <- ggdid(overlap_child_0_2_controls_2019_amazon_weighted_agg.es, title = "overlap_child_0_2_controls_2019_amazon_weighted")
ggsave("overlap_child_0_2_controls_2019_amazon_weighted.png", f72a, width = 8, height = 5, dpi = 300)
