library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(readr)
library(ggplot2)
library(geobr)
library(scales)

mining_clean <- readRDS("/data/brazil/mining_mapbiomas/mining_area_csv/mb_mines_all.csv")|> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

substances_all <- tribble(
  ~category, ~id_range, ~substance,
  "Artisanal", 202:213, "artisanal_metallic_other",
  "Artisanal", 214,     "artisanal_metallic_tin",
  "Artisanal", 215,     "artisanal_metallic_gold",
  "Artisanal", 217:222, "artisanal_non-metallic",
  "Artisanal", 224:225, "artisanal_precious_stones",
  "Artisanal", 227:229, "artisanal_non_identified",
  
  "Industrial", 102:113, "industrial_metallic_other",
  "Industrial", 114,     "industrial_metallic_tin",
  "Industrial", 115,     "industrial_metallic_gold",
  "Industrial", 117:122, "industrial_non-metallic",
  "Industrial", 124:125, "industrial_non_identified",
  "Industrial", 127:129, "industrial_energetic",
  
  "Other", 302:329, "mining_OTHER"
) %>%
  mutate(mining_id = map(id_range, as.integer)) %>%
  unnest(mining_id) %>%
  select(mining_id, substance) |> 
  mutate(mining_type = case_when(
    str_detect(substance, "artisanal") ~ "artisanal",
    str_detect(substance, "industrial") ~ "industrial",
    TRUE ~ "other"
  ))


mining_spec <- mining_clean %>%
  left_join(substances_all, by = c("mining_class" = "mining_id"))
glimpse(mining_spec)

mining_summary <- mining_spec %>%
  filter(mining_type %in% c("artisanal", "industrial")) %>%  # keep only artisanal & industrial
  group_by(year, mining_type) %>%
  summarise(total_area_ha = sum(mining_area_ha, na.rm = TRUE)) %>%
  ungroup()

# Filter to include only artisanal & industrial
mining_summary_filtered <- mining_summary %>%
  filter(mining_type %in% c("artisanal", "industrial")) %>%
  filter(between(year, 1985, 2023))

# Figure 1
ggplot(mining_summary_filtered,
       aes(x = year, y = total_area_ha, fill = mining_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("artisanal" = "#FFD700", "industrial" = "#00008B"),
    name = "Mining Type",
    labels = c("artisanal", "industrial")
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(1985, 2023),
    breaks = seq(1985, 2023, by = 1)  # optional, adjust as you like
  ) +
  labs(
    #title = "Artisanal and Industrial Mining Footprints in the Legal Amazon",
    #subtitle = "2002–2022",
    x = "Year",
    y = "Area (ha)",
    caption = "Data: MapBiomas"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0),
    axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1, size = 10, color = "gray40")
  )
ggsave("/home/francesca/brazil_mining/mining_health/figures/mining_timeseries_both.png", width = 10, height = 6, dpi = 300, bg = "white")

#mining total
mining_summary_total <- mining_spec %>%
  group_by(year) %>%
  summarise(total_area_ha = sum(mining_area_ha, na.rm = TRUE)) %>%
  ungroup()

mining_art <- mining_spec |>  
  filter(mining_type == "artisanal") |>
  group_by(year, substance) |>
  summarise(mining_area_ha = sum(mining_area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "artisanal_metallic_other" = "Other",
    "artisanal_metallic_tin" = "Tin",
    "artisanal_metallic_gold" = "Gold",
    "artisanal_non-metallic" = "Non-metallic",
    "artisanal_non_identified" = "Non-identified",
    "artisanal_precious_stones" = "Precious stones"
  ))

## percentage of substances in artisanal mining per year

mining_art_year <- mining_art |> 
  group_by(year) |> 
  mutate(
    total_area_ha = sum(mining_area_ha, na.rm = TRUE),
    .groups = "drop"
    
  ) 

mining_art_year <- mining_art_year |>
  mutate(
    pct = mining_area_ha / total_area_ha * 100
  )

## pct in 2022 87% was gold extraction


## Proportional stacked area chart for industrial mining

mining_ind <- mining_spec |>  
  filter(mining_type == "industrial") |>
  group_by(year, substance) |>
  summarise(mining_area_ha = sum(mining_area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "industrial_metallic_other" = "Other",
    "industrial_metallic_tin" = "Tin",
    "industrial_metallic_gold" = "Gold",
    "industrial_non-metallic" = "Non-metallic",
    "industrial_non_identified" = "Non-identified",
    "industrial_energetic" = "Energetic"
  ))


library(viridis)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# All years and substances
all_years <- 1985:2023
all_substances <- c("Gold", "Tin", "Non-metallic", "Precious stones", "Non-identified", "Energetic", "Other")

# Define fixed colors for each substance
substance_colors <- setNames(viridis(length(all_substances), option = "D", direction = -1), 
                             all_substances)

# Complete artisanal data
mining_art_complete <- mining_art %>%
  mutate(substance = factor(substance, levels = all_substances)) %>%
  complete(year = all_years, substance = all_substances, fill = list(mining_area_ha = 0))

# Complete industrial data
mining_ind_complete <- mining_ind %>%
  mutate(substance = factor(substance, levels = all_substances)) %>%
  complete(year = all_years, substance = all_substances, fill = list(mining_area_ha = 0))

# Artisanal plot
p_art <- mining_art_complete %>%
  ggplot(aes(x = year, y = mining_area_ha, fill = substance)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = substance_colors, name = "Substance extracted") +
  scale_y_continuous(labels = scales::label_comma(), breaks = seq(0, 300000, 40000)) +
  scale_x_continuous(breaks = seq(1985, 2023, 1)) +
  labs(x = "Year", y = "Area (ha)", 
       title = "Artisanal Mining") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9))

# Industrial plot
p_ind <- mining_ind_complete %>%
  ggplot(aes(x = year, y = mining_area_ha, fill = substance)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = substance_colors, name = "Substance extracted") +
  scale_y_continuous(labels = scales::label_comma(), breaks = seq(0, 300000, 40000)) +
  scale_x_continuous(breaks = seq(1985, 2023, 1)) +
  labs(x = "Year", y = "Area (ha)", 
       title = "Industrial Mining") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9))

# Combine plots with shared legend
combined <- (p_art / p_ind) +
  plot_layout(heights = c(1.875, 1)) +
  plot_layout(ncol = 1, guides = "collect") & 
  theme(legend.position = "bottom")

plot(combined)

# Save figure
ggsave("/home/francesca/brazil_mining/mining_health/figures/mining_timeseries_combined_final.png",
       combined, width = 12, height = 11, dpi = 300, bg = "white")

# average industrial and artisanal
mining_spec %>%
  filter(mining_type == "artisanal") %>%
  summarise(mean_area = mean(mining_area_ha, na.rm = TRUE))

mining_spec %>%
  filter(mining_type == "industrial") %>%
  summarise(mean_area = mean(mining_area_ha, na.rm = TRUE))

# total mining area each year
mining_yearly <- mining_spec %>%
  group_by(year) %>%
  summarise(
    total_mining_area_ha = sum(mining_area_ha, na.rm = TRUE),
    .groups = "drop"
  )


#how much of industrial mining in 2023 is in the legal amazon?
mining_spec2023 <- mining_spec %>%
  filter(mining_type == "industrial",
        year == "2023") %>%
  mutate(
    in_legal_amazon = as.integer(
      st_within(st_centroid(geometry), legal_amazon, sparse = FALSE)[,1]
    )
  )

mining_spec2023_prc <- mining_spec2023 %>%
  summarise(
    total_area = sum(mining_area_ha, na.rm = TRUE),
    area_inside = sum(mining_area_ha[in_legal_amazon == 1], na.rm = TRUE),
    area_outside = sum(mining_area_ha[in_legal_amazon == 0], na.rm = TRUE),
    percent_inside = 100 * area_inside / total_area,
    percent_outside = 100 * area_outside / total_area
  )


## explore mining_ind

mining_ind |> 
  summarise(
    max_area_ha = max(area_ha, na.rm = TRUE),
    min_area_ha = min(area_ha, na.rm = TRUE)
  )

## min is 2.52 hectare and it is "Non-identifies" in 1987,
## max is 40544 hectare and it is "Other" in 2023.

## pct of substances in industrial mining per year

mining_ind <- mining_ind |> 
  group_by(year) |> 
  mutate(
    pct = area_ha / sum(area_ha, na.rm = TRUE) * 100,
    .groups = "drop"
    
  )


#treatment map

library(sf)
library(ggplot2)
library(geobr)

municipalities <- read_municipality(year = 2023) %>% 
  st_make_valid()
st_crs(municipalities)
br <- read_country()
capitals <- read_municipal_seat(year = 2010)

basins <- st_read("/data/brazil_health/HYBAS/hybas_lake_sa_lev07_v1c.shp")
basins_valid <- basins %>%
  st_make_valid()
st_crs(basins_valid)

br_mun <- st_transform(municipalities, st_crs(basins_valid))

st_crs(basins_valid) == st_crs(br_mun)

munis_no_islands <- br_mun %>%
  filter(!code_muni %in% c("2605459", "2204659", "3520400", "2607604"))

basins_brazil <- basins_valid %>%
  filter(st_intersects(., munis_no_islands, sparse = FALSE) %>% apply(1, any))

basins <- st_read(
  paste0("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/",
         "basins_l07_1998.shp"),
  quiet = TRUE
) |>
  st_transform(st_crs(br_mun)) |>
  st_make_valid() %>%
  filter(min_bsn == "6070491900")



library(ggspatial)  # for annotation functions
library(patchwork)


dummy_status <- basins[1:3, ]
dummy_status$legend <- c(
  "Downstream basin",
  "Upstream basin",
  "Mine-basin"
)
dummy_status$status <- c("downstream", "upstream", "mine")

legend_levels <- c(
  "Capitals",
  "Basins Brazil",
  "Municipalities",
  "Downstream basin",
  "Upstream basin",
  "Mine-basin"
)

basins_brazil$legend <- "Basins Brazil"
br_mun$legend <- "Municipalities"
capitals$legend <- "Capitals"


# --------------------------------------------------
# Main map
# --------------------------------------------------
main_map <- ggplot() +
  
  # Basins Brazil (outline)
  geom_sf(
    data = basins_brazil,
    aes(color = legend),
    fill = NA,
    size = 0.5
  ) +
  
  # Real basins filled by status (map only)
  geom_sf(
    data = basins,
    aes(fill = status),
    color = "darkblue",
    alpha = 0.6,
    size = 0.5,
    show.legend = FALSE
  ) +
  
  # Dummy basins for legend only
  geom_sf(
    data = dummy_status,
    aes(color = legend, fill = status),
    alpha = 0.6,
    show.legend = TRUE
  ) +
  
  # Municipalities
  geom_sf(
    data = br_mun,
    aes(color = legend),
    fill = NA,
    size = 0.3
  ) +
  
  # Capitals
  geom_sf(
    data = capitals,
    aes(color = legend),
    size = 2
  ) +
  
  # --------------------------------------------------
# Scales (ONE unified legend)
# --------------------------------------------------
scale_color_manual(
  name = "Legend",
  breaks = legend_levels,
  values = c(
    "Capitals" = "red",
    "Basins Brazil" = "darkblue",
    "Municipalities" = "#FF7256",
    "Downstream basin" = "darkblue",
    "Upstream basin" = "darkblue",
    "Mine-basin" = "darkblue"
  )
) +
  
  scale_fill_manual(
    values = c(
      "downstream" = "yellow",
      "upstream"   = "green",
      "mine"       = "gray"
    ),
    guide = "none"   # <- removes the "status" legend
  ) +
  
  # --------------------------------------------------
# Legend appearance
# --------------------------------------------------
guides(
  color = guide_legend(
    override.aes = list(
      linetype = c(0, 1, 1, 0, 0, 0),
      shape    = c(16, NA, NA, NA, NA, NA),
      size     = c(2, 0.6, 0.4, 4, 4, 4),
      fill     = c(NA, NA, NA, "yellow", "green", "gray")
    )
  )
) +
  
  # --------------------------------------------------
# Map layout
# --------------------------------------------------
coord_sf(
  xlim = c(-49.5, -46.5),
  ylim = c(-12, -9),
  expand = FALSE
) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.background = element_rect(fill = "snow"),
    legend.position = c(0.85, 0.20),
    legend.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering, height = unit(0.8, "cm"))

# --------------------------------------------------
# Inset map
# --------------------------------------------------
zoom_bbox <- st_as_sfc(
  st_bbox(
    c(xmin = -49.5, xmax = -47.5, ymin = -11.5, ymax = -9),
    crs = st_crs(br)
  )
)

inset_map <- ggplot() +
  geom_sf(data = br, fill = "grey85", color = "black") +
  geom_sf(data = zoom_bbox, fill = NA, color = "red", linewidth = 0.8) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# --------------------------------------------------
# Combine main map + inset
# --------------------------------------------------
final_map <- main_map +
  inset_element(
    inset_map,
    left = 0.65, bottom = 0.65,
    right = 1, top = 1
  )

# --------------------------------------------------
# Plot
# --------------------------------------------------
final_map
ggsave(
  filename = "final_map.png",   # Output file name
  plot = final_map,             # Your ggplot object
  width = 10,                   # Width in inches
  height = 8,                   # Height in inches
  dpi = 300                     # Resolution in dots per inch
)


#summary stat
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
process_treatment_data <- function(
    path,
    dist_n_max = 2,
    year_min = 2003,
    year_max = 2019
) {
  
  data_raw <- st_read(path)
  data_raw <- st_set_geometry(data_raw, NULL)
  
  data_processed <- data_raw %>% 
    group_by(muni_id) %>%
    # remove municipalities with any dist_n > dist_n_max
    filter(!any(dist_n > dist_n_max, na.rm = TRUE)) %>%
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


treatment_combined_child_mort <-
  process_treatment_data(
    "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg"
  )

treatment_combined_hosp <-
  process_treatment_data(
    "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg"
  )

count_treat_control <- function(df) {
  df %>%
    distinct(muni_id, G) %>%
    mutate(group = if_else(G == 0, "control_upstream", "treated_downstream")) %>%
    count(group)
}

count_treat_control(treatment_combined_child_mort_2019_art)
count_treat_control(treatment_combined_hosp_2019_art)
count_treat_control(treatment_combined_child_mort)
count_treat_control(treatment_combined_hosp)
count_treat_control(treatment_combined_child_mort_2019_ind)
count_treat_control(treatment_combined_hosp_2019_ind)
count_treat_control(treatment_capital_child_mort)
count_treat_control(treatment_capital_hosp)
count_treat_control(treatment_overlap_child_mort)
count_treat_control(treatment_overlap_hosp)
