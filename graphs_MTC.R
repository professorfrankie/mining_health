# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggstream)
library(datazoom.amazonia)
library(ggplot2)
library(viridis)
mining <- read_csv("/home/francesca/brazil_mining/mining_health/mining_munis.csv", quote = "")

mining_clean <- mining |> 
  transmute(muni_id = CD_MUN, 
            muni_id = str_replace(muni_id, '"', ""),
            year = as.numeric(substr(bandName, 17, 20)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", ""),
            histogram = str_replace_all(histogram, '"', "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("mining_id", "area"), sep = "=") |>  
  mutate(mining_id = as.numeric(mining_id)) |> 
  transmute(muni_id, year, mining_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year) |> 
  filter(between(year, 2003, 2019))

## change code to name for mining_id to create graphs

min_code <- mining_clean |> 
  distinct(mining_id) |> 
  arrange(mining_id)

print(min_code, n = 100)

min_art <- mining_clean |> 
  filter(mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229))

min_ind <- mining_clean |> 
  filter(mining_id %in% c(102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                          112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
                          124, 125, 127, 128, 129))

## mapping of mining_id to substances and mining types

substances_all <- tibble(
  mining_id = c(
    # Artisanal
    202:213, 214, 215, 217, 218:222, 224, 225, 227:229,
    
    # Industrial
    102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
    124, 125, 127, 128, 129,
    
    # Other
    302, 303, 304, 305, 306, 307, 308, 309, 310, 311,
    312, 313, 314, 315, 317, 318, 319, 320, 321, 322,
    324, 325, 327, 328, 329
  ),
  substance = c(
    # Artisanal
    rep("artisanal_metallic_other", 12),     # 202–213
    "artisanal_metallic_tin",               # 214
    "artisanal_metallic_gold",              # 215
    rep("artisanal_non-metallic", 6), # 217–222
    rep("artisanal_precious stones",2),              # 224-225
    rep("artisanal_non_identified", 3),       # 227–229
    
    # Industrial
    rep("industrial_metallic_other", 12),
    "industrial_metallic_tin",               # 214
    "industrial_metallic_gold",              # 215
    rep("industrial_non-metallic", 6),
    rep("industrial_non_identified", 2), 
    rep("industrial_energetic", 3),
    # Other
    rep("mining_OTHER", 25)
  ),
)


mining1 <- left_join(mining_clean, substances_all, by = "mining_id")

artisanal <- c(
  "artisanal_non-metallic", 
  "artisanal_metallic_tin", 
  "artisanal_metallic_gold", 
  "artisanal_precious stones",
  "artisanal_metallic_other",
  "artisanal_non_identified"
  
)

industrial <- c(
  "industrial_non-metallic",
  "industrial_metallic_other",
  "industrial_non_identified",
  "industrial_metallic_gold",
  "industrial_energetic",
  "industrial_metallic_tin",
  "industrial_metallic_other"
)


## legal amazon munis

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

## Proportional stacked area chart for artisanal mining

mining_art <- mining1 |>  
  filter(substance %in% artisanal #&
           #muni_id %in% legal_amazon_munis
         ) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "artisanal_metallic_other" = "Other",
    "artisanal_metallic_tin" = "Tin",
    "artisanal_metallic_gold" = "Gold",
    "artisanal_non-metallic" = "Non-metallic",
    "artisanal_non_identified" = "Non-identified",
    "artisanal_precious stones" = "Precious stones"
  ))

## percentage of substances in artisanal mining per year

mining_art <- mining_art |> 
  group_by(year) |> 
  mutate(
    total_area_ha = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
    
  ) 

## plot

mining_art |> 
  mutate(substance = fct_relevel(substance, 
                                 "Gold", "Tin", "Non-metallic", "Precious stones", "Other")) |>
  filter (between(year, 2003, 2019)) |>
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_area(size = 1, color = "black" ) +
  scale_fill_viridis_d(option = "D", direction = -1, name = "Substance") +
  scale_y_continuous(
    labels = scales::label_comma(),
    breaks = scales::pretty_breaks(n = 7)   # adjust n to control tick density
  ) +
  scale_x_continuous(breaks = seq(2003, 2019, 1)) +
  labs(
    #title = "Artisanal mining area in Brazil by substance",
    #subtitle = "2002-2022",
    x = "Year",
    y = "Area (ha)",
    fill = "Substance"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  theme_minimal()

ggsave("/home/francesca/brazil_mining/mining_health/figures/artisanal_mining_timeseries.png", width = 10, height = 6, dpi = 300, bg = "white")

mining_art <- mining_art |>
  mutate(
    pct = area_ha / total_area_ha * 100
  )

## pct in 2022 87% was gold extraction


## Proportional stacked area chart for industrial mining

mining_ind <- mining1 |>  
  filter(substance %in% industrial #&
           #muni_id %in% legal_amazon_munis
           ) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "industrial_metallic_other" = "Other",
    "industrial_metallic_tin" = "Tin",
    "industrial_metallic_gold" = "Gold",
    "industrial_non-metallic" = "Non-metallic",
    "industrial_non_identified" = "Non-identified",
    "industrial_energetic" = "Energetic"
  ))


mining_ind |> 
  mutate(substance = fct_relevel(substance, 
                                 "Gold", "Tin", "Non-metallic", "Non-identified", "Other")) |>
  filter(between(year, 2003, 2019)) |>
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_area(size = 1, color = "black") +
  scale_fill_viridis_d(option = "D", direction = -1, name = "Substance") +
  ## change scale y to make it readable withouth e
  scale_y_continuous(
    labels = scales::label_comma(),
    breaks = scales::pretty_breaks(n = 7)   # adjust n to control tick density
  ) +
  scale_x_continuous(breaks = seq(2003, 2019, 1)) +
  labs(
    #title = "Industrial mining area in Brazil by substance",
    #subtitle = "2002-2022",
    x = "Year",
    y = "Area (ha)",
    fill = "Substance"
  ) +
  theme(    plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)) +
  theme_minimal()

ggsave("/home/francesca/brazil_mining/mining_health/figures/industrial_mining_timeseries.png", 
       width = 10, height = 6, dpi = 300, bg = "white")

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
