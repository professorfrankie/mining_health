library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tidyselect)
library(sf)
library(geobr)
library(readr)
library(datazoom.amazonia)

treatment_capital <- st_read("/home/francesca/brazil_mining/mining_health/treatment_capital_location.gpkg")
treatment_overlap20 <- st_read("/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.gpkg",
                             layer = "treatment_area_overlap_20"
                            #) %>%
  #rename(legal_amazon = legal_amazon.x) %>%
 # select(-legal_amazon.y#, -legal_amazon.x.x, -legal_amazon.y.y
         )
treatment_combined20 <- st_read("/home/francesca/brazil_mining/mining_health/treatment_combined_20.gpkg")
treatment_overlap40 <- st_read("/home/francesca/brazil_mining/mining_health/treatment_area_overlap_40.gpkg")
treatment_combined40 <- st_read("/home/francesca/brazil_mining/mining_health/treatment_combined_40.gpkg")
child_mort_full <- read.csv("/home/francesca/brazil_mining/mining_health/processed data/child_mort_full.csv")
hosp_full <- read.csv("/home/francesca/brazil_mining/mining_health/processed data/hosp_full.csv")
controls <- read.csv("/home/francesca/brazil_mining/mining_health/processed data/controls_1998_2023.csv")

# create a OPENED and DOWNSTREAM dummy
treatment_capital_child_mort <- child_mort_full %>%
  left_join(treatment_capital, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(
    # per capita rates
    child_mort_pc_0_2 = (child_mort_0_2/pop)*1000,
    child_mort_pc_male_0_2 = (child_mort_male_0_2/pop)*1000,
    child_mort_pc_female_0_2 = (child_mort_female_0_2/pop)*1000,
    
    child_mort_pc_0_1 = (child_mort_0_1/pop)*1000,
    child_mort_pc_male_0_1 = (child_mort_male_0_1/pop)*1000,
    child_mort_pc_female_0_1 = (child_mort_female_0_1/pop)*1000,
    
    child_mort_pc_0 = (child_mort_0/pop)*1000,
    child_mort_pc_male_0 = (child_mort_male_0/pop)*1000,
    child_mort_pc_female_0 = (child_mort_female_0/pop)*1000
  ) %>%
  ungroup()
treatment_combined_child_mort20 <- child_mort_full %>%
  left_join(treatment_combined20, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(
    # per capita rates
    child_mort_pc_0_2 = (child_mort_0_2/pop)*1000,
    child_mort_pc_male_0_2 = (child_mort_male_0_2/pop)*1000,
    child_mort_pc_female_0_2 = (child_mort_female_0_2/pop)*1000,
    
    child_mort_pc_0_1 = (child_mort_0_1/pop)*1000,
    child_mort_pc_male_0_1 = (child_mort_male_0_1/pop)*1000,
    child_mort_pc_female_0_1 = (child_mort_female_0_1/pop)*1000,
    
    child_mort_pc_0 = (child_mort_0/pop)*1000,
    child_mort_pc_male_0 = (child_mort_male_0/pop)*1000,
    child_mort_pc_female_0 = (child_mort_female_0/pop)*1000
  ) %>%
  ungroup()
treatment_overlap_child_mort20 <- child_mort_full %>%
  left_join(treatment_overlap20, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(
    # per capita rates
    child_mort_pc_0_2 = (child_mort_0_2/pop)*1000,
    child_mort_pc_male_0_2 = (child_mort_male_0_2/pop)*1000,
    child_mort_pc_female_0_2 = (child_mort_female_0_2/pop)*1000,
    
    child_mort_pc_0_1 = (child_mort_0_1/pop)*1000,
    child_mort_pc_male_0_1 = (child_mort_male_0_1/pop)*1000,
    child_mort_pc_female_0_1 = (child_mort_female_0_1/pop)*1000,
    
    child_mort_pc_0 = (child_mort_0/pop)*1000,
    child_mort_pc_male_0 = (child_mort_male_0/pop)*1000,
    child_mort_pc_female_0 = (child_mort_female_0/pop)*1000
  ) %>%
  ungroup()

treatment_combined_child_mort40 <- child_mort_full %>%
  left_join(treatment_combined40, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(
    # per capita rates
    child_mort_pc_0_2 = (child_mort_0_2/pop)*1000,
    child_mort_pc_male_0_2 = (child_mort_male_0_2/pop)*1000,
    child_mort_pc_female_0_2 = (child_mort_female_0_2/pop)*1000,
    
    child_mort_pc_0_1 = (child_mort_0_1/pop)*1000,
    child_mort_pc_male_0_1 = (child_mort_male_0_1/pop)*1000,
    child_mort_pc_female_0_1 = (child_mort_female_0_1/pop)*1000,
    
    child_mort_pc_0 = (child_mort_0/pop)*1000,
    child_mort_pc_male_0 = (child_mort_male_0/pop)*1000,
    child_mort_pc_female_0 = (child_mort_female_0/pop)*1000
  ) %>%
  ungroup()
treatment_overlap_child_mort40 <- child_mort_full %>%
  left_join(treatment_overlap40, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(
    # per capita rates
    child_mort_pc_0_2 = (child_mort_0_2/pop)*1000,
    child_mort_pc_male_0_2 = (child_mort_male_0_2/pop)*1000,
    child_mort_pc_female_0_2 = (child_mort_female_0_2/pop)*1000,
    
    child_mort_pc_0_1 = (child_mort_0_1/pop)*1000,
    child_mort_pc_male_0_1 = (child_mort_male_0_1/pop)*1000,
    child_mort_pc_female_0_1 = (child_mort_female_0_1/pop)*1000,
    
    child_mort_pc_0 = (child_mort_0/pop)*1000,
    child_mort_pc_male_0 = (child_mort_male_0/pop)*1000,
    child_mort_pc_female_0 = (child_mort_female_0/pop)*1000
  ) %>%
  ungroup()
    

treatment_capital_hosp <- hosp_full %>%
  left_join(treatment_capital, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(intestinal_diseases_pc = (intestinal_diseases/pop)*100000,
         toxic_effects_pc = (toxic_effects/pop)*100000,
         cancer_pc = (cancer/pop)*100000,
         neurodegenerative_pc = (neurodegenerative/pop)*100000,
         pregnancy_pc = (pregnancy/pop)*100000,
         stds_pc = (stds/pop)*100000,
         T56_pc = (T56/pop)*100000)
treatment_combined_hosp20 <- hosp_full %>%
  left_join(treatment_combined20, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(intestinal_diseases_pc = (intestinal_diseases/pop)*100000,
         toxic_effects_pc = (toxic_effects/pop)*100000,
         cancer_pc = (cancer/pop)*100000,
         neurodegenerative_pc = (neurodegenerative/pop)*100000,
         pregnancy_pc = (pregnancy/pop)*100000,
         stds_pc = (stds/pop)*100000,
         T56_pc = (T56/pop)*100000)
treatment_overlap_hosp20 <- hosp_full %>%
  left_join(treatment_overlap20, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(intestinal_diseases_pc = (intestinal_diseases/pop)*100000,
         toxic_effects_pc = (toxic_effects/pop)*100000,
         cancer_pc = (cancer/pop)*100000,
         neurodegenerative_pc = (neurodegenerative/pop)*100000,
         pregnancy_pc = (pregnancy/pop)*100000,
         stds_pc = (stds/pop)*100000,
         T56_pc = (T56/pop)*100000)

treatment_combined_hosp40 <- hosp_full %>%
  left_join(treatment_combined40, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(intestinal_diseases_pc = (intestinal_diseases/pop)*100000,
         toxic_effects_pc = (toxic_effects/pop)*100000,
         cancer_pc = (cancer/pop)*100000,
         neurodegenerative_pc = (neurodegenerative/pop)*100000,
         pregnancy_pc = (pregnancy/pop)*100000,
         stds_pc = (stds/pop)*100000,
         T56_pc = (T56/pop)*100000)
treatment_overlap_hosp40 <- hosp_full %>%
  left_join(treatment_overlap40, by = c("year" = "year", "muni_id" = "code_muni")) %>%
  left_join(controls, by = c("year" = "year", "muni_id" = "muni_id")) %>%
  select(-nome) %>%
  mutate(intestinal_diseases_pc = (intestinal_diseases/pop)*100000,
         toxic_effects_pc = (toxic_effects/pop)*100000,
         cancer_pc = (cancer/pop)*100000,
         neurodegenerative_pc = (neurodegenerative/pop)*100000,
         pregnancy_pc = (pregnancy/pop)*100000,
         stds_pc = (stds/pop)*100000,
         T56_pc = (T56/pop)*100000)


#create mine_type variable
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

# Drop geometry for data manipulation
geom_data <- treatment_capital_child_mort %>%
  st_drop_geometry() %>%
  mutate(row_id = row_number())

treatment_capital_child_mort <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()


# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_combined_child_mort20) %>%
  mutate(row_id = row_number())

treatment_combined_child_mort20 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()


# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_overlap_child_mort20) %>%
  mutate(row_id = row_number())

treatment_overlap_child_mort20 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

geom_data <- st_drop_geometry(treatment_combined_child_mort40) %>%
  mutate(row_id = row_number())

treatment_combined_child_mort40 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()


# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_overlap_child_mort40) %>%
  mutate(row_id = row_number())

treatment_overlap_child_mort40 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_capital_hosp) %>%
  mutate(row_id = row_number())

treatment_capital_hosp <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()


# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_combined_hosp20) %>%
  mutate(row_id = row_number())

treatment_combined_hosp20 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_overlap_hosp20) %>%
  mutate(row_id = row_number())

treatment_overlap_hosp20 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

geom_data <- st_drop_geometry(treatment_combined_hosp40) %>%
  mutate(row_id = row_number())

treatment_combined_hosp40 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

# Drop geometry and add row_id BEFORE pipeline
geom_data <- st_drop_geometry(treatment_overlap_hosp40) %>%
  mutate(row_id = row_number())

treatment_overlap_hosp40 <- geom_data %>%
  mutate(mining_class = str_split(mining_class, ";\\s*")) %>%
  unnest(mining_class) %>%
  mutate(mining_class = as.integer(mining_class)) %>%
  
  # Join with mapping table
  left_join(substances_all %>% select(mining_id, mining_type),
            by = c("mining_class" = "mining_id")) %>%
  
  # Collapse multiple mining classes per original row
  group_by(row_id) %>%
  summarise(
    mine_type = case_when(
      all(is.na(mining_type)) ~ NA_character_, 
      all(mining_type == "artisanal", na.rm = TRUE) ~ "artisanal",
      all(mining_type == "industrial", na.rm = TRUE) ~ "industrial",
      all(mining_type == "other", na.rm = TRUE) ~ "other",
      n_distinct(mining_type, na.rm = TRUE) > 1 ~ "both",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  
  # Join back to the full (non-spatial) data
  right_join(geom_data, by = "row_id") %>%
  select(-row_id) %>%
  group_by(muni_id) %>%
  mutate(
    mine_type = case_when(
      all(is.na(mine_type)) ~ NA_character_,  # no info at all
      n_distinct(na.omit(mine_type)) > 1 ~ "both",  # multiple types within muni
      TRUE ~ na.omit(mine_type)[1]  # fill with the unique non-NA type
    )
  ) %>%
  ungroup()

munis <- read_municipality(year = 2010) %>% 
  st_make_valid() %>%
  select(code_muni, name_muni)

data("municipalities") 
municipalities <- municipalities %>%
  select(code_muni, legal_amazon)

munis <- left_join(munis, municipalities, by = "code_muni")

expanded_df <- tidyr::crossing(
  munis,
  year = 1998:2023
)
legal_amazon <- expanded_df %>%
  select(code_muni, year, legal_amazon)

treatment_capital_child_mort <- treatment_capital_child_mort %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_capital_hosp <- treatment_capital_hosp %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_combined_child_mort20 <- treatment_combined_child_mort20 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_combined_hosp20 <- treatment_combined_hosp20 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_overlap_child_mort20 <- treatment_overlap_child_mort20 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_overlap_hosp20 <- treatment_overlap_hosp20 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_combined_child_mort40 <- treatment_combined_child_mort40 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_combined_hosp40 <- treatment_combined_hosp40 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_overlap_child_mort40 <- treatment_overlap_child_mort40 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))
treatment_overlap_hosp40 <- treatment_overlap_hosp40 %>%
  left_join(legal_amazon, by = c("muni_id" = "code_muni", "year" = "year"))

write_sf(treatment_capital_child_mort, "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort_scaled.gpkg")
write_sf(treatment_combined_child_mort20, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_20_scaled.gpkg")
write_sf(treatment_overlap_child_mort20, "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort_20_scaled.gpkg")
write_sf(treatment_combined_child_mort40, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_child_mort_40_scaled.gpkg")
write_sf(treatment_overlap_child_mort40, "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_child_mort_40_scaled.gpkg")

write_sf(treatment_capital_hosp, "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_hosp_scaled1.gpkg")
write_sf(treatment_combined_hosp20, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_20_scaled.gpkg")
write_sf(treatment_overlap_hosp20, "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp_20_scaled.gpkg")
write_sf(treatment_combined_hosp40, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_hosp_40_scaled.gpkg")
write_sf(treatment_overlap_hosp40, "/home/francesca/brazil_mining/mining_health/processed data/treatment_overlap_hosp_40_scaled.gpkg")

# if necessary
# expand treatment overlap dataset
munis <- read_municipality(year = 2010) %>% 
  st_make_valid() %>%
  select(code_muni, name_muni)

data("municipalities") 
municipalities <- municipalities %>%
  select(code_muni, legal_amazon)

munis <- left_join(munis, municipalities, by = "code_muni")

expanded_df <- tidyr::crossing(
  munis,
  year = 1998:2023
)

treatment_overlap1 <- expanded_df %>%
  left_join(treatment_overlap, by = c("year" = "year", "code_muni" = "code_muni")) %>%
  select(-geom.x, -name_muni.x) %>%
  rename(geom = geom.y,
         name_muni = name_muni.y)
write_sf(treatment_overlap1, "/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.shp")
write_sf(treatment_overlap1, "/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.gpkg")