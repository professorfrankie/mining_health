library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tidyselect)
library(sf)
library(geobr)


#health data
# Step 1: List of only the files you want
state_files <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
  "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE",
  "SP", "SP1", "SP2", "TO"
)

# Step 2: Construct full file paths
file_paths <- paste0("/data/brazil_health/processed/SIH/", state_files, "_hosp_patient_residence.RDS")

# Step 3: Processing function
process_health_data <- function(file_path) {
  readRDS(file_path) %>%
    filter(
      year >= 1998,
      (str_sub(diag_pri, 1, 3) >= "A00" & str_sub(diag_pri, 1, 3) <= "A09") |
        (str_sub(diag_pri, 1, 3) >= "T52" & str_sub(diag_pri, 1, 3) <= "T59") |
        (str_sub(diag_pri, 1, 3) >= "T61" & str_sub(diag_pri, 1, 3) <= "T62") |
        (str_sub(diag_pri, 1, 3) >= "T64" & str_sub(diag_pri, 1, 3) <= "T65") |
        (str_sub(diag_pri, 1, 3) >= "C00" & str_sub(diag_pri, 1, 3) <= "C97") |
        (str_sub(diag_pri, 1, 3) >= "G10" & str_sub(diag_pri, 1, 3) <= "G10") |
        (str_sub(diag_pri, 1, 3) >= "G20" & str_sub(diag_pri, 1, 3) <= "G20") |
        (str_sub(diag_pri, 1, 3) >= "G30" & str_sub(diag_pri, 1, 3) <= "G32") |
        (str_sub(diag_pri, 1, 3) >= "G35" & str_sub(diag_pri, 1, 3) <= "G35") |
        (str_sub(diag_pri, 1, 3) >= "A50" & str_sub(diag_pri, 1, 3) <= "A64") |
        (str_sub(diag_pri, 1, 3) >= "O00" & str_sub(diag_pri, 1, 3) <= "O03") |
        (str_sub(diag_pri, 1, 3) >= "O05" & str_sub(diag_pri, 1, 3) <= "O06") |
        (str_sub(diag_pri, 1, 3) >= "O08" & str_sub(diag_pri, 1, 3) <= "O16") |
        (str_sub(diag_pri, 1, 3) >= "O20" & str_sub(diag_pri, 1, 3) <= "O24") |
        (str_sub(diag_pri, 1, 3) >= "O26" & str_sub(diag_pri, 1, 3) <= "O28") |
        (str_sub(diag_pri, 1, 3) >= "O31" & str_sub(diag_pri, 1, 3) <= "O31") |
        (str_sub(diag_pri, 1, 3) >= "O33" & str_sub(diag_pri, 1, 3) <= "O36") |
        (str_sub(diag_pri, 1, 3) >= "O40" & str_sub(diag_pri, 1, 3) <= "O48") |
        (str_sub(diag_pri, 1, 3) >= "O60" & str_sub(diag_pri, 1, 3) <= "O75") |
        (str_sub(diag_pri, 1, 3) >= "O85" & str_sub(diag_pri, 1, 3) <= "O92") |
        (str_sub(diag_pri, 1, 3) >= "O94" & str_sub(diag_pri, 1, 3) <= "O94") |
        (str_sub(diag_pri, 1, 3) >= "O98" & str_sub(diag_pri, 1, 3) <= "O99") 
    ) %>%
    group_by(year, muni_pat, diag_pri, age) %>%
    summarise(
      total_hosp = sum(hosp_num, na.rm = TRUE),
      total_days = sum(hosp_days, na.rm = TRUE),
      total_brl = sum(val_brl, na.rm = TRUE),
      total_usd = sum(val_usd, na.rm = TRUE),
      .groups = "drop"
    )
}



# Step 4: Run the function and bind all results
health_all_states <- map_dfr(file_paths, process_health_data)
health_all_states <- health_all_states %>%
  na.omit() %>%
  filter(muni_pat != "000000")
health_diag_summary <- health_all_states %>%
  # Step 1: Remove the last 3 columns
  select(-total_days, -total_brl, -total_usd) %>%
  
  # Step 2: Summarize and pivot
  group_by(year, muni_pat, diag_pri) %>%
  summarise(total_hosp = sum(total_hosp, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = diag_pri,
    values_from = total_hosp,
    values_fill = 0  # Fill missing combinations with 0
  )

# Create summary columns
health_diag_summary <- health_diag_summary %>%
  mutate(
    intestinal_diseases = rowSums(select(., starts_with("A0")), na.rm = TRUE),
    toxic_effects = rowSums(select(., matches("^T")), na.rm = TRUE),
    cancer = rowSums(select(., matches("^C")), na.rm = TRUE),
    neurodegenerative = rowSums(select(., matches("^G")), na.rm = TRUE),
    pregnancy = rowSums(select(., matches("^O")), na.rm = TRUE),
    stds = rowSums(select(., matches("^A(5[0-9]|6[0-4])")), na.rm = TRUE)
  )

col_order <- c("muni_pat", "year", "intestinal_diseases", "toxic_effects", 
               "stds", "cancer", "neurodegenerative", "pregnancy",
               "A00", "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09",
               "A50", "A51", "A52", "A53", "A54", "A55", "A56", "A57", "A58", "A59",
               "A60", "A63", "A64",
               "T52", "T53", "T54", "T55", "T56", "T57", "T58", "T59", "T61",
               "T62", "T64", "T65",
               "G10", "G20", "G30", "G31", "G32", "G35",
               "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
               "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
               "C20", "C21", "C22", "C23", "C24", "C25", "C26", 
               "C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39",
               "C40", "C41", "C43", "C44", "C45", "C46", "C47", "C48", "C49",
               "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", 
               "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
               "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C77", "C78", "C79",
               "C80", "C81", "C82", "C83", "C84", "C85", "C88", 
               "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97",
               "O00", "O01", "O02", "O03", "O05", "O06",
               "O08", "O10", "O11", "O12", "O13", "O14", "O15", "O16",
               "O20", "O21", "O22", "O23", "O24", "O26", "O28", 
               "O31", "O33", "O34", "O35", "O36", 
               "O40", "O41", "O42", "O43", "O44", "O45", "O46", "O47", "O48",
               "O60", "O61", "O62", "O63", "O64", "O65", "O66", "O67", "O68", "O69",
               "O70", "O71", "O72", "O73", "O74", "O75",
               "O85", "O86", "O87", "O88", "O89", "O90", "O91", "O92", "O94", "O98", "O99"
)

health_diag_summary <- health_diag_summary[, col_order]
health_diag_summary$muni_pat <- as.numeric(as.character(health_diag_summary$muni_pat))

#child mortality
#state_files_mort <- c(
#  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
#  "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE",
#  "SP", "TO"
#)

file_paths <- paste0("/data/brazil_health/processed/SIM/mortalities_patient_residence.RDS")

process_mort_data <- function(file_path) {
  readRDS(file_path) %>%
    filter(
      year >= 1998,
      between(age, 0,2),
    ) %>%
    group_by(year, age, muni_pat, gender) %>%
    summarise(
      child_mort = sum(mort_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      gender = case_when(
        gender == 1 ~ "male",
        gender == 2 ~ "female",
        TRUE ~ "unknown"
      )
    ) %>%
    pivot_wider(
      names_from = gender,
      values_from = child_mort,
      names_prefix = "child_mort_",
      values_fn = sum,  
      values_fill = 0   
    ) %>%
    mutate(
      # make sure columns exist and are numeric
      child_mort_male = as.numeric(child_mort_male %||% 0),
      child_mort_female = as.numeric(child_mort_female %||% 0),
      total_child_mort = child_mort_male + child_mort_female
    )
}


# Step 4: Run the function and bind all results
mort_all_states <- map_dfr(file_paths, process_mort_data) 
mort_all_states <- mort_all_states %>%
  select(-child_mort_unknown) %>%
  rename(child_mort = total_child_mort)
mort_all_states$muni_pat <- as.numeric(as.character(mort_all_states$muni_pat))
colSums(is.na(mort_all_states))

muni_conc <- readRDS("/home/francesca/brazil_mining/mining_health/muni_conc.RDS")

child_mort <- left_join(mort_all_states, muni_conc[,c("muni_pat","muni_id")], by = "muni_pat")
hosp <- left_join(health_diag_summary, muni_conc[,c("muni_pat","muni_id")], by = "muni_pat")

colSums(is.na(child_mort))
colSums(is.na(hosp))

child_mort_noNA <- child_mort %>%
  filter(!is.na(muni_id)) %>%
  select(-muni_pat)
hosp_noNA <- hosp %>%
  filter(!is.na(muni_id)) %>%
  select(-muni_pat)

child_mort_NA <- child_mort %>%
  filter(is.na(muni_id))
hosp_NA <- hosp %>%
  filter(is.na(muni_id))
# hosp_noNA and child_mort_noNA have a different number of observations. so we need to understand for
# munis both data is available
setdiff(child_mort_noNA$muni_id, hosp_noNA$muni_id) 
length(unique(as.character(child_mort_noNA$muni_id)))
length(unique(as.character(hosp_noNA$muni_id)))# we see that the dataset have the same munis
#but still the data is different (not evry muni has data for all the years)

#check weather for some munis not some years are missing
expected_years <- 1998:2023
missing_years_child <- child_mort_noNA %>%
  group_by(muni_id) %>%
  summarise(
    missing_years_child = list(setdiff(expected_years, year)),
    n_missing = length(missing_years_child[[1]])
  ) %>%
  filter(n_missing > 0)

missing_years_child #for all these years child_mort = 0

missing_years_hosp <- hosp_noNA %>%
  group_by(muni_id) %>%
  summarise(
    missing_years_hosp = list(setdiff(expected_years, year)),
    n_missing = length(missing_years_hosp[[1]])
  ) %>%
  filter(n_missing > 0) 

missing_years_hosp #for all these years all desises = 0

#sum duplicates
child_mort_noNA <- child_mort_noNA %>%
  group_by(muni_id, age, year) %>%
  summarise(
    child_mort = sum(child_mort, na.rm = TRUE),
    across(-child_mort, ~ first(.x))
  ) %>%
  ungroup()

#create datasets with 0 for missing years
expected_years <- 1998:2023
age <- 0:2

# All expected muni–year pairs
full_panel <- expand_grid(
  muni_id = unique(child_mort_noNA$muni_id),
  year = expected_years
)

full_panel_age <- expand_grid(
  muni_id = unique(child_mort_noNA$muni_id),
  year = expected_years,
  age = age
)
child_mort_full <- full_panel_age %>%
  left_join(child_mort_noNA, by = c("muni_id", "year", "age")) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

child_mort_full1 <- child_mort_full %>%
  pivot_wider(
    id_cols = c(muni_id, year),
    names_from = age,
    values_from = c(child_mort, child_mort_male, child_mort_female),
    names_glue = "{.value}_{age}"
  ) %>%
  mutate(
    # ALL (0–2)
    child_mort_0_2        = child_mort_0 + child_mort_1 + child_mort_2,
    child_mort_male_0_2   = child_mort_male_0 + child_mort_male_1 + child_mort_male_2,
    child_mort_female_0_2 = child_mort_female_0 + child_mort_female_1 + child_mort_female_2,
    
    # AGE 0–1
    child_mort_0_1        = child_mort_0 + child_mort_1,
    child_mort_male_0_1   = child_mort_male_0 + child_mort_male_1,
    child_mort_female_0_1 = child_mort_female_0 + child_mort_female_1,
    
    # AGE 0 only
    child_mort_0        = child_mort_0,
    child_mort_male_0   = child_mort_male_0,
    child_mort_female_0 = child_mort_female_0
  )
  

hosp_full <- expand_grid(
  muni_id = unique(hosp_noNA$muni_id),
  year = expected_years
) %>%
  left_join(hosp_noNA, by = c("muni_id", "year")) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

colSums(is.na(child_mort_full))
colSums(is.na(hosp_full))

write_csv(child_mort_full1, "/home/francesca/brazil_mining/mining_health/processed data/child_mort_full.csv")
write_csv(hosp_full, "/home/francesca/brazil_mining/mining_health/processed data/hosp_full.csv")


