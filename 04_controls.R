library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tidyselect)
library(sf)
library(geobr)
library(readr)

df_yearly <- readRDS("/data/brazil_health/processed/df_yearly.RDS") %>%
  mutate(gva_agric_pc = gva_agric/pop) %>%
  select(muni_id, year, gdp, pop, share_poor_1991, illiteracy_1991, gva_agric_pc, gdp_pc,
        share_rural_1991, share_poor_2000, illiteracy_2000, share_rural_2000, gva_agric, 
        share_electricity_1991, share_electricity_2000) %>%
  filter(between(year, 1998, 2023))
# https://basedosdados.org/dataset/bbc6df97-08ff-4bb4-819d-6e5d85a80392?table=340d49d8-4e3a-4cde-bf14-e0bdb733ddde
controls <- read_csv("/home/francesca/brazil_mining/mining_health/processed data/br_ieps_saude_municipio.csv.gz") %>% 
  select(ano, id_municipio, nome, gasto_pbf_pc_def, cob_esf, desp_tot_saude_pc_mun_def, tx_med)
# gasto_pbf_pc_def: Gasto com o Programa Bolsa Familia (por Hab., R$ de 2021)
# cob_esf: Porcentagem da Cobertura de Estratégia de Saúde da Família
# desp_tot_saude_pc_mun_def: Total Health Expenditure Under Municipal Responsibility (per Hab., R$ de 2019)
# tx_med: Doctors (per 1,000 Hab.)
# num_familias_bf: Número de beneficiários do Bolsa Familia (however all NAs)
colSums(is.na(controls))
controls_na <- controls %>%
  filter(if_any(everything(), is.na))
BCG_vaccine <- read_csv("/home/francesca/brazil_mining/mining_health/processed data/br_ms_imunizacoes_municipio.csv.gz") %>%
  select(ano, id_municipio, cobertura_bcg) %>%
  filter(ano %in% c(1997, 2000, 2002))
BCG_zero <- BCG_vaccine %>%
  filter(cobertura_bcg == 0) # only less then 2% of the data (hopefully negligible)

BCG_wide <- BCG_vaccine %>%
  filter(ano %in% c(1997, 2000, 2002)) %>%
  mutate(ano = paste0("vaccine_", ano)) %>%
  pivot_wider(
    id_cols    = id_municipio,
    names_from = ano,
    values_from = cobertura_bcg
  )


# create dataset with CONTROLS
controls_joined <- df_yearly %>%
  left_join(controls, by = c("muni_id" = "id_municipio", "year" = "ano")) %>%
  left_join(BCG_wide, by = c("muni_id" = "id_municipio")) 
#%>%
#  filter(between(year, 2002, 2022))
colSums(is.na(controls_joined))

controls_2002 <- controls_joined %>%
  filter(year == 2002) %>%
  select(
    muni_id,
    gdp_pc_2002        = gdp_pc,
    gva_agric_pc_2002  = gva_agric_pc
  )

controls_joined <- controls_joined %>%
  left_join(controls_2002, by = "muni_id")


write_csv(controls_joined, "/home/francesca/brazil_mining/mining_health/processed data/controls_1998_2023.csv")
