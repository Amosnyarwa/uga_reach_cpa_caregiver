library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("inputs/clean_data_caregiver.csv")
df_children_perform_domestic_chores_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_domestic_chores_info") 
df_protection_risky_places = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "protection_risky_places")
df_children_perform_economic_labour_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_economic_labour_info")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population.csv")
df_host_pop <- read_csv("inputs/host_population.csv")

# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_dfa(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                            status == "host_community" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  ))

# split data into host and refugee ----------------------------------------

df_ref <- df_with_composites %>% 
  filter(status == "refugee")

df_host <- df_with_composites %>% 
  filter(status == "host_community")