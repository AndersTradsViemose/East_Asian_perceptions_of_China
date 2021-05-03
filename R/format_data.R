# Load packages
library(labelled)
library(tidyverse)
library(haven)
library(sf)
library(ggrepel)
library(ggthemes)
library(tidytext)

baro %>% names(se3_2)

# Read data ----
baro_raw <- read_dta("data/W4 Merged Data/W4_v15_merged20181211_release.dta")
jpn_1_raw <- readRDS("data/gadm36_JPN_1_sf.rds")

jpn_rgns_raw <- read_csv("data/japan/japan_regions.csv")
jpn_pref_raw <- read_csv("data/japan/japan_pref.csv")

# Shapefiles
# https://www.igismap.com/download-japan-shapefile-free-country-boundaries-cities-maps-and-polygon-shapefile/
#japan_regions <- sf::read_sf("data/jpn_adm_2019_shp/jpn_admbnda_adm1_2019.shp")

# Tidy barometer data ----
## By country or region ----
# Get labels
baro <- baro_raw %>%
  mutate(across(where(is.labelled),
                ~as_factor(., levels = "labels", ordered = TRUE) %>% # extract value labels
                  tolower %>% trimws %>% str_squish) # lowercase, remove trailing/leading and double spaces
  )
# Select questions
baro_selec <- baro %>%
  select(country, year, month, region, level, q125,
         q127, q163, q164, q165, q166, q167, q168, q169, q170, q171,q172)

# Number of people given the answer per country, national level
baro_perc_nat<- baro_selec %>% 
  pivot_longer(cols = c(q125, q127, q163, q164, q165, q166, q167, q168, q169, q170, q171,q172)
  ) %>%
  group_by(country, name, value) %>% # grouped by country, name (question), and response (value)
  summarise(n = n()) # this gives us number of respondents per response per question per country

# Calculates share of response per country and question
baro_perc_nat <- baro_perc_nat %>%
  group_by(country, name) %>% # I don't include 'region' in order to get national share
  mutate(sum_country = sum(n), # total respondents for each question per country
         share = n/sum_country*100) # share of respondents who picked a certain response



## Rural-urban divide ----
rural_or_urban <- baro_selec %>% 
  pivot_longer(cols = c(q125, q127, q163, q164, q165, q166, q167, q168, q169, q170, q171,q172)
  ) %>%
  group_by(country, region, level, name, value) %>%
  summarise(n = n()) %>%
  mutate(value = case_when(value == "economic development is definitely more important" ~ "prefers_economic",
                           value == "economic development is somewhat more important"  ~ "prefers_economic",
                           value == "democracy is somewhat more important" ~ "prefers_democracy",
                           value == "democracy is definitely more important" ~ "prefers_democracy",
                           value == "very positive" ~ "more_positive_than_negative",
                           value == "positive" ~ "more_positive_than_negative",
                           value == "somewhat positive" ~ "more_positive_than_negative",
                           value == "somewhat negative" ~ "more_negative_than_positive",
                           value == "negative" ~ "more_negative_than_positive",
                           value == "very negative" ~ "more_negative_than_positive",
                           TRUE ~ value))

# Per level and country
rural_or_urban_per_country <- rural_or_urban %>%
  group_by(country, level, name, value) %>%
  summarise(sum = sum(n)) %>%
  filter(level != "missing") %>%
  group_by(country, level, name) %>%
  mutate(total = sum(sum)) %>%
  ungroup() %>%
  mutate(share = sum/total*100) %>%
  select(-sum, - total)

rural_or_urban_per_country <- rural_or_urban_per_country %>%
  pivot_wider(names_from = value, values_from = share)


# Per level across all countries
# Calculate number of respondent for each answer per level
rural_or_urban <- rural_or_urban %>%
  group_by(level, name, value) %>%
  summarise(sum = sum(n))

# Calculate share of total respondents
rural_or_urban <- rural_or_urban %>%
  filter(level != "missing") %>%
  group_by(level, name) %>%
  mutate(total = sum(sum)) %>%
  ungroup() %>%
  mutate(share = sum/total*100)

rural_or_urban

# Create national share by urban or rural
baro_perc_nat_urban_rural <- baro_selec %>% 
  pivot_longer(cols = c(q125, q127, q163, q164, q165, q166, q167, q168, q169, q170, q171,q172)
  ) %>%
  group_by(country, level, name, value) %>% # level is included to see rural-urban difference
  summarise(n = n())

baro_perc_nat_urban_rural <- baro_perc_nat_urban_rural %>%
  group_by(country, level, name) %>%
  mutate(sum_country = sum(n),
         share = n/sum_country*100
  )

baro_perc_nat_urban_rural






# Tidy spatial data ----
# Join Japan's look-up data
jpn_rgns <- jpn_rgns_raw %>%
  transmute(id, region = `name-en`)

jpn_pref <- jpn_pref_raw %>%
  transmute(prefecture = `name-en`, id = regions)

japans_regions <- left_join(jpn_pref, jpn_rgns) %>%
  mutate(region = tolower(region),
         prefecture = tolower(prefecture))

# Simplify to plot
jpn_1_simp <- st_simplify(jpn_1_raw, dTolerance = 0.05) %>%
  mutate(country = tolower(NAME_0),
         prefecture = tolower(NAME_1)) %>%
  left_join(japans_regions, by = "prefecture")

# Join barometer and spatial ----
baro_spatial <- left_join(jpn_1_simp, baro_perc, by = c("country", "region"))




