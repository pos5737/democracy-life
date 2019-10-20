
library(tidyverse)
library(readxl)
library(countrycode)

# clean life expectancy dataset ----
le_df_raw <- read_csv("raw-data/API_SP.DYN.LE00.IN_DS2_en_csv_v2_315861.csv", skip = 4) %>%
  glimpse()

le_df <- le_df_raw %>%
  select(-`Indicator Name`, -`Indicator Code`, -X64) %>% 
  pivot_longer(cols = `1960`:`2018`, names_to = "year", values_to = "life_expectancy") %>%
  mutate(year = as.numeric(year),
         cown = countrycode(`Country Code`, "wb", "cown"),
         country.name = countrycode(cown, "cown", "country.name")) %>%
  rename(wb = `Country Code`, 
         wb_name = `Country Name`) %>%
  glimpse()

# check non-identical names; seems okay
names_do_not_match <- le_df %>%
  group_by(wb_name, country.name) %>%
  summarize(n = n()) %>%
  filter(wb_name != country.name) %>%
  glimpse()

# clean life expectancy dataset ----
gdp_df_raw <- read_csv("raw-data/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_315877.csv", skip = 4) %>%
  glimpse()

gdp_df <- gdp_df_raw %>%
  select(-`Indicator Name`, -`Indicator Code`, -X64) %>% 
  pivot_longer(cols = `1960`:`2018`, names_to = "year", values_to = "gdp_per_capita") %>%
  mutate(year = as.numeric(year),
         cown = countrycode(`Country Code`, "wb", "cown"),
         country.name = countrycode(cown, "cown", "country.name")) %>%
  rename(wb = `Country Code`, 
         wb_name = `Country Name`) %>%
  glimpse()

# clean polity dataset ----
polity_df_raw <- read_xls("raw-data/p4v2018.xls") %>%
  glimpse()

polity_df <- polity_df_raw %>% 
  select(cown = ccode, year, democ, autoc) %>%
  glimpse()

# join datasets ----
df <- polity_df %>%
  left_join(le_df) %>% 
  left_join(gdp_df) %>%
  select(year, country_name = country.name, wb, cown, life_expectancy, gdp_per_capita, democ, autoc) %>%
  mutate(democ = na_if(democ, -88),
         democ = na_if(democ, -77),
         democ = na_if(democ, -66),
         autoc = na_if(autoc, -88),
         autoc = na_if(autoc, -77),
         autoc = na_if(autoc, -66)) %>%
  glimpse()

# write complete observations for 2016 to file 
df %>%
  drop_na(democ, autoc, life_expectancy, gdp_per_capita) %>%
  filter(year == 2016) %>%
  write_csv("data/democracy-life.csv")
