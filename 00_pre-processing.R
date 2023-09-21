if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(geodata)) install.packages("geodata", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readr)
library(leaflet)
library(leaflet.extras)
library(terra)
library(sf)
library(here)
library(DT)
library(geodata)

sites_raw <- read_rds(here("www", "sites_unfiltered_2km_2023-09-20.rds"))

column_order <- c("village_id", "village", "source", "longitude", "latitude", "lassa_occ", "num_buildings", "treecover_tot_area", "state", "LGA", "SenDist",
                  "shrubland_tot_area", "grassland_tot_area", "builtup_tot_area", "cropland_tot_area",
                  "treecover_patch_area", "shrubland_patch_area", "grassland_patch_area", "builtup_patch_area",  "cropland_patch_area",
                  "landscape_patch_area", "landscape_area_sd", 
                  "treecover_percent", "shrubland_percent", "grassland_percent", "builtup_percent", "cropland_percent",
                  "travel_time", "distance_to_road", "build_area")

sites_cleaned <- sites_raw %>%
  mutate(across(contains("percent"), \(x) round(x, digits = 2)),
         across(contains("area"), \(x) round(x, digits = 3)),
         build_area = round(build_area, 0),
         lassa_occ = round(lassa_occ, 4)) %>%
  select(all_of(column_order))

# Previously selected groups and clustering
current_groups <- bind_rows(
  tibble(group = rep(1, 9),
         cluster = c(rep("1A", 3), rep("1B", 4), rep("1C", 2)),
         village = c(c("Eja", "Ogamana", "Okunbongha"), c("Ndiweya", "Manden", "Akudfune", "Mgbumgangbom"), c("Abayono", "Abaribara"))),
  tibble(group = rep(2, 41),
         cluster = c(rep("2D", 7), rep("2E", 17), rep("2F", 7), rep("2I", 7), rep("2J", 3)),
         village = c(c("Shingile", "Wanagina", "Adum", "Anyogbe", "Nfom", "Owoleyeche", "Iduku"), 
                     c("Igberi", "Umuta", "Ikwo", "Opuitumo", "Nsobo", "Afiaelem", "Nko", "Eka", "Aru", "Ndilche Achara", "Agbanyim", "Otabo", "Mbode",
                       "Ebiom", "Akpanudele", "Udomowo", "Ohike"),
                     c("Ijibollo", "Apiapum Eja", "Obagu Ete", "Okum", "Ovoaba", "Azu Anyim", "Ijutum"),
                     c("Ndingele", "Olua", "Ominiyi", "Ominyi", "Amaguanyim", "Ndowa", "Ndowu"),
                     c("Ofianko", "Ndiogbokote", "Okponga"))),
  tibble(group = rep(3, 31),
         cluster = c(rep("3G", 7), rep("3H", 6), rep("3K", 4), rep("3L", 4), rep("3M", 5), rep("3N", 5)),
         village = c(c("Ugbala", "Ozante", "Ndiawala", "Izenyi", "Offianwe", "Isi Ohumini", "Onuenyim"),
                     c("Mkpumeakwaokoro", "Edenyaka", "Inyinba", "Ndibinaofia", "Ndibiofa", "Okpoduma"),
                     c("Ijilaga", "Ijibollo", "Odariko", "Ubeagu Ndikuda"),
                     c("Enyibichiri", "Elekpe", "Ndiofutu Echara", "Mgbabu"),
                     c("Okwaraka", "Ndialagu Akpu", "Akpu", "Agbara Oza", "Ndibulofia"),
                     c("Indingele", "Indiogogo", "Indigele", "Nwomila", "Indinwikwe")))
  ) %>%
  arrange(group, cluster, village) %>%
  mutate(cluster = fct_inorder(cluster),
         group = fct_inorder(as.character(group)))

# Information on Lassa seroprevalence
# Unsure how accurate these name matches are as there are some spelling differences but closest I could find

lassa_sero <- tibble(village = c(c("Okpuitomo", "Abakaliki", "Abofifia", "Ndiawala", "Agbaja", "Offianwe", "Ndiofutu"),
                                 c("Isieke", "Agalagu", "Amagu")),
                     lassa_seroprevalence = c(rep("High", 7),
                                              rep("Medium", 3))) %>%
  mutate(lassa_seroprevalence = factor(lassa_seroprevalence, levels = c("Medium", "High")))
                                              
# Bring this additional data into the base site data

sites_final <- sites_cleaned %>%
  left_join(current_groups) %>%
  left_join(lassa_sero) %>%
  filter(!(village == "Ijibollo" & treecover_percent == 0.24 & cluster == "2F")) %>%
  filter(!(village == "Ijibollo" & treecover_percent == 0.18 & cluster == "3K")) %>% # This village name is duplicated, remove the wrongly allocated cluster manually
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         distance_to_road = distance_to_road/1000)

write_rds(sites_final, here("www", "sites_final.rds"))

# Get Nigeria shapefiles

NGA_0 <- gadm("NGA", level = 0, path = here("www"))
NGA_1 <- gadm("NGA", level = 1, path = here("www"))
NGA_2 <- gadm("NGA", level = 2, path = here("www"))

