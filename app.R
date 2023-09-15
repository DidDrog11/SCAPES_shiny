if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(conflicted)) install.packages("conflicted")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(cowplot)
library(readr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(ggiraph)
library(terra)
library(sf)
library(here)
library(DT)
library(conflicted)

conflict_prefer("pickerInput", "shinyWidgets")
conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("layout", "plotly")

# Data has been produced in an associated but separate repository
# Data is stored in /www and there is an associated R scrip 00_pre-processing.R that does some final cleaning outside of the app
site_df <- read_rds(here("www", "sites_final.rds")) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "EPSG:4326") %>%
  mutate(village_unique = paste0(village, ": ", village_id))
lc <- rast(here("www", "ds_processed_raster.tif"))

adm_0 <- vect(here("www", "gadm", "gadm41_NGA_0_pk.rds"))
adm_1 <- vect(here("www", "gadm", "gadm41_NGA_1_pk.rds"))
adm_2 <- vect(here("www", "gadm", "gadm41_NGA_2_pk.rds"))


# Define UI ---------------------------------------------------------------

ui <- dashboardPage(
  
  # Setting up the dashboard
  skin = "purple",
  
  dashboardHeader(title = "SCAPES Nigeria",
                  titleWidth = 450),
  # Setting up the sidebar
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Homepage", tabName = "homepage", icon = icon("passport")),
      menuItem("Site selection", icon = icon("house"),
               menuSubItem("Potential village sites", tabName = "village_sites"),
               menuSubItem("Group 1", tabName = "group_1", icon = icon("1")),
               menuSubItem("Group 2", tabName = "group_2", icon = icon("2")),
               menuSubItem("Group 3", tabName = "group_3", icon = icon("3")),
               menuSubItem("Produce site lists", tabName = "sitelists", icon = icon("arrow-down-1-9"))),
      menuItem("Rodent sampling simulations", tabName = "rodentsample", icon = icon("paw"))
    )
  ),
  
  
  # Homepage ----------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      # Homepage tab content
      tabItem(tabName = "homepage",
              fluidRow(
                
                box(width = 12,
                    h1("Introduction"),
                    p(HTML("This is a shiny app for several tasks and visualisations within the <a href=`https://www.saganfriant.com/risk-lab`>SCAPES</a> project.")),
                    p("The Site selection tab contains several visualisations and methods to identify the housing clusters included in the study. The first page, `Potential village sites` shows all of the identified villages in the study region. Data on village locations were obtained from the OpenStreetMap (OSM) project and the Geographic Names Server (GNS)."),
                    p(HTML("Within the `Potential village sites` tab filters can be used to show a subset of the returned villages. These filters include measures of the `Travel time to Abakaliki` in minutes (obtained from OSM), `Distance to main road` in km (obtained from OSM), the `number of buildings` (obtained from Google open data), and measures of proportion of different land use (from the ESA). Finally, the probability of Lassa occurrence has been estimated from a model produced by Redding et al. (<a href = 'https://www.nature.com/articles/s41467-021-25910-y'>link</a>). Sites can be filtered using these characteristics.")),
                    p("The basemap for the `Potential village sites` map can be switched between OSM and a satellite image. The landuse raster produced from the ESA image can be overlain to the area to visualise recorded landuse for the pixel. ")
                )
              )),
      
      
      ## All sites ---------------------------------------------------------
      
      tabItem(tabName = "village_sites",
              position = "left",
              sidebarPanel(
                sliderInput(inputId = "TravTime",
                            label = "Travel Time to Abakaliki (mins)",
                            min = 0,
                            step= 10,
                            max = 300,
                            value = c(0,150)),
                sliderInput(inputId = "RoadDist",
                            label = "Distance to main road (km)",
                            min = 0,
                            step= 0.5,
                            max = 50,
                            value = c(0,50)),
                sliderInput(inputId = "NumBuild",
                            label = "Number of buildings",
                            min = 0,
                            step= 50,
                            max = 18000,
                            value = c(0,1500)),
                sliderInput(inputId = "BuiltUpSelect", 
                            label = "Proportion Built-up", 
                            min = 0,
                            step = 0.05,
                            max = 1,
                            value = c(0,1)), 
                sliderInput(inputId = "TreeSelect", 
                            label = "Proportion Treecover", 
                            min = 0,
                            step = 0.05,
                            max = 1,
                            value = c(0,1)), 
                sliderInput(inputId = "CropSelect", 
                            label = "Proportion Cropland", 
                            min = 0,
                            step=0.05,
                            max = 1,
                            value = c(0,1)), 
                sliderInput(inputId = "GrassSelect", 
                            label = "Proportion Grassland", 
                            min = 0,
                            max = 1,
                            step=0.05,
                            value = c(0,1)),
                sliderInput(inputId = "ShrubSelect", 
                            label = "Proportion Shrubland", 
                            min = 0,
                            max = 1,
                            step=0.05,
                            value = c(0,1)),
                sliderInput(inputId = "LassaSelect", 
                            label = "Lassa occurrence probability", 
                            min = 0,
                            max = 1,
                            step=0.05,
                            value = c(0,1))
              ),
              mainPanel(
                
                leafletOutput("sitemap", height = "850px")
                
              )),
      
      
      ### Group 1 sites -----------------------------------------------------------
      
      tabItem(tabName = "group_1",
              fluidRow(
                box(width = 12,
                    h1("Group 1"),
                    p("Group 1 villages are those with a central nucleus of housing with farmland surrounding these sites. Villages are classified through containing a built-up patch area of >0.2km²", strong("and"), "a cropland patch area of <0.75km²."),
                    p("Group 1 currently contains three clusters 1A, 1B and 1C. The characteristics of group 1 villages are shown on the plots below.")
                )
              ),
              
              useShinyjs(),
              
              tags$style(HTML("
      .box-header {
        padding: 0 10px 0 0;
      }
      .box-header h3 {
        width: 100%;
        padding: 10px;
      }")
              ),
      
      #### Group 1 sites plots -----------------------------------------------------
      
      # Built up land plots
      fluidRow(
        box(width = 12,
            id="builtup", 
            title = "Proportion of Built-up land",
            p("Group 1 has a median builtup land percentage of ", median(site_df %>%
                                                                           filter(group == 1) %>%
                                                                           pull(builtup_percent)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(builtup_percent),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(builtup_percent),
                       3/4), "]. The patch area of builtup land is a median of ", median(site_df %>%
                                                                                           filter(group == 1) %>%
                                                                                           pull(builtup_patch_area)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(builtup_patch_area),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(builtup_patch_area),
                       3/4), "]. Current Group 1 sites are shown in the TRUE row with blue shading."),
            status = "danger", 
            solidHeader = TRUE, 
            collapsible = T,
            plotOutput("builtup_group1", height = 400)
        )
      ),
      
      # Cropland plots
      fluidRow(
        box(width = 12,
            id="cropland", 
            title = "Proportion of Cropland",
            p("Group 1 has a median cropland percentage of ", median(site_df %>%
                                                                       filter(group == 1) %>%
                                                                       pull(cropland_percent)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(cropland_percent),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(cropland_percent),
                       3/4), "]. The patch area of cropland is a median of ", median(site_df %>%
                                                                                       filter(group == 1) %>%
                                                                                       pull(cropland_patch_area)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(cropland_patch_area),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(cropland_patch_area),
                       3/4), "]. Current Group 1 sites are shown in the TRUE row with blue shading."),
            status = "danger", 
            solidHeader = TRUE, 
            collapsible = T,
            plotOutput("cropland_group1", height = 400)
        )
      ),
      
      # Grassland plots
      fluidRow(
        box(width = 12,
            id="grassland", 
            title = "Proportion of Grassland",
            p("Group 1 has a median grassland percentage of ", median(site_df %>%
                                                                        filter(group == 1) %>%
                                                                        pull(grassland_percent)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(grassland_percent),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(grassland_percent),
                       3/4), "]. The patch area of grassland is a median of ", median(site_df %>%
                                                                                        filter(group == 1) %>%
                                                                                        pull(grassland_patch_area)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(grassland_patch_area),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(grassland_patch_area),
                       3/4), "]. Current Group 1 sites are shown in the TRUE row with blue shading."),
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = T,
            plotOutput("grassland_group1", height = 400)
        )
      ),
      
      # Shrubland plots
      fluidRow(
        box(width = 12,
            id="shrubland", 
            title = "Proportion of Shrubland",
            p("Group 1 has a median shrubland percentage of ", median(site_df %>%
                                                                        filter(group == 1) %>%
                                                                        pull(shrubland_percent)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(shrubland_percent),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(shrubland_percent),
                       3/4), "]. The patch area of shrubland is a median of ", median(site_df %>%
                                                                                        filter(group == 1) %>%
                                                                                        pull(shrubland_patch_area)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(shrubland_patch_area),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(shrubland_patch_area),
                       3/4), "]. Current Group 1 sites are shown in the TRUE row with blue shading."),
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = T,
            plotOutput("shrubland_group1", height = 400)
        )
      ),
      
      # Treecover plots
      fluidRow(
        box(width = 12,
            id="treecover", 
            title = "Proportion of Tree-cover",
            p("Group 1 has a median treecover percentage of ", median(site_df %>%
                                                                        filter(group == 1) %>%
                                                                        pull(treecover_percent)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(treecover_percent),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(treecover_percent),
                       3/4), "]. The patch area of treecover is a median of ", median(site_df %>%
                                                                                        filter(group == 1) %>%
                                                                                        pull(treecover_patch_area)),
              " with an IQR of [", quantile(site_df %>%
                                              filter(group == 1) %>%
                                              pull(treecover_patch_area),
                                            1/4),
              "-", 
              quantile(site_df %>%
                         filter(group == 1) %>%
                         pull(treecover_patch_area),
                       3/4), "]. Current Group 1 sites are shown in the TRUE row with blue shading."),
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = T,
            plotOutput("treecover_group1", height = 400)
        )
      ),
      
      
      #### Group 1 sites map -------------------------------------------------------
      
      # Map of sites for Group 1 comparison
      fluidRow(
        box(width = 12,
            id = "map_group1",
            title = "Group 1 sites mapped",
            p("This map shows the location of currently identified Group 1 sites. Additional layers may be from the layers option within the map."),
            tags$ol(
              tags$li("Sites that match the primary characteristics of built-up patch area and cropland."),
              tags$li("Sites nearby (~5km) to those previously identified."),
              tags$li("Sites that match the primary characteristics within a defined distance (~5km)."),
              tags$li("Sites that fall within the min/max for the characteristics of Group 1 across all of built-up, cropland, grassland, shrubland and treecover.")
            ),
            leafletOutput("map_group1", height = "850px")
        ))
      ),
      
      ### Group 2 sites -----------------------------------------------------------
      tabItem(tabName = "group_2",
              
              fluidRow(
                box(width = 12,
                    h1("Group 2"),
                    p("Group 2 villages are those with a built-up patch area of <0.2km²", strong("and"), "a cropland patch area <2km² (at the cluster level)."),
                    p("Group 2 currently contains five clusters 2D, 2E, 2F, 2I and 2J. The characteristics of group 2 villages are shown on the plots below.")
                )),
              
              #### Group 2 sites plots -----------------------------------------------------
              # Built up land plots
              fluidRow(
                box(width = 12,
                    id="builtup", 
                    title = "Proportion of Built-up land",
                    p("Group 2 has a median builtup land percentage of ", median(site_df %>%
                                                                                   filter(group == 2) %>%
                                                                                   pull(builtup_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(builtup_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(builtup_percent),
                               3/4), "]. The patch area of builtup land is a median of ", median(site_df %>%
                                                                                                   filter(group == 2) %>%
                                                                                                   pull(builtup_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(builtup_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(builtup_patch_area),
                               3/4), "]. Current Group 2 sites are shown in the TRUE row with blue shading."),
                    status = "danger", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("builtup_group2", height = 400)
                )),
              
              # Cropland plots
              fluidRow(
                box(width = 12,
                    id="cropland", 
                    title = "Proportion of Cropland",
                    p("Group 2 has a median cropland percentage of ", median(site_df %>%
                                                                               filter(group == 2) %>%
                                                                               pull(cropland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(cropland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(cropland_percent),
                               3/4), "]. The patch area of cropland is a median of ", median(site_df %>%
                                                                                               filter(group == 2) %>%
                                                                                               pull(cropland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(cropland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(cropland_patch_area),
                               3/4), "]. Current Group 2 sites are shown in the TRUE row with blue shading."),
                    status = "danger", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("cropland_group2", height = 400)
                )),
              
              # Grassland plots
              fluidRow(
                box(width = 12,
                    id="grassland", 
                    title = "Proportion of Grassland",
                    p("Group 2 has a median grassland percentage of ", median(site_df %>%
                                                                                filter(group == 2) %>%
                                                                                pull(grassland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(grassland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(grassland_percent),
                               3/4), "]. The patch area of grassland is a median of ", median(site_df %>%
                                                                                                filter(group == 2) %>%
                                                                                                pull(grassland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(grassland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(grassland_patch_area),
                               3/4), "]. Current Group 2 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("grassland_group2", height = 400)
                )),
              
              # Shrubland plots
              fluidRow(
                box(width = 12,
                    id="shrubland", 
                    title = "Proportion of Shrubland",
                    p("Group 2 has a median shrubland percentage of ", median(site_df %>%
                                                                                filter(group == 2) %>%
                                                                                pull(shrubland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(shrubland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(shrubland_percent),
                               3/4), "]. The patch area of shrubland is a median of ", median(site_df %>%
                                                                                                filter(group == 2) %>%
                                                                                                pull(shrubland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(shrubland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(shrubland_patch_area),
                               3/4), "]. Current Group 2 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("shrubland_group2", height = 400)
                )),
              
              # Treecover plots
              fluidRow(
                box(width = 12,
                    id="treecover", 
                    title = "Proportion of Tree-cover",
                    p("Group 2 has a median treecover percentage of ", median(site_df %>%
                                                                                filter(group == 2) %>%
                                                                                pull(treecover_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(treecover_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(treecover_percent),
                               3/4), "]. The patch area of treecover is a median of ", median(site_df %>%
                                                                                                filter(group == 2) %>%
                                                                                                pull(treecover_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 2) %>%
                                                      pull(treecover_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 2) %>%
                                 pull(treecover_patch_area),
                               3/4), "]. Current Group 2 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("treecover_group2", height = 400)
                )),
              
              #### Group 2 sites map -------------------------------------------------------
              
              # Map of sites for Group 2 comparison
              fluidRow(
                box(width = 12,
                    id = "map_group2",
                    title = "Group 2 sites mapped",
                    p("This map shows the location of currently identified Group 2 sites. Additional layers may be from the layers option within the map."),
                    tags$ol(
                      tags$li("Sites that match the primary characteristics of built-up patch area and cropland (at the village level)."),
                      tags$li("Sites nearby (~5km) to those previously identified."),
                      tags$li("Sites that match the primary characteristics within a defined distance (~5km)."),
                      tags$li("Sites that fall within the min/max for the characteristics of Group 2 across all of built-up, cropland, grassland, shrubland and treecover.")
                    ),
                    leafletOutput("map_group2", height = "850px")
                ))
      ),
      
      
      ### Group 3 sites -----------------------------------------------------------
      
      tabItem(tabName = "group_3",
              fluidRow(
                box(width = 12,
                    h1("Group 3"),
                    p("Group 3 villages are those with a built-up patch area of <0.2km²", strong("and"), "a cropland patch area of >2km² (at the cluster level)."),
                    p("Group 3 currently contains six clusters 3G, 3H, 3K, 3L, 3M and 3N. The characteristics of group 3 villages are shown on the plots below.")
                )
              ),
              
              #### Group 3 sites plots -----------------------------------------------------
              # Built up land plots
              fluidRow(
                box(width = 12,
                    id="builtup", 
                    title = "Proportion of Built-up land",
                    p("Group 3 has a median builtup land percentage of ", median(site_df %>%
                                                                                   filter(group == 3) %>%
                                                                                   pull(builtup_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(builtup_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(builtup_percent),
                               3/4), "]. The patch area of builtup land is a median of ", median(site_df %>%
                                                                                                   filter(group == 3) %>%
                                                                                                   pull(builtup_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(builtup_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(builtup_patch_area),
                               3/4), "]. Current Group 3 sites are shown in the TRUE row with blue shading."),
                    status = "danger", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("builtup_group3", height = 400)
                )),
              
              # Cropland plots
              fluidRow(
                box(width = 12,
                    id="cropland", 
                    title = "Proportion of Cropland",
                    p("Group 3 has a median cropland percentage of ", median(site_df %>%
                                                                               filter(group == 3) %>%
                                                                               pull(cropland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(cropland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(cropland_percent),
                               3/4), "]. The patch area of cropland is a median of ", median(site_df %>%
                                                                                               filter(group == 3) %>%
                                                                                               pull(cropland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(cropland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(cropland_patch_area),
                               3/4), "]. Current Group 3 sites are shown in the TRUE row with blue shading."),
                    status = "danger", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("cropland_group3", height = 400)
                )),
              
              # Grassland plots
              fluidRow(
                box(width = 12,
                    id="grassland", 
                    title = "Proportion of Grassland",
                    p("Group 3 has a median grassland percentage of ", median(site_df %>%
                                                                                filter(group == 3) %>%
                                                                                pull(grassland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(grassland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(grassland_percent),
                               3/4), "]. The patch area of grassland is a median of ", median(site_df %>%
                                                                                                filter(group == 3) %>%
                                                                                                pull(grassland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(grassland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(grassland_patch_area),
                               3/4), "]. Current Group 3 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("grassland_group3", height = 400)
                )),
              
              # Shrubland plots
              fluidRow(
                box(width = 12,
                    id="shrubland", 
                    title = "Proportion of Shrubland",
                    p("Group 3 has a median shrubland percentage of ", median(site_df %>%
                                                                                filter(group == 3) %>%
                                                                                pull(shrubland_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(shrubland_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(shrubland_percent),
                               3/4), "]. The patch area of shrubland is a median of ", median(site_df %>%
                                                                                                filter(group == 3) %>%
                                                                                                pull(shrubland_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(shrubland_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(shrubland_patch_area),
                               3/4), "]. Current Group 3 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("shrubland_group3", height = 400)
                )),
              
              # Treecover plots
              fluidRow(
                box(width = 12,
                    id="treecover", 
                    title = "Proportion of Tree-cover",
                    p("Group 3 has a median treecover percentage of ", median(site_df %>%
                                                                                filter(group == 3) %>%
                                                                                pull(treecover_percent)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(treecover_percent),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(treecover_percent),
                               3/4), "]. The patch area of treecover is a median of ", median(site_df %>%
                                                                                                filter(group == 3) %>%
                                                                                                pull(treecover_patch_area)),
                      " with an IQR of [", quantile(site_df %>%
                                                      filter(group == 3) %>%
                                                      pull(treecover_patch_area),
                                                    1/4),
                      "-", 
                      quantile(site_df %>%
                                 filter(group == 3) %>%
                                 pull(treecover_patch_area),
                               3/4), "]. Current Group 3 sites are shown in the TRUE row with blue shading."),
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = T,
                    plotOutput("treecover_group3", height = 400)
                )),
              
              #### Group 3 sites map -------------------------------------------------------
              
              # Map of sites for Group 3 comparison
              fluidRow(
                box(width = 12,
                    id = "map_group3",
                    title = "Group 3 sites mapped",
                    p("This map shows the location of currently identified Group 2 sites. Additional layers may be from the layers option within the map."),
                    tags$ol(
                      tags$li("Sites that match the primary characteristics of built-up patch area and cropland (at the village level)."),
                      tags$li("Sites nearby (~5km) to those previously identified."),
                      tags$li("Sites that match the primary characteristics within a defined distance (~5km)."),
                      tags$li("Sites that fall within the min/max for the characteristics of Group 3 across all of built-up, cropland, grassland, shrubland and treecover.")
                    ),
                    leafletOutput("map_group3", height = "850px")
                ))
              
              
      ),
      
      
      ## Selecting sites tab -----------------------------------------------------
      
      tabItem(tabName = "sitelists",
              fluidRow(
                box(width = 12,
                    id = "sitelist-entry",
                    title = "Enter manually identified sites",
                    p("Select the sites to included and specify their grouping in the below sections. This will update the underlying data and will be plotted on the map below. Selections can then be made to update the potential sites that are closely identified with the index site.")),
                box(width = 12, 
                    title = "Define the focal villages for Group 1.",
                    selectizeInput(inputId = "group1_manual",
                                   label = "Select focal villages to be entered as Group 1.",
                                   choices = site_df$village_unique,
                                   selected = site_df$village_unique[str_detect(site_df$village_unique, "Ogamana|Mkpaya|Okunbongha|Afono")],
                                   options = list('actions-box' = TRUE),
                                   multiple = TRUE),
                    textOutput("manual_group1"),
                    sliderInput("radius_village", label = "Set the radius for inclusion of villages X km from these focal villages.",
                                min = 0,
                                max = 100,
                                value = c(0, 100),
                                step = 0.5),
                    collapsible = TRUE),
                box(width = 12,
                    title = "This map shows the focal and nearby villages selected above.",
                    collapsible = TRUE,
                    leafletOutput("manual_group1_map"),
                )
              )),
      
      
      ## Simulating rodent sampling ----------------------------------------------
      
      tabItem(tabName = "rodentsample",
              fluidRow(
                box(width = 12,
                    h1("Simulation of rodent sample size"),
                    p("Simulation of capture rate of Mastomys natalensis in Ebonyi, Cross River and Benue and a simulation of their seroprevalence for LASV."),
                    p("The simulation is based on the following assumptions."),
                    tags$ol(
                      tags$li("The rodent population is at equilibrium with no substantial changes in rodent abundance between sampling."),
                      tags$li("The rodent population is closed with no substantial immigration or emmigration."),
                      tags$li("All individuals have an equal chance of being captured and this is not impacted by seroprevalence to LASV or spatial structuring of the population across land use types."),
                      tags$li("The capture of non-Mastomys individuals is not associated with seroprevalence among the Mastomys population.")
                    ),
                    p("The starting conditions for the simulation can be modified using the below options. For reference the seroprevalence among Mastomys natalensis trapped in Ebonyi state was 41.6%. Although this was targetted sampling. Five other species were also found to be positive ranging from 31-100% of tested individuals. The trap success (or capture rate) for Mastomys in Ebonyi was 3% although this ranged from 0.6-7.8% across the state and whether the traps were inside households or fields."),
                    p("Previously used conditions include."),
                    tags$ol(
                      tags$li("Light touch: TN = 6,000, TS = 0.01, Seroprevalence = 0.05"),
                      tags$li("Ebonyi low prevalence: TN = 6,000, TS = 0.0125, Seroprevalence = 0.15"),
                      tags$li("Ebonyi high prevalence: TN = 6,000, TS = 0.0125, Seroprevalence = 0.48"),
                      tags$li("Ondo high prevalence: TN = 6,000, TS = 0.067, Seroprevalence = 0.45"),
                      tags$li("Cross River low prevalence: TN = 6,000, TS = 0.0125, Seroprevalence = 0.08"),
                      tags$li("Ebonyi low TS: TN = 6,000, TS = 0.005, Seroprevalence = 0.15"),
                      tags$li("Ebonyi moderate TS: TN = 6,000, TS = 0.01, Seroprevalence = 0.15"),
                      tags$li("Ebonyi high TS: TN = 6,000, TS = 0.08, Seroprevalence = 0.15")
                    ),
                    actionButton("load_results", "Load saved simulations")
                )),
              fluidRow(
                box(width = 12,
                    h2("Simulation settings"),
                    textInput("sim_name", "Simulation name:", ""))),
              fluidRow(box(width = 12,
                           column(width = 4, 
                                  numericInput("abundance", "Mastomys population size:", 100000)),
                           column(width = 4, 
                                  numericInput("trap_nights", "Number of trap nights:", 6000)),
                           column(width = 4, 
                                  numericInput("trap_success", "Proportion of traps set capturing a Mastomys (i.e., Percentage TS/100):", 0.03)),
                           column(width = 4,
                                           numericInput("seroprevalence", "Expected Mastomys seroprevalence:", 0.15)),
                           column(width = 4,
                                           numericInput("n_simulations", "Number of simulations to run:", 10000)),
                           actionButton("run_simulation", "Run Simulation"))),
              fluidRow(box(width = 12,
                           uiOutput("model_checkboxes"))),
              fluidRow(box(width = 12,
                           h2("Simulation results"),
                           DTOutput("simulation_results_table"))),
              fluidRow(box(width = 12,
                  plotlyOutput("simulation_results",
                             height = "850px")))
              )
      
    )
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {
  
  
  # Produce all sites map ---------------------------------------------------
  
  
  values <- reactiveValues(sites_final_filtered = site_df)
  
  observe({
    values$sites_final_filtered = site_df %>%
      filter(between(travel_time, input$TravTime[1], input$TravTime[2])) %>%
      filter(between(distance_to_road, input$RoadDist[1], input$RoadDist[2])) %>%
      filter(between(builtup_percent, input$BuiltUpSelect[1], input$BuiltUpSelect[2])) %>%
      filter(between(num_buildings, input$NumBuild[1], input$NumBuild[2])) %>%
      filter(between(treecover_percent, input$TreeSelect[1], input$TreeSelect[2])) %>%
      filter(between(cropland_percent, input$CropSelect[1], input$CropSelect[2])) %>%
      filter(between(grassland_percent, input$GrassSelect[1], input$GrassSelect[2])) %>%
      filter(between(shrubland_percent, input$ShrubSelect[1], input$ShrubSelect[2])) %>%
      filter(between(lassa_occ, input$LassaSelect[1], input$LassaSelect[2]))
  })
  
  labels <- reactive({
    paste0("<b>", values$sites_final_filtered$village,"</b>", "<br/>", 
           "Cluster: ", if_else(is.na(values$sites_final_filtered$cluster), "N/A", values$sites_final_filtered$cluster), "<br/>",  
           "Built-up: ", values$sites_final_filtered$builtup_percent*100, "%", "<br/>",
           "Buildings: ", values$sites_final_filtered$num_buildings, "<br/>",
           "Tree Cover: ", values$sites_final_filtered$treecover_percent*100, "%", "<br/>",
           "Cropland: ", values$sites_final_filtered$cropland_percent*100, "%", "<br/>",
           "Grassland: ", values$sites_final_filtered$grassland_percent*100, "%", "<br/>",
           "Shrublandland: ", values$sites_final_filtered$shrubland_percent*100, "%", "<br/>",
           "Lassa occ: ", round(values$sites_final_filtered$lassa_occ, 2))%>%
      lapply(htmltools::HTML)
  })
  
  output$sitemap <- renderLeaflet({ 
    
    pal <- colorFactor(palette = c("blue", "red", "orange"), values$sites_final_filtered$source)
    cluster_pal <- colorFactor(palette = "viridis", values$sites_final_filtered$cluster, na.color = "white")
    
    bounds <- values$sites_final_filtered %>% 
      st_bbox() %>% 
      as.character()
    
    leaflet(values$sites_final_filtered) %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
      addRasterImage(lc, opacity = 0.8, group = "Landuse raster") %>%
      addPolylines(data = adm_0 %>%
                     as.lines(),
                   weight = 1,
                   color = "white",
                   popup = c("Nigerian border"),
                   group = "Nigeria border") %>%
      addPolylines(data = adm_1 %>%
                     as.lines(),
                   weight = 2,
                   color = "white",
                   popup = c("Administrative level 1 borders"),
                   group = "Admin level 1") %>%
      addPolygons(data = adm_2,
                  weight = 2,
                  color = "gray",
                  fill = FALSE,
                  popup = ~ paste0("Local Authority: ", adm_2$NAME_2),
                  group = "Admin level 2") %>%
      addCircleMarkers(data = values$sites_final_filtered,
                       fillColor = ~cluster_pal(values$sites_final_filtered$cluster),
                       radius = 5,
                       stroke = TRUE,
                       weight = 0.5,
                       color = "black",
                       fillOpacity = 0.9,
                       popup = labels()) %>%
      addLayersControl(baseGroups = c("Satellite", "OSM"),
                       overlayGroups = c("Landuse raster", "Nigeria border", "Admin level 1", "Admin level 2")) %>%
      addLegend(pal = cluster_pal,
                values = values$sites_final_filtered$cluster,
                title = "Cluster") %>%
      hideGroup(c("Landuse raster", "Nigeria border", "Admin level 1", "Admin level 2")) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addScaleBar()
    
  })
  
  
  # Produce plots for landuse type Group 1 ------------------------------------------
  
  
  group_1_plot <- site_df %>%
    tibble() %>%
    select(-geometry) %>%
    mutate(group_1 = case_when(group == 1 ~ TRUE,
                               TRUE ~ FALSE)) %>%
    select(contains(c("village_id", "village", "cluster", "group_1", "num_buildings", "patch", "perc"))) %>%
    pivot_longer(cols = !contains(c("village", "cluster", "group_1")), names_to = "metric", values_to = "value") %>%
    mutate(metric = case_when(str_detect(metric, "patch") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "km^2^")),
                              str_detect(metric, "percent") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "%")),
                              str_detect(metric, "buildings") ~ paste("Buildings (n)"),
                              TRUE ~ "error")) %>%
    filter(!str_detect(metric, "Landscape")) %>%
    mutate(metric = factor(metric, levels = c("Builtup %", "Builtup km^2^", "Cropland %", "Cropland km^2^", "Grassland %", "Grassland km^2^",
                                              "Shrubland %", "Shrubland km^2^", "Treecover %", "Treecover km^2^", "Buildings (n)"))) %>%
    group_by(metric) %>%
    group_split() %>%
    lapply(., function(x) {
      
      ggplot(x) +
        geom_violin(aes(x = value, y = group_1, fill = group_1)) +
        geom_point(aes(x = value, y = group_1, colour = group_1), alpha = 0.3, position = position_jitter(height = 0.6, width = 0, seed = 123)) +
        labs(title = unique(x$metric),
             y = "Group 1",
             x = if(str_detect(unique(x$metric), "%")) "Proportion" else if(str_detect(unique(x$metric), "km")) "Area" else "N") +
        theme_bw() +
        theme(legend.position = "none")
      
    })
  
  output$builtup_group1 <- renderPlot({
    plot_grid(plotlist = list(group_1_plot[[1]], group_1_plot[[2]] +
                                geom_vline(aes(xintercept = 0.2))))
  })
  
  output$cropland_group1 <- renderPlot({
    plot_grid(plotlist = list(group_1_plot[[3]], group_1_plot[[4]] +
                                geom_vline(aes(xintercept = 0.75))))
  })
  
  output$grassland_group1 <- renderPlot({
    plot_grid(plotlist = list(group_1_plot[[5]], group_1_plot[[6]]))
  })
  
  output$shrubland_group1 <- renderPlot({
    plot_grid(plotlist = list(group_1_plot[[7]], group_1_plot[[8]]))
  })
  
  output$treecover_group1 <- renderPlot({
    plot_grid(plotlist = list(group_1_plot[[9]], group_1_plot[[10]]))
  })
  
  runjs("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
  
  
  # Produce map for Group 1 sites -------------------------------------------
  
  
  output$map_group1 <- renderLeaflet({
    
    group_1_labels <- function(site) {
      paste0("<b>", site$village,"</b>", "<br/>", 
             "Cluster: ", if_else(is.na(site$cluster), "N/A", site$cluster), "<br/>",  
             "Built-up: ", site$builtup_percent*100, "%", "<br/>",
             "Buildings: ", site$num_buildings, "<br/>",
             "Tree Cover: ", site$treecover_percent*100, "%", "<br/>",
             "Cropland: ", site$cropland_percent*100, "%", "<br/>",
             "Grassland: ", site$grassland_percent*100, "%", "<br/>",
             "Shrubland: ", site$shrubland_percent*100, "%", "<br/>",
             "Lassa occ: ", round(site$lassa_occ, 2))%>%
        lapply(htmltools::HTML)
    }
    
    group_1 <- site_df %>%
      filter(group == 1)
    
    group_1_buffer <- group_1 %>%
      st_buffer(dist = 5000)
    
    group_1_1 <- site_df %>%
      filter((group != 1 | is.na(group))) %>%
      filter((builtup_patch_area > 0.2 & cropland_patch_area < 0.75))
    
    group_1_2 <- site_df %>%
      st_filter(group_1_buffer, .predicate = st_within)
    
    group_1_3 <- site_df %>%
      filter((group != 1 | is.na(group))) %>%
      filter((builtup_patch_area > 0.2 & cropland_patch_area < 0.75)) %>%
      st_filter(group_1_buffer, .predicate = st_within)
    
    group_1_4 <- site_df %>%
      filter((group != 1 | is.na(group))) %>%
      filter((builtup_patch_area >= min(group_1$builtup_patch_area) & builtup_patch_area <= max(group_1$builtup_patch_area))) %>%
      filter((cropland_patch_area >= min(group_1$cropland_patch_area) & cropland_patch_area <= max(group_1$cropland_patch_area))) %>%
      filter((grassland_patch_area >= min(group_1$grassland_patch_area) & grassland_patch_area <= max(group_1$grassland_patch_area))) %>%
      filter((shrubland_patch_area >= min(group_1$shrubland_patch_area) & shrubland_patch_area <= max(group_1$shrubland_patch_area))) %>%
      filter((treecover_patch_area >= min(group_1$treecover_patch_area) & treecover_patch_area <= max(group_1$treecover_patch_area)))
    
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
      addRasterImage(lc, opacity = 0.8, group = "Landuse raster") %>%
      addCircleMarkers(group = "Current Group 1",
                       data = group_1,
                       radius = 4,
                       fillColor = "white",
                       fillOpacity = 1,
                       popup = group_1_labels(group_1),
                       stroke = TRUE,
                       color = "black",
                       weight = 2) %>%
      addCircleMarkers(group = "1) Meet Group 1 - builtup, cropland",
                       data = group_1_1,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_1_labels(group_1_1),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "2) Within 5km of current Group 1",
                       data = group_1_2,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_1_labels(group_1_2),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "3) Meet Group 1 - builtup, cropland, within 5km",
                       data = group_1_3,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_1_labels(group_1_3),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "4) Fall within min/max Group 1 - builtup, cropland, grassland, shrubland, treecover",
                       data = group_1_4,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_1_labels(group_1_4),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addLayersControl(baseGroups = c("Satellite", "OSM"),
                       overlayGroups = c("Landuse raster", "Current Group 1", "1) Meet Group 1 - builtup, cropland", "2) Within 5km of current Group 1", "3) Meet Group 1 - builtup, cropland, within 5km", "4) Fall within min/max Group 1 - builtup, cropland, grassland, shrubland, treecover")) %>%
      hideGroup(c("Landuse raster", "1) Meet Group 1 - builtup, cropland", "2) Within 5km of current Group 1", "3) Meet Group 1 - builtup, cropland, within 5km", "4) Fall within min/max Group 1 - builtup, cropland, grassland, shrubland, treecover")) %>%
      addScaleBar()
    
    
  })
  
  
  # Produce plots for landuse Group 2 ---------------------------------------
  group_2_plot <- site_df %>%
    tibble() %>%
    filter(travel_time >= 30) %>%
    filter(distance_to_road >= 1.5) %>%
    # filter(builtup_percent >= 0.1) %>%
    select(-geometry) %>%
    mutate(group_2 = case_when(group == 2 ~ TRUE,
                               TRUE ~ FALSE)) %>%
    select(contains(c("village_id", "village", "cluster", "group_2", "num_buildings", "patch", "perc"))) %>%
    pivot_longer(cols = !contains(c("village", "cluster", "group_2")), names_to = "metric", values_to = "value") %>%
    mutate(metric = case_when(str_detect(metric, "patch") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "km^2^")),
                              str_detect(metric, "percent") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "%")),
                              str_detect(metric, "buildings") ~ paste("Buildings (n)"),
                              TRUE ~ "error")) %>%
    filter(!str_detect(metric, "Landscape")) %>%
    mutate(metric = factor(metric, levels = c("Builtup %", "Builtup km^2^", "Cropland %", "Cropland km^2^", "Grassland %", "Grassland km^2^",
                                              "Shrubland %", "Shrubland km^2^", "Treecover %", "Treecover km^2^", "Buildings (n)"))) %>%
    group_by(metric) %>%
    group_split() %>%
    lapply(., function(x) {
      
      ggplot(x) +
        geom_violin(aes(x = value, y = group_2, fill = group_2)) +
        geom_point(aes(x = value, y = group_2, colour = group_2), alpha = 0.3, position = position_jitter(height = 0.6, width = 0, seed = 123)) +
        labs(title = unique(x$metric),
             y = "Group 2",
             x = if(str_detect(unique(x$metric), "%")) "Proportion" else if(str_detect(unique(x$metric), "km")) "Area" else "N") +
        theme_bw() +
        theme(legend.position = "none")
      
    })
  
  output$builtup_group2 <- renderPlot({
    plot_grid(plotlist = list(group_2_plot[[1]], group_2_plot[[2]] +
                                geom_vline(aes(xintercept = 0.2))))
  })
  
  output$cropland_group2 <- renderPlot({
    plot_grid(plotlist = list(group_2_plot[[3]], group_2_plot[[4]] +
                                geom_vline(aes(xintercept = 2))))
  })
  
  output$grassland_group2 <- renderPlot({
    plot_grid(plotlist = list(group_2_plot[[5]], group_2_plot[[6]]))
  })
  
  output$shrubland_group2 <- renderPlot({
    plot_grid(plotlist = list(group_2_plot[[7]], group_2_plot[[8]]))
  })
  
  output$treecover_group2 <- renderPlot({
    plot_grid(plotlist = list(group_2_plot[[9]], group_2_plot[[10]]))
  })
  
  
  
  # Produce map for Group 2 sites -------------------------------------------
  
  output$map_group2 <- renderLeaflet({
    
    group_2_labels <- function(site) {
      paste0("<b>", site$village,"</b>", "<br/>", 
             "Cluster: ", if_else(is.na(site$cluster), "N/A", site$cluster), "<br/>",  
             "Built-up: ", site$builtup_percent*100, "%", "<br/>",
             "Buildings: ", site$num_buildings, "<br/>",
             "Tree Cover: ", site$treecover_percent*100, "%", "<br/>",
             "Cropland: ", site$cropland_percent*100, "%", "<br/>",
             "Grassland: ", site$grassland_percent*100, "%", "<br/>",
             "Shrubland: ", site$shrubland_percent*100, "%", "<br/>",
             "Lassa occ: ", round(site$lassa_occ, 2))%>%
        lapply(htmltools::HTML)
    }
    
    group_2 <- site_df %>%
      filter(travel_time >= 30) %>%
      filter(distance_to_road >= 1.5) %>%
      filter(group == 2)
    
    group_2_buffer <- group_2 %>%
      st_buffer(dist = 5000)
    
    group_2_1 <- site_df %>%
      filter(travel_time >= 30) %>%
      filter(distance_to_road >= 1.5) %>%
      filter((group != 2 | is.na(group))) %>%
      filter((builtup_patch_area < 0.2 & cropland_patch_area < 2))
    
    group_2_2 <- site_df %>%
      filter(travel_time >= 30) %>%
      filter(distance_to_road >= 1.5) %>%
      filter(builtup_percent <= 0.2) %>%
      st_filter(group_2_buffer, .predicate = st_within)
    
    group_2_3 <- site_df %>%
      filter(travel_time >= 30) %>%
      filter(distance_to_road >= 1.5) %>%
      filter((group != 2 | is.na(group))) %>%
      filter((builtup_patch_area < 0.2 & cropland_patch_area < 2)) %>%
      st_filter(group_2_buffer, .predicate = st_within)
    
    group_2_4 <- site_df %>%
      filter(travel_time >= 30) %>%
      filter(distance_to_road >= 1.5) %>%
      filter((group != 2 | is.na(group))) %>%
      filter((builtup_patch_area >= min(group_2$builtup_patch_area) & builtup_patch_area <= max(group_2$builtup_patch_area))) %>%
      filter((cropland_patch_area >= min(group_2$cropland_patch_area) & cropland_patch_area <= max(group_2$cropland_patch_area))) %>%
      filter((grassland_patch_area >= min(group_2$grassland_patch_area) & grassland_patch_area <= max(group_2$grassland_patch_area))) %>%
      filter((shrubland_patch_area >= min(group_2$shrubland_patch_area) & shrubland_patch_area <= max(group_2$shrubland_patch_area))) %>%
      filter((treecover_patch_area >= min(group_2$treecover_patch_area) & treecover_patch_area <= max(group_2$treecover_patch_area)))
    
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
      addRasterImage(lc, opacity = 0.8, group = "Landuse raster") %>%
      addCircleMarkers(group = "Current Group 2",
                       data = group_2,
                       radius = 4,
                       fillColor = "white",
                       fillOpacity = 1,
                       popup = group_2_labels(group_2),
                       stroke = TRUE,
                       color = "black",
                       weight = 2) %>%
      addCircleMarkers(group = "1) Meet Group 2 - builtup, cropland",
                       data = group_2_1,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_2_labels(group_2_1),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "2) Within 5km of current Group 2",
                       data = group_2_2,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_2_labels(group_2_2),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "3) Meet Group 2 - builtup, cropland, within 5km",
                       data = group_2_3,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_2_labels(group_2_3),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "4) Fall within min/max Group 2 - builtup, cropland, grassland, shrubland, treecover",
                       data = group_2_4,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_2_labels(group_2_4),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addLayersControl(baseGroups = c("Satellite", "OSM"),
                       overlayGroups = c("Landuse raster", "Current Group 2", "1) Meet Group 2 - builtup, cropland", "2) Within 5km of current Group 2", "3) Meet Group 2 - builtup, cropland, within 5km", "4) Fall within min/max Group 2 - builtup, cropland, grassland, shrubland, treecover")) %>%
      hideGroup(c("Landuse raster", "1) Meet Group 2 - builtup, cropland", "2) Within 5km of current Group 2", "3) Meet Group 2 - builtup, cropland, within 5km", "4) Fall within min/max Group 2 - builtup, cropland, grassland, shrubland, treecover")) %>%
      addScaleBar()
    
    
  })
  
  # Produce plots for landuse Group 3 ---------------------------------------
  group_3_plot <- site_df %>%
    tibble() %>%
    select(-geometry) %>%
    mutate(group_3 = case_when(group == 3 ~ TRUE,
                               TRUE ~ FALSE)) %>%
    select(contains(c("village_id", "village", "cluster", "group_3", "num_buildings", "patch", "perc"))) %>%
    pivot_longer(cols = !contains(c("village", "cluster", "group_3")), names_to = "metric", values_to = "value") %>%
    mutate(metric = case_when(str_detect(metric, "patch") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "km^2^")),
                              str_detect(metric, "percent") ~ str_to_sentence(paste(str_split(metric, "_", simplify = TRUE)[, 1], "%")),
                              str_detect(metric, "buildings") ~ paste("Buildings (n)"),
                              TRUE ~ "error")) %>%
    filter(!str_detect(metric, "Landscape")) %>%
    mutate(metric = factor(metric, levels = c("Builtup %", "Builtup km^2^", "Cropland %", "Cropland km^2^", "Grassland %", "Grassland km^2^",
                                              "Shrubland %", "Shrubland km^2^", "Treecover %", "Treecover km^2^", "Buildings (n)"))) %>%
    group_by(metric) %>%
    group_split() %>%
    lapply(., function(x) {
      
      ggplot(x) +
        geom_point(aes(x = value, y = group_3, colour = group_3), alpha = 0.3, position = position_jitter(height = 0.6, width = 0, seed = 123)) +
        labs(title = unique(x$metric),
             y = "Group 3",
             x = if(str_detect(unique(x$metric), "%")) "Proportion" else if(str_detect(unique(x$metric), "km")) "Area" else "N") +
        theme_bw() +
        theme(legend.position = "none")
      
    })
  
  output$builtup_group3 <- renderPlot({
    plot_grid(plotlist = list(group_3_plot[[1]], group_3_plot[[2]] +
                                geom_vline(aes(xintercept = 0.2))))
  })
  
  output$cropland_group3 <- renderPlot({
    plot_grid(plotlist = list(group_3_plot[[3]], group_3_plot[[4]] +
                                geom_vline(aes(xintercept = 2))))
  })
  
  output$grassland_group3 <- renderPlot({
    plot_grid(plotlist = list(group_3_plot[[5]], group_3_plot[[6]]))
  })
  
  output$shrubland_group3 <- renderPlot({
    plot_grid(plotlist = list(group_3_plot[[7]], group_3_plot[[8]]))
  })
  
  output$treecover_group3 <- renderPlot({
    plot_grid(plotlist = list(group_3_plot[[9]], group_3_plot[[10]]))
  })
  
  
  
  # Produce map for Group 3 sites -------------------------------------------
  
  output$map_group3 <- renderLeaflet({
    
    group_3_labels <- function(site) {
      paste0("<b>", site$village,"</b>", "<br/>", 
             "Cluster: ", if_else(is.na(site$cluster), "N/A", site$cluster), "<br/>",  
             "Built-up: ", site$builtup_percent*100, "%", "<br/>",
             "Buildings: ", site$num_buildings, "<br/>",
             "Tree Cover: ", site$treecover_percent*100, "%", "<br/>",
             "Cropland: ", site$cropland_percent*100, "%", "<br/>",
             "Grassland: ", site$grassland_percent*100, "%", "<br/>",
             "Shrubland: ", site$shrubland_percent*100, "%", "<br/>",
             "Lassa occ: ", round(site$lassa_occ, 2))%>%
        lapply(htmltools::HTML)
    }
    
    group_3 <- site_df %>%
      filter(group == 3)
    
    group_3_buffer <- group_3 %>%
      st_buffer(dist = 5000)
    
    group_3_1 <- site_df %>%
      filter((group != 3 | is.na(group))) %>%
      filter((builtup_patch_area < 0.2 & cropland_patch_area > 2))
    
    group_3_3 <- site_df %>%
      st_filter(group_3_buffer, .predicate = st_within)
    
    group_3_3 <- site_df %>%
      filter((group != 3 | is.na(group))) %>%
      filter((builtup_patch_area < 0.2 & cropland_patch_area > 2)) %>%
      st_filter(group_3_buffer, .predicate = st_within)
    
    group_3_4 <- site_df %>%
      filter((group != 3 | is.na(group))) %>%
      filter((builtup_patch_area >= min(group_3$builtup_patch_area) & builtup_patch_area <= max(group_3$builtup_patch_area))) %>%
      filter((cropland_patch_area >= min(group_3$cropland_patch_area) & cropland_patch_area <= max(group_3$cropland_patch_area))) %>%
      filter((grassland_patch_area >= min(group_3$grassland_patch_area) & grassland_patch_area <= max(group_3$grassland_patch_area))) %>%
      filter((shrubland_patch_area >= min(group_3$shrubland_patch_area) & shrubland_patch_area <= max(group_3$shrubland_patch_area))) %>%
      filter((treecover_patch_area >= min(group_3$treecover_patch_area) & treecover_patch_area <= max(group_3$treecover_patch_area)))
    
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
      addRasterImage(lc, opacity = 0.8, group = "Landuse raster") %>%
      addCircleMarkers(group = "Current Group 3",
                       data = group_3,
                       radius = 4,
                       fillColor = "white",
                       fillOpacity = 1,
                       popup = group_3_labels(group_3),
                       stroke = TRUE,
                       color = "black",
                       weight = 2) %>%
      addCircleMarkers(group = "1) Meet Group 3 - builtup, cropland",
                       data = group_3_1,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_3_labels(group_3_1),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "2) Within 5km of current Group 3",
                       data = group_3_3,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_3_labels(group_3_3),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "3) Meet Group 3 - builtup, cropland, within 5km",
                       data = group_3_3,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_3_labels(group_3_3),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addCircleMarkers(group = "4) Fall within min/max Group 3 - builtup, cropland, grassland, shrubland, treecover",
                       data = group_3_4,
                       radius = 3,
                       fillColor = "black",
                       fillOpacity = 1,
                       popup = group_3_labels(group_3_4),
                       stroke = TRUE,
                       color = "white",
                       weight = 2) %>%
      addLayersControl(baseGroups = c("Satellite", "OSM"),
                       overlayGroups = c("Landuse raster", "Current Group 3", "1) Meet Group 3 - builtup, cropland", "2) Within 5km of current Group 3", "3) Meet Group 3 - builtup, cropland, within 5km", "4) Fall within min/max Group 3 - builtup, cropland, grassland, shrubland, treecover")) %>%
      hideGroup(c("Landuse raster", "1) Meet Group 3 - builtup, cropland", "2) Within 5km of current Group 3", "3) Meet Group 3 - builtup, cropland, within 5km", "4) Fall within min/max Group 3 - builtup, cropland, grassland, shrubland, treecover")) %>%
      addScaleBar()
    
    
  })
  
  
  # Selecting sites tab -----------------------------------------------------
  
  output$manual_group1 <- renderPrint({
    cat("Selected Group 1 focal villages: ")
    cat(paste0(input$group1_manual, sep = ", "))
  })
  
  get_village_colour_palette <- function(num_selected) {
    colour_palette <- viridis::viridis_pal()(num_selected)
  }
  
  calculate_distances <- function(focal_villages_1_df) {
    
    focal_villages_1_df %>%
      group_by(village_unique) %>%
      group_split() %>%
      lapply(., function(x) {
        
        focal_village_unique = unique(x$village_unique)
        focal_location = x %>%
          filter(village_unique == focal_village_unique) %>%
          select(village, geometry)
        
        site_df %>%
          mutate(distance_from_focal = as.numeric(st_distance(site_df$geometry, focal_location$geometry)),
                 focal_village_unique = focal_village_unique)
        
      }) %>%
      bind_rows()
    
  }
  
  observeEvent(input$group1_manual, {
    
    focal_villages_1 <- input$group1_manual
    
    num_selected <- length(focal_villages_1)
    colours <- get_village_colour_palette(num_selected)
    
    focal_villages_1_df <- site_df[site_df$village_unique %in% input$group1_manual, ]
    
    nearby_villages_df <- calculate_distances(focal_villages_1_df) %>%
      filter(between(distance_from_focal, input$radius_village[1], input$radius_village[2]))
    
    output$manual_group1_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = focal_villages_1_df,
                         fill = TRUE,  # Enable fill for markers
                         fillOpacity = 1,  # Full opacity
                         fillColor = ~colours,  # Assign colours to fill
                         stroke = FALSE,
                         radius = 2
        ) %>%
        addCircleMarkers(data = nearby_villages_df,
                         fill = TRUE,  # Enable fill for markers
                         fillOpacity = 1,  # Full opacity
                         fillColor = "black",  # Default colour for unselected villages
                         radius = 0.5,
                         stroke = FALSE
        )
    })
    
  })
  
  
  
  # Initialize the Leaflet map with all selected villages
  output$manual_group1_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = site_df,
                       fill = TRUE,  # Enable fill for markers
                       fillOpacity = 1,  # Full opacity
                       fillColor = "black",  # Default colour for unselected villages
                       radius = 0.5
      )
  })
  
  
  # Simulating rodent sampling ----------------------------------------------
  #' @param seroprevalence expected seroprevalence in simulated population
  #' @param trap_nights number of trap nights
  #' @param trap_success trap success Mastomys natalensis (measured as number rodents/number nights)
  rodentSimulator = function(seroprevalence, abundance = 100000, trap_nights, trap_success, name = "rodent_trapping", simulations = 10000) {
    
    # rodent pop (n = abundance, prob = seroprev)
    # produce seroprevalence status for the rodent population
    rodent_population = rbinom(n = abundance, size = 1, prob = seroprevalence)
    
    # n mastomys sampled
    total_rodents_trapped = round(trap_nights * trap_success)
    
    # run the simulator
    result = c()
    for(i in 1:simulations) {
      sx = sample(rodent_population, total_rodents_trapped, replace = FALSE)
      seropositivity_rate = sum(sx)/length(sx)
      result = c(result, seropositivity_rate)
    }
    
    # dataframe
    result_df = data.frame(seroprevalence = seroprevalence,
                           trap_nights = trap_nights,
                           trap_success = trap_success,
                           total_trapped = total_rodents_trapped,
                           sim_seropos = result,
                           name = name)
    return(result_df)
    
  }
  
  results <- reactiveVal(data.frame(
    seroprevalence = numeric(),
    trap_nights = numeric(),
    trap_success = numeric(),
    total_trapped = numeric(),
    sim_seropos = numeric(),
    name = character()
  ))
  
  # Store selected model names
  selected_models <- reactiveVal(character(0))
  
  
  observeEvent(input$run_simulation, {
    # disable run simulation button
    shinyjs::disable("run_simulation")
    
    sim_name <- paste(input$sim_name, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    sim_name <- gsub("[: ]", "_", sim_name)  # Replace invalid characters
    abundance <- input$abundance
    trap_nights <- input$trap_nights
    trap_success <- input$trap_success
    n_simulations <- input$n_simulations
    seroprevalence <- input$seroprevalence
    
    simulate_asynchronously <- function() {
      
      simulation_results <- rodentSimulator(seroprevalence = seroprevalence,
                                            abundance = abundance,
                                            trap_nights = trap_nights,
                                            trap_success = trap_success,
                                            name = sim_name,
                                            simulations = n_simulations)
      
      updated_results <- rbind(results(), simulation_results)
      
      # Update the reactiveVal
      results(updated_results)
      
      # enable run simulation button
      shinyjs::enable("run_simulation")
    }
    
    isolate(simulate_asynchronously())
  })
  
  observeEvent(input$load_results, {
    
    # Read the saved results from the CSV file
    loaded_results <- read.csv(here("www", "saved_simulations.csv"), stringsAsFactors = FALSE)
    
    # Update the results with the loaded data
    results(loaded_results)
  })
  
  output$model_checkboxes <- renderUI({
    checkboxGroupInput("selected_models", "Select Models", choices = unique(results()$name))
  })
  
  # Filter the data based on selected models
  selected_data <- reactive({
    if (length(input$selected_models) > 0) {
      results() %>%
        filter(name %in% input$selected_models)
    } else {
      NULL
    }
  })
  
  output$simulation_results_table <- renderDT({
    
    results() %>%
      mutate(modified_names = sub("_(\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2})", "", name)) %>%
      group_by(modified_names, name) %>%
      summarise(`Model name` = unique(modified_names),
                `Expected seroprevalence` = unique(seroprevalence),
                TN = unique(trap_nights),
                TS = unique(trap_success),
                `Mnat sampled` = unique(total_trapped),
                `mean observed seroprev` = round(mean(sim_seropos), 4),
                `sd observed seroprev` = round(sd(sim_seropos), 4)) %>%
      ungroup() %>%
      select(-c("modified_names", "name"))
    
    
  })
  
  output$simulation_results <- renderPlotly({
    
    if (!is.null(selected_data())) {
      
      summary_data <- selected_data() %>%
        mutate(modified_names = sub("_(\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2})", "", name)) %>%
        group_by(modified_names)
      
      plot_ly(data = summary_data, x = ~modified_names, y = ~sim_seropos, color = ~modified_names, type = "box") %>%
        layout(title = "Simulation Results",
               xaxis = list(title = "Model"),
               yaxis = list(title = "Seropositivity Rate"),
               hovermode = "closest")
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
