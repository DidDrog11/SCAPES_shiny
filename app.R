if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(cowplot)
library(readr)
library(leaflet)
library(leaflet.extras)
library(ggiraph)
library(terra)
library(sf)
library(here)
library(DT)

# Data has been produced in an associated but separate repository
# Data is stored in /www and there is an associated R scrip 00_pre-processing.R that does some final cleaning outside of the app
site_df <- read_rds(here("www", "sites_final.rds")) %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = "EPSG:4326")
lc <- rast(here("www", "ds_processed_raster.tif"))

adm_0 <- vect(here("www", "gadm", "gadm41_NGA_0_pk.rds"))
adm_1 <- vect(here("www", "gadm", "gadm41_NGA_1_pk.rds"))
adm_2 <- vect(here("www", "gadm", "gadm41_NGA_2_pk.rds"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title = "SCAPES Nigeria",
                    titleWidth = 450),
    
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            menuItem("Homepage", tabName = "homepage", icon = icon("passport")),
            menuItem("Site selection", icon = icon("house"),
                     menuSubItem("Potential village sites", tabName = "village_sites"),
                     menuSubItem("Group 1", tabName = "group_1"))
        )
    ),
    
    
    ## Homepage ----------------------------------------------------------------
    
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
            
            
            ## Village sites ---------------------------------------------------------
            
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
            )
      
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
    
    output$map_group1 <- renderLeaflet({
        
        group_1_labels <- function(site) {
            paste0("<b>", site$village,"</b>", "<br/>", 
                   "Cluster: ", if_else(is.na(site$cluster), "N/A", site$cluster), "<br/>",  
                   "Built-up: ", site$builtup_percent*100, "%", "<br/>",
                   "Buildings: ", site$num_buildings, "<br/>",
                   "Tree Cover: ", site$treecover_percent*100, "%", "<br/>",
                   "Cropland: ", site$cropland_percent*100, "%", "<br/>",
                   "Grassland: ", site$grassland_percent*100, "%", "<br/>",
                   "Shrublandland: ", site$shrubland_percent*100, "%", "<br/>",
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
                             overlayGroups = c("Current Group 1", "1) Meet Group 1 - builtup, cropland", "2) Within 5km of current Group 1", "3) Meet Group 1 - builtup, cropland, within 5km", "4) Fall within min/max Group 1 - builtup, cropland, grassland, shrubland, treecover")) %>%
            hideGroup(c("1) Meet Group 1 - builtup, cropland", "2) Within 5km of current Group 1", "3) Meet Group 1 - builtup, cropland, within 5km", "4) Fall within min/max Group 1 - builtup, cropland, grassland, shrubland, treecover")) %>%
            addScaleBar()
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
