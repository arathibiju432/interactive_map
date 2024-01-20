# =============================================================================
# Project Overview:
# Interactive Map of High-Elevation Snow Stations and Proximity to Major Cities in Estonia
#
# Goal of the Project:
# The main aim of this project is to evaluate and visualize the spatial 
# relationship between high-elevation snow stations, characterized by substantial 
# snowfall duration, and major cities in Estonia. Using the interactive map, 
# which combines these stations with nearby major cities and overlays the 
# information on the counties layer, the project aims to identify patterns and 
# insights into the distribution of snow-related infrastructure. This analysis 
# becomes crucial in the context of emergency response planning.
#
#
# Data Source and Download Links: The download link provided in R lesson 5 was used
#   1. DEM layer of Estonia:
#    - Source: Estonian Land Board, 
#    - Download Link: https://geoportaal.maaamet.ee/eng/Maps-and-Data/Elevation-data/Download-Elevation-Data-p664.html
#    - Direct download link from lesson: https://aoraki.domenis.ut.ee/geopython-and-r/R_05/dem_est.tif
#   2. Counties of estonia:
#    - Source: Estonian Land Board
#    - Download Link: https://geoportaal.maaamet.ee/docs/haldus_asustus/maakond_shp.zip?t=20231201112848
#   3, Estonian cities 
#     - Source: Natural Earth
#     - The data is provided in the folder "Interactivemap_data", uploaded to moodle
#   4. Snow stations data:
#     - Source: R assignment 2 
#     - The data is provided in the folder "Interactivemap_data", uploaded to moodle
#
#
# Code Overview:
# The R code uses the Leaflet library to make an interactive map. The map 
# highlights high-elevation snow stations and their closeness to major cities 
# within a 50km radius, providing a valuable interface for exploring the spatial 
# context. Features such as a title, county boundaries, and markers for both snow 
# stations and cities are incorporated. The use of HTML widgets enables to  
# save the interactive map, facilitating user interaction to gain insights 
# into the geographic distribution of snow-related infrastructure and the 
# potential impact on emergency response scenarios.

# =============================================================================
# R code 

# Load all the necessary libraries
library(sf)
library(leaflet)
library(raster)
library(terra) 
library(dplyr)            
library(tidyverse)        
library(RColorBrewer)     
library(leaflet)          
library(htmlwidgets)      
library(htmltools)
library(leaflet.extras)   
library(leaflet.extras2)  
library(mapview)                   
library(xts)              
library(leafsync)         
library(viridis)          

# Read DEM Data
dem_est <- rast("dem_est.tif")  
dem_est # Check the data 

# Check CRS of the data 
terra::crs(dem_est, proj = TRUE)  # proj = TRUE returns only PROJ-string

# Reproject the raster to EPSG:3301
dem_est_proj <- project(dem_est, "+init=epsg:3301", method = "bilinear")

# Retrieve and print the CRS information with projection details
terra::crs(dem_est_proj, proj = TRUE)

# Read Snow Stations 
snow_stations <- st_read("snow_stations.gpkg")
snow_stations # Check the data 

# Read Cities as Shapefile
cities <- st_read("ne_10m_populated_places.shp")
cities # Check the data 

# Extract Estonian Cities
estonian_cities <- cities[cities$ADM0NAME == "Estonia", ]
estonian_cities # Check the data 

# Clean data to keep only required columns 
columns_to_keep <- c("NAMEASCII", "LATITUDE", "LONGITUDE", "POP_MAX")
estonian_cities <- estonian_cities[, columns_to_keep]

estonian_cities # View to check 

# Convert snow stations and estonian_cities to sf objects
snow_stations_sf <- st_as_sf(snow_stations, coords = c("longitude", "latitude"))
estonian_cities_sf <- st_as_sf(estonian_cities)
snow_stations_sf # Check the data

# Extract elevation values at station points
elev_values <- terra::extract(dem_est_proj, snow_stations_sf)

# Add elevation_values to snow_stations_sf object
snow_stations_sf <- snow_stations_sf %>%
  mutate(elev = elev_values$dem_est)

# Check if "stations_sf_elev" contains any missing values
anyNA(snow_stations_sf) 

snow_stations_sf # Check the data 

# Read counties data 
counties <- st_read("maakond_20231201.shp")

# Transform the snow stations and counties layer and estonian cities layer into WGS 84 
snow_stations_sf <- snow_stations_sf %>% st_transform(4326)
counties <- counties %>% st_transform(4326)
estonian_cities_sf <- estonian_cities_sf %>% st_transform(4326)

# Extract coordinates of snow stations from geometry column
coords <- st_coordinates(snow_stations_sf)
coords

# Add 'lat' and 'lon' columns to snow_stations_sf
snow_stations_sf <- cbind(snow_stations_sf, lat = coords[, "Y"], lon = coords[, "X"])
snow_stations_sf

# Set thresholds for snow duration and elevation and filter the snow stations.
filtered_stations_sf <- snow_stations_sf [snow_stations_sf$duration > 100 & snow_stations_sf$elev > 50, ]
filtered_stations_sf

# Create a 50km buffer around the snow stations
buffer <- st_buffer(filtered_stations_sf, dist = 50000)  # 50 km buffer

# View in map to visualize
mapview(buffer) + mapview(estonian_cities_sf)

# Perform spatial intersection
intersection_result <- st_intersection(estonian_cities_sf, buffer)

# Filter cities within the buffer
cities_within_buffer <- intersection_result[!is.na(intersection_result$geometry), ]
cities_within_buffer

# check to results
mapview(cities_within_buffer)

# Define a color palette of counties 
pal_d <- colorFactor(palette = "viridis", domain = counties$MNIMI)

# Define custom marker icons for cities 
city_icons <- awesomeIcons(
  icon = 'tower',   
  iconColor = 'green',   
  library = 'glyphicon', 
  markerColor = 'orange',
  squareMarker = TRUE
)

# Define custom marker icons for snow stations
snow_icons <- awesomeIcons(
  icon = 'ion-ios-snowy', 
  iconColor = 'white',
  markerColor = 'lightblue',
  library = 'ion')     

# Add a title to the map 
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Estonian cities within 50km radius snow stations having high snowfall duration and elevation, in Estonia")
)  

# Add map showing estonian cities within 50km radius of snow stations at high elevations 
# and high snowfall dutation in Estonia
m1 <- leaflet() %>%
  setView(lng = 26.73,lat = 58.38, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addControl(title, position = "topleft", className="map-title") %>%
  addPolygons(data = counties, fillColor = ~pal_d(counties$MNIMI), stroke = 1, opacity = 0.4) %>%
  addAwesomeMarkers(data = filtered_stations_sf, lng = ~lon, lat = ~lat, icon = snow_icons, 
                    label = ~as.character(duration)) %>% 
  addAwesomeMarkers(data = cities_within_buffer, lng = ~LONGITUDE, lat = ~LATITUDE, icon = city_icons, 
                    label = ~as.character(NAMEASCII)) %>%
  addMiniMap(width = 80, height = 80)
  
# Add map showing filtered snow stations based on snowfall duration
m2 <-mapview(filtered_stations_sf, cex = "duration", 
             col.regions = "darkblue", 
             alpha.region = 0.5, legend = TRUE,
             layer.name = "Snow Duration")

# Add map showing the estonian cities within the buffer based on their population
m3 <- mapview(cities_within_buffer, cex = "POP_MAX", 
              col.regions = "lightgreen", 
              alpha.region = 0.5, legend = TRUE,
              layer.name = "Population")

pal_s <- colorNumeric(palette = "viridis", domain = filtered_stations_sf$elev)

# Add map showing the filtered snow stations based on their elevation
m4 <- leaflet(filtered_stations_sf) %>%
  setView(lng = 26.73, lat = 58.38, zoom = 12) %>%
  addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%  
  addCircles(
    color = ~pal_s(elev), 
    radius = ~sqrt(elev) * 20, 
    label = ~as.character(station)) %>%
  addDrawToolbar() %>% # Add a draw toolbar for user interaction
  
  # Add a legend to the map
  leaflet::addLegend(
    pal = pal_s, 
    values = ~elev, 
    position = "topright",
    title = "Elevation")

# Synchronize the four maps using leafsync
leafsync::sync(m1, m2,m3,m4, ncol = 2)

# Save the map of Estonian cities within 50km radius of high elevation snow stations
saveWidget(widget = m1, file = "interactive_map.html", selfcontained = TRUE)
