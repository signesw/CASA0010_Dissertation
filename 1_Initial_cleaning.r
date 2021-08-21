library(geojsonio)
library(here)
library(tidyverse)
library(geojson)
library(sf)
library(tmap)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(sp)
library(dplyr)
library(janitor)
library(caret)
library(ggspatial)
library(ggmap)
library(rgeos)
library(sp)

#---------------------------------------Loading data--------------------- -------------------

#read london boroughs shapefile, downloaded from the london datastore (https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london)
boroughs <- st_read(here::here("Data/GeoData/London_Borough_Excluding_MHW.shp"))%>% st_transform(., 4326)

#from boroughs, select westminster
westminster <-  dplyr::filter(boroughs, NAME=='Westminster') %>% st_transform(., 27700) 
westminster_wgs84 <- westminster %>% st_transform(., 4326)

#read green spaces and filter for westminster -Green spaces shapefile downloaded from the london datastore (https://data.london.gov.uk/dataset/green-and-blue-cover)
green_space <-  st_read(here::here("Data/GeoData/Green_spaces_excluding_private.shp")) %>% st_transform(., 27700)
green_space_westminster <- st_intersection(green_space, westminster) %>% dplyr::filter(., TypeMain == 'park')

# !!! Read Buildings within Westminster geojson. Created by downloading the Ordnance Survey MasterMap Topography (Buildings Layer Only). 
#Raw data was downloaded from Edina Digimap https://digimap.edina.ac.uk/.
#To download data for westminster only, you can upload a shapefile ZIP-file for the borough of westminster, for example by downloading the sf object created above as a shapefile 
buildings <- st_read(here::here("Data/BuildingData/westminster_topography.gpkg"),layer="Topographicarea")



#Let's plot our Study Area
#buildings:
study_area <- ggplot()+ 
  geom_sf(data=westminster, fill='#E8E8E8',size=0.1)+
  geom_sf(data=green_space_westminster, color=NA, fill='#C3ECB2')+
  geom_sf(data=buildings, color='NA', fill='#a88554')+
  theme_map()+
  annotation_scale(location = "tr",height = unit(0.1, "cm"),pad_y=unit(0.6,'in'))+
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)
#borough map:
boroughmap <-ggplot()+
  geom_sf(data = boroughs, fill='#E8E8E8',size=0.1)+
  geom_sf(data = westminster_wgs84, fill='red', alpha = 0.5, color='black')+
  theme_map()

#combine and draw
studyarea <- ggdraw() +
  draw_plot(study_area) +
  draw_plot(boroughmap, x = 0.05, y = 0.05, width = 0.4, height = 0.4)

studyarea

#save plot
save_plot('plots/studyarea.png',studyarea)

#----------------------------- getting count of properties in each building polygon ---------------------------------

# !!! Read Ordnance Survey Open UPRN. Available from the Ordnance Survey via Edina Digimap https://digimap.edina.ac.uk/. 
#This data was preprocessed in QGIS to only include UPRN points within the borough of Westminster
UPRN_points <- st_read(here::here("Data/BuildingData/uprn_buildings.geojson"))

# find points within buildings
points_in_buildings <- st_join(UPRN_points, buildings, join = st_within)

# count points per building polygon
property_count <- count(as_tibble(points_in_buildings), fid) %>% rename(.,"NUMPOINTS"="n") %>% drop_na()

#merge this with building object
buildings <- dplyr::left_join(buildings,property_count,by='fid')

#replace nas with 0
buildings[is.na(buildings)] <- 0

#write buildings as a geojson to be used in other code files
write_sf(buildings,'Data/BuildingData/FinalBuildings.geojson')
