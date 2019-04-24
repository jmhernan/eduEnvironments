# get tracts that contain schools of interest using the catchment file information
install.packages('geojsonio')
library(geojsonio)
library(leaflet)
library(rgdal)
library(sf)
library(tidyverse)

# School data 

# We need tract information
kc_tracts <- geojsonio::geojson_read("https://opendata.arcgis.com/datasets/b4cf82cdfa2b437eb51ccbfe980aa39e_2549.geojson",
                                     what = "sp")

##Load geo-located school information
path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData"
sch_geo_fn = "king_sch_geo.csv"

kc_sch_geo_test = read.csv(file.path(path, sch_geo_fn)) 

# Check visually
leaflet(king_county) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -122.335167, lat=47.608013,zoom=11) %>%
  addPolygons(data=kc_tracts,weight=1, smoothFactor = 0.5, opacity = .5) %>%
  addCircleMarkers(kc_sch_geo, lng = kc_sch_geo$LON, 
                   lat = kc_sch_geo$LAT,
                   label = ~kc_sch_geo$SCH_NAME,
                   radius = 5,
                   stroke = FALSE, fillOpacity = 0.5, color = 'red')


##first, need to change the polygon IDs so that they are not duplicated across shapefile sets
kct_1 <- spChFIDs(kc_tracts, as.character(kc_tracts$GEO_ID_TRT))


#tell R that school coordinates are in the same lat/long reference system as the places data
coordinates(kc_sch_geo) <- c("LON", "LAT")
proj4string(kc_sch_geo) <- proj4string(kct_1)

#combine is.na() with over() to do the containment test (note that we need to "demote" places to a SpatialPolygons object first)
inside.place <- !is.na(over(kc_sch_geo, as(kct_1, "SpatialPolygons")))

#use "over" again, this time with places as a SpatialPolygonsDataFrame object, to determine which places (if any) contains each school, and store the place name as attribute of the schools data
kc_sch_geo$tract <- rep(NA, nrow(kc_sch_geo))
kc_sch_geo$tract <- over(kc_sch_geo, kct_1)$GEO_ID_TRT

#school df with tract information 
schools_tract <- data.frame(kc_sch_geo)
tract_pop <- data.frame(kc_tracts@data)
###########
names(schools_tract)
# Add other education data sets 
#source(___nces_preprocess.R)

wa_sch_dems %>%
  glimpse()

sch_census <- left_join(schools_tract, tract_pop, by = c("tract"="GEO_ID_TRT"))

                        