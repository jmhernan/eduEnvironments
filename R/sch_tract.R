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
leaflet(kc_tracts) %>% addProviderTiles(providers$CartoDB.Positron) %>%
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
# COMBINE EDDATA 

glimpse(schools_tract)

# Select features of interest
kc_school_tract <- schools_tract %>% 
  filter(!is.na(tract)) %>% 
  mutate(st_schid = str_sub(ST_SCHID, start= -4),
         st_leaid = str_sub(ST_LEAID, start = -5),
         NCESSCH = as.character(NCESSCH)) %>%
  select(1:6, st_schid, st_leaid, 74, 12, 31:33, 35, 36, 39, 64) 


names(wa_sch_dems)

names(crdc_vars)

crdc_vars_cl <- crdc_vars %>% 
  mutate(absent_tot = ifelse(absent_tot < 0, 0, absent_tot),
         refferal_tot = ifelse(refferal_tot < 0, 0, refferal_tot),
         suspen_tot = ifelse(suspen_tot < 0, 0, suspen_tot),
         suspen_tot_black = ifelse(suspen_tot_black < 0, 0, suspen_tot_black),
         SCH_LEO = ifelse(SCH_FTESECURITY_LEO > 0, 1, 0),
         SCHID = str_pad(SCHID, 5, pad = "0")) %>%
  select(-SCH_FTESECURITY_LEO) %>%
  unite(NCESSCH, LEAID, SCHID, sep = "", remove = FALSE)
  
kc_school_tract %>% select(NCESSCH) %>% unique() %>% nrow()
crdc_vars_cl %>% select(NCESSCH) %>% unique() %>% nrow()

str(crdc_vars_cl$NCESSCH)
str(kc_school_tract$NCESSCH)
sch_combined <- left_join(kc_school_tract, crdc_vars_cl, by = c("NCESSCH"))

sch_combined <- left_join(sch_combined, wa_sch_dems, by = c("NCESSCH"))

sch_combined <- left_join(sch_combined, scores %>% select(-School), by = c("st_leaid"="DistrictCode", "st_schid"="SchoolCode"))
#### 50 school specific features 
## Add census 
glimpse(tract_pop)
names(sch_combined)
sch_census <- left_join(sch_combined, tract_pop, by = c("tract" = "GEO_ID_TRT"))


## Add life expectancy
sch_cen_life <- left_join(sch_census %>% mutate(tract = as.character(tract)), life_df, by = c("tract" = "Tract_ID"))
## Add 500 citities [HOLD]
glimpse(sch_cen_life)

## CREATE FINAL TABLE 
analysis_df <- sch_cen_life %>% 
  select(tract, NCESSCH, st_schid, st_leaid, sch_name=SCH_NAME.x, lea_name = LEA_NAME.x,
         SCH_TYPE_TEXT, LEVEL, JJ, enrollment_tot_nces=Total,
         absent_tot:SCH_LEO, AmericanIndianorAlaskaNative:pass_ela, NFB2:PercentOtherLang,
         POC:Percent65andOver, `e(0)`, `se(e(0))`
         )

table(analysis_df$JJ)
####
#write_csv(analysis_df, "/Users/josehernandez/Google Drive File Stream/My Drive/edData/analysis_df.csv")
###