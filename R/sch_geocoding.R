library(tidyverse)
# Only run once
path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData"
sch_name = "ccd_sch_dir.csv"

file.path(path, data_name)

### We need a list of school lacations that are in King County districts 
sch_df = read.csv(file.path(path, sch_name)) %>%
  filter(FIPST == 53) %>%
  mutate(district_id = as.integer(str_extract(ST_LEAID, "[0-9]+"))) 

glimpse(sch_df)

king_district_ids <- c('17001',
                       '17210',	
                       '17216',	
                       '17400',	
                       '17401',	
                       '17402',
                       '17403',
                       '17404',
                       '17405',
                       '17406',
                       '17408',
                       '17409',
                       '17410',
                       '17411',
                       '17412',
                       '17414',
                       '17415',
                       '17417',
                       '27417',
                       '17407')

king_district_sch <- sch_df %>% 
  filter( district_id %in% king_district_ids)

# Geo code using google api
library(DBI)
library(stringr)


#google API call

geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, '&sensor=false&key=', api_key, sep=''))
  x <- fromJSON(url,simplify=FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lat,
             x$results[[1]]$geometry$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.0167) # Verified developer now 160/s (API only allows 50 requests per second for non-verified user)
  out
}

names(king_district_sch)

geo_data <- king_district_sch %>%
  select(MSTREET1, MCITY, MSTATE, MZIP) %>%
  unite(adress, MSTREET1, MCITY, MSTATE, MZIP, sep = " ", remove = FALSE)

#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA
g_add=list()

for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data[,1][i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]
  
  if ((i %% 100) ==0) cat(paste(i," "))
  if (i == nrow(geo_data)) cat("Done!\n")
}

#add addresses w/geo_cords back to main dataset
kc_sch_geo <- bind_cols(king_district_sch, geo_data)
#save since geolocation cost $
write_csv(kc_sch_geo,  path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData/king_sch_geo.csv")
####