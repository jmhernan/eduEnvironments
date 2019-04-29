library(data.table)
# Life expectancy + 500 cities data integration
path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData"

life_fn = "WA_A.csv"
city_500_fn = "500_Cities__Local_Data_for_Better_Health__2018_release.csv"

file.path(path, life_fn)


# load data
life_df <- fread(file.path(path, life_fn)) 
life_df <- life_df %>%
  mutate(Tract_ID = as.character(`Tract ID`)) %>% glimpse()

WA_500 = fread(file.path(path, city_500_fn)) %>%
  filter(StateAbbr == "WA")

# HOLD OFF ON INCORPORATING THE CITY DATA AS IT LEAVES OUT A LOT OF KING COUNTY
# WILL INCORPORATE WHEN DEALING WITH CITY LEVEL DATA 

# FOCUS ON LIFE EXP CHECK OUT FLAGS FOR FUTURE PROJECTS
glimpse(life_df)



