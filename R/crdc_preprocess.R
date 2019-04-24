# CRDC data 
library(data.table)
path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData/2015-16-crdc-data/Data Files and Layouts"

crdc_fn = "CRDC_2015_16_School_Data.csv"
file.path(path, crdc_fn)


# load data
crdc_df = fread(file.path(path, crdc_fn),
                colClasses=c(COMBOKEY="numeric")) %>%
  filter(LEA_STATE == "WA")

str(crdc_df$COMBOKEY)

crdc_vars <-  crdc_df %>%
  mutate(COMBOKEY = as.character(COMBOKEY)) %>%
  # select(1:8, contains("ABSENT"), contains("DISCWODIS_ISS"), 
  #        contains("DISCWODIS_MULTOOS"), contains("DISCWODIS_SINGOOS"),
  #        contains("DISCWODIS_REF"), contains("SCH_FTESECURITY_LEO"),
  #        contains("ENR")) %>%
  mutate(enrollment_tot = TOT_ENR_M + TOT_ENR_F,
         absent_tot = TOT_ABSENT_M + TOT_ABSENT_F,
         refferal_tot = TOT_DISCWODIS_REF_M + TOT_DISCWODIS_REF_F,
         suspen_tot = TOT_DISCWODIS_MULTOOS_M + TOT_DISCWODIS_MULTOOS_F + 
           TOT_DISCWODIS_SINGOOS_M + TOT_DISCWODIS_SINGOOS_F + 
           TOT_DISCWODIS_ISS_M + TOT_DISCWODIS_ISS_F,
         suspen_tot_black = SCH_DISCWODIS_SINGOOS_BL_F  + SCH_DISCWODIS_SINGOOS_BL_M + 
           SCH_DISCWODIS_ISS_BL_F  + SCH_DISCWODIS_ISS_BL_M + 
           SCH_DISCWODIS_MULTOOS_BL_M  + SCH_DISCWODIS_MULTOOS_BL_F) %>%
  select(1:8, enrollment_tot, absent_tot, refferal_tot, suspen_tot, suspen_tot_black)

names(crdc_vars)
