library(tidyverse)

path = "/Users/josehernandez/Google Drive File Stream/My Drive/edData"

data_name = "ccd_SCH_16_17.csv"
income_name = "ccd_sch_income.csv"
person_name = "ccd_sch_people.csv"
sch_name = "ccd_sch_dir.csv"
test = "sba.txt"

file.path(path, data_name)


# read.csv

n_df = read.csv(file.path(path, data_name)) %>%
  filter(FIPST == 53)

income_df = read.csv(file.path(path, income_name)) %>%
  filter(FIPST == 53)

person_df = read.csv(file.path(path, person_name)) %>%
  filter(FIPST == 53)

sch_df = read.csv(file.path(path, sch_name)) %>%
  filter(FIPST == 53)

sch_test = read.delim(file.path(path, test), sep = "\t") 
names(sch_test)
str(sch_test)

sch_ela = sch_test %>%
  filter(Subject == "ELA", StudentGroup == "All", !is.na(District)) %>%
  group_by(DistrictCode, School, SchoolCode) %>%
  summarise(pass_ela = sum(as.numeric(countMetStandardIncludingPP), na.rm=T)) %>%
  filter(!is.na(DistrictCode))


sch_math = sch_test %>%
  filter(Subject == "Math" | Subject == "MATH", StudentGroup == "All", !is.na(District)) %>%
  group_by(DistrictCode, School, SchoolCode) %>%
  summarise(pass_math = sum(as.numeric(countMetStandardIncludingPP), na.rm=T)) %>%
  filter(!is.na(DistrictCode))

scores = left_join(sch_math, sch_ela, by = c("DistrictCode", "School", "SchoolCode"))



str(n_df)
####PCA EXPLORATORY
n_df <- n_df %>% 
  filter(FIPST == 53)
names(n_df)

table(n_df$TOTAL_INDICATOR)

totals <- n_df %>%
  filter(TOTAL_INDICATOR == "Education Unit Total") %>%
  select(LEAID, SCH_NAME, SCHID, NCESSCH, Total=STUDENT_COUNT) 

race <- n_df %>%
  filter(TOTAL_INDICATOR == "Category Set A - By Race/Ethnicity; Sex; Grade") %>%
  group_by(SCH_NAME, SCHID, NCESSCH, RACE_ETHNICITY) %>%
  summarise(n = sum(STUDENT_COUNT, na.rm=T)) %>%
  mutate(RACE_ETHNICITY = str_replace_all(RACE_ETHNICITY, " ", ""),
         RACE_ETHNICITY = str_replace(RACE_ETHNICITY, "/", "")) %>%
  spread(key = RACE_ETHNICITY, value = n)

income <- income_df %>%
  filter(LUNCH_PROGRAM == "Free lunch qualified" | LUNCH_PROGRAM == "Reduced-price lunch qualified") %>%
  select(SCH_NAME,SCHID, NCESSCH, LUNCH_PROGRAM, STUDENT_COUNT) %>%
  mutate(LUNCH_PROGRAM = str_replace_all(LUNCH_PROGRAM, " ", ""),
         LUNCH_PROGRAM = str_replace_all(LUNCH_PROGRAM, "-", "")) %>%
  spread(key = LUNCH_PROGRAM, value = STUDENT_COUNT)


all_df <- left_join(totals, race, by = c("SCH_NAME", "SCHID", "NCESSCH"))

all_df <- left_join(all_df, income, by = c("SCH_NAME", "SCHID", "NCESSCH"))

names(all_df)

wa_sch_dems <- all_df %>%
  mutate(frl = (`Freelunchqualified` + `Reducedpricelunchqualified`)/Total) %>%
  mutate(AIAN_p = AmericanIndianorAlaskaNative/Total, 
         AS_p = Asian/Total,
         B_p = BlackorAfricanAmerican/Total,
         Hi_p = HispanicLatino/Total,
         NHP = NativeHawaiianorOtherPacificIslander/Total,
         Wh_p = White/Total,
         NCESSCH = as.character(NCESSCH))

wa_sch_dems %>% select(NCESSCH) %>% nrow() == wa_sch_dems %>% select(NCESSCH) %>% unique() %>% nrow()
