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
  summarise(pass_ela = sum(as.numeric(countMetStandardIncludingPP), na.rm=T))


sch_math = sch_test %>%
  filter(Subject == "Math" | Subject == "MATH", StudentGroup == "All", !is.na(District)) %>%
  group_by(DistrictCode, School, SchoolCode) %>%
  summarise(pass_math = sum(as.numeric(countMetStandardIncludingPP), na.rm=T))

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

all_df <- all_df %>%
  mutate(frl = (`Freelunchqualified` + `Reducedpricelunchqualified`)/Total) %>%
  mutate(AIAN_p = AmericanIndianorAlaskaNative/Total, 
          AS_p = Asian/Total,
         B_p = BlackorAfricanAmerican/Total,
         Hi_p = HispanicLatino/Total,
         NHP = NativeHawaiianorOtherPacificIslander/Total,
         Wh_p = White/Total)

all_df <- left_join(all_df, scores, by = c("SCH_NAME" = "School")) 

all_df = all_df %>%
  mutate(pass_math_p = pass_math/Total,
         pass_ela_p = pass_ela/Total)

pca_dat <- all_df %>%
  select(-SCH_NAME, -SCHID, -NCESSCH, -`NotSpecified`) %>% na.omit(.)
str(pca_dat)

write.csv(all_df, file= "WA_SCH.csv")

sch_pca <- prcomp(pca_dat,
                      center = TRUE,
                      scale. = TRUE) 

summary(sch_pca)

library(ggfortify)

autoplot(sch_pca, data = pca_dat)

autoplot(kmeans(pca_dat, 3), data = pca_dat)
########################
library(caret)
library(Rtsne)

tsne_model_1 = Rtsne(as.matrix(pca_dat), check_duplicates=FALSE, pca=T, perplexity=30, theta=0.5, dims=2)

d_tsne_1 = as.data.frame(tsne_model_1$Y) 

d_tsne_1 = bind_cols(d_tsne_1,pca_dat %>% select(LEAID) %>% mutate(LEAID = as.character(LEAID)))

ggplot(d_tsne_1, aes(x=V1, y=V2, colour = LEAID)) +  
  geom_point(size=0.25) +
  #guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("WA State Schools Dems + Test Scores") +
  theme_light(base_size=20) +
  theme(legend.position="none")

ggsave("WA_sch_test2.png")

######
d_tsne_1_original=d_tsne_1

## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans=kmeans(scale(d_tsne_1), 3)  
d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1)))

## setting 3 clusters as output
d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=7))  


plot_cluster=function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=0.25) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(#axis.text.x=element_blank(),
          #axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
}

plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")  

plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set1")
plot_k
plot_h

clust1 = bind_cols(pca_dat, d_tsne_1_original)

clust1 %>%
  group_by(cl_hierarchical) %>%
  summarise(mn_math = mean(pass_math_p),
            mn_ela = mean(pass_ela_p),
            mn_frl = mean(frl),
            mn_w = mean(Wh_p),
            mn_b = mean(B_p),
            mn_h = mean(Hi_p))

kmeans(scale(d_tsne_1), 3) 

wss <- function(k) {
  kmeans(scale(d_tsne_1), k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

