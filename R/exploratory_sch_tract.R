# EXPLORATORY
# Clustering
# PCA
# Blender

library(tidyverse) 
library(caret)
library(cluster)    
library(factoextra) 
#library(dendextend) 
# create one-hot encoded features 


names(analysis_df)
excluded_v <- c('NFB2',
                 'NP2002',
                 'NS2',
                 'NR2',
                 'NC2',
                 'NK2',
                 'NV2',
                 'NAL2',
                 'NL2',
                 'OtherLang',
                 'POC',
                 'WhiteAlone',
                 'NotLatino',
                 'BlackAlone',
                 'AIANAlone',
                 'AsianAlone',
                 'NHOPIalone',
                 'TworacesAlone',
                 'HispanicorLatino',
                 'Male',
                 'Female',
                 'Under18',
                 'Between18and65',
                 'Over65',
                 'AmericanIndianorAlaskaNative', 
                 'Asian',
                 'BlackorAfricanAmerican', 
                 'HispanicLatino',                     
                 'NativeHawaiianorOtherPacificIslander',
                 'NotSpecified',                 
                 'Twoormoreraces',                     
                 'White',                                
                 'Freelunchqualified',
                'Reducedpricelunchqualified',
                 'refferal_tot',
                 'suspen_tot',
                 'suspen_tot_black',
                 'pass_math',
                 'pass_ela')

analysis_df_clean <- analysis_df %>%
  mutate(ch_absent_p = (absent_tot/enrollment_tot_nces)*100,
         refferal_p = (refferal_tot/enrollment_tot_nces)*100,
         suspend_p = (suspen_tot/enrollment_tot_nces)*100,
         suspen_p_black = (suspen_tot_black/enrollment_tot_nces)*100,
         pass_math_p = (pass_math/enrollment_tot_nces)*100,
         pass_ela_p = (pass_ela/enrollment_tot_nces)*100) %>%
  select(-one_of(excluded_v)) %>%
  mutate_at(vars(frl:Wh_p), .funs = funs(.*100))

summary(analysis_df_clean)
names(analysis_df_clean)

pca_dat <- analysis_df_clean %>%
  mutate(SCH_TYPE_TEXT = as.character(SCH_TYPE_TEXT),
         LEVEL = as.character(LEVEL),
         dist = word(lea_name, 1)) %>%
  unite(name, sch_name, dist, sep = "_") %>%
  select(name,7:52) %>% na.omit() 
names(pca_dat)

dmy <- dummyVars("~ SCH_TYPE_TEXT + LEVEL + JJ", data = pca_dat)

dummy <- data.frame(predict(dmy, newdata = pca_dat))

pca_dat <- pca_dat %>%
  bind_cols(dummy) %>%
  select(-SCH_TYPE_TEXT, -LEVEL, -JJ)
str(pca_dat)

# which(apply(pca_dat, 2, var)==0)    

# pca_dat <- pca_dat[ , which(apply(pca_dat, 2, var) !=0)]

names(pca_dat)

sch_pca <- prcomp(pca_dat[, -1],
                  center = TRUE,
                  scale. = TRUE) 

summary(sch_pca)

library(ggfortify)

autoplot(sch_pca, data = pca_dat)
###
library(Rtsne)

tsne_model_1 = Rtsne(as.matrix(pca_dat[,-1]), check_duplicates=FALSE, pca=T, perplexity=30, theta=0.5, dims=2)

d_tsne_1 = as.data.frame(tsne_model_1$Y) 

# d_tsne_1 = bind_cols(d_tsne_1,pca_dat %>% select(LEAID) %>% mutate(LEAID = as.character(LEAID)))

ggplot(d_tsne_1, aes(x=V1, y=V2)) +  
  geom_point(size=0.25) +
  #guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("WA State Schools Dems + Test Scores") +
  theme_light(base_size=20) +
  theme(legend.position="none")



######
d_tsne_1_original=d_tsne_1

## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans=kmeans(scale(d_tsne_1), 5)  
d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1)))

## setting 3 clusters as output
d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=5))  


plot_cluster=function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=0.50) +
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
#####
#####Clustering explore
# make v1 row names
hhc <- pca_dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="name")

###
# Dissimilarity matrix
d <- dist(hhc, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
##
# Compute with agnes
hc2 <- agnes(hhc, method = "complete")

# Agglomerative coefficient
hc2$ac
####
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(hhc, method = x)$ac
}

map_dbl(m, ac)
###
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)
###

hhc <- hhc %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = hhc, cluster = sub_grp))
########

fviz_nbclust(hhc, FUN = hcut, method = "wss")
fviz_nbclust(hhc, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(hhc, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

### HS only traditional
table(analysis_df_clean$LEVEL)
table(analysis_df_clean$SCH_TYPE_TEXT)
table(analysis_df_clean$JJ)
names(analysis_df_clean)
pca_dat <- analysis_df_clean %>%
  mutate(SCH_TYPE_TEXT = as.character(SCH_TYPE_TEXT),
         LEVEL = as.character(LEVEL),
         dist = word(lea_name, 1)) %>%
  unite(name, sch_name, dist, sep = "_") %>%
  select(name,7:54) %>% na.omit() %>%
  filter(LEVEL == "High", JJ == "No", SCH_TYPE_TEXT == "Regular School") %>%
  select(-LEVEL, -JJ, -SCH_TYPE_TEXT)
names(pca_dat)

dmy <- dummyVars("~ .", data = pca_dat)

dummy <- data.frame(predict(dmy, newdata = pca_dat))

pca_dat <- pca_dat %>%
  bind_cols(dummy) %>%
str(pca_dat)

###
hhc <- pca_dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="name")

###
# Dissimilarity matrix
d <- dist(hhc, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
##
# Compute with agnes
hc2 <- agnes(hhc, method = "complete")

# Agglomerative coefficient
hc2$ac
####
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(hhc, method = x)$ac
}

map_dbl(m, ac)
###
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
km <- kmeans(hhc, 3, nstart = 25)
print(km)
# Cut tree into 3 groups
sub_grp <- cutree(hc5, k = 3)

# Number of members in each cluster
table(sub_grp)
###

hhc_test <- pca_dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = hhc, cluster = sub_grp))

three <- hhc %>%
  filter(cluster == 3)
########

fviz_nbclust(hhc, FUN = hcut, method = "wss")
fviz_nbclust(hhc, FUN = hcut, method = "silhouette")

hhc %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
######
# Elementary + middle schools 
table(analysis_df_clean$LEVEL)
table(analysis_df_clean$SCH_TYPE_TEXT)
table(analysis_df_clean$JJ)
names(analysis_df_clean)

pca_dat <- analysis_df_clean %>%
  mutate(SCH_TYPE_TEXT = as.character(SCH_TYPE_TEXT),
         LEVEL = as.character(LEVEL),
         dist = word(lea_name, 1)) %>%
  unite(name, sch_name, dist, sep = "_") %>%
  select(name,7:54) %>% na.omit() %>%
  filter(LEVEL == "Elementary" | LEVEL == "Middle") %>%
  filter(JJ == "No", SCH_TYPE_TEXT == "Regular School") %>%
  select(-LEVEL, -JJ, -SCH_TYPE_TEXT)
pca_dat_geo <- analysis_df_clean %>%
  mutate(SCH_TYPE_TEXT = as.character(SCH_TYPE_TEXT),
         LEVEL = as.character(LEVEL),
         dist = word(lea_name, 1)) %>%
  unite(name, sch_name, dist, sep = "_") %>%
  select(tract,NCESSCH,name,7:54) %>% na.omit() %>%
  filter(LEVEL == "Elementary" | LEVEL == "Middle") %>%
  filter(JJ == "No", SCH_TYPE_TEXT == "Regular School") %>%
  select(-LEVEL, -JJ, -SCH_TYPE_TEXT)
names(pca_dat)

dmy <- dummyVars("~ .", data = pca_dat)

dummy <- data.frame(predict(dmy, newdata = pca_dat))

pca_dat <- pca_dat %>%
  bind_cols(dummy) %>%
  str(pca_dat)

###
hhc <- pca_dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="name")

###
# Dissimilarity matrix
d <- dist(hhc, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
##
# Compute with agnes
hc2 <- agnes(hhc, method = "complete")

# Agglomerative coefficient
hc2$ac
####
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(hhc, method = x)$ac
}

map_dbl(m, ac)
###
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
km <- kmeans(hhc, 5, nstart = 25)
print(km)
# Cut tree into 3 groups
sub_grp <- cutree(hc5, k = 5)

# Number of members in each cluster
table(sub_grp)
###

hhc_test <- pca_dat %>%
  mutate(cluster = sub_grp,
         tract_id = pca_dat_geo$tract,
         ncesid = pca_dat_geo$NCESSCH)

fviz_cluster(list(data = hhc, cluster = sub_grp))

three <- hhc_test %>%
  filter(cluster == 4)
########

fviz_nbclust(hhc, FUN = hcut, method = "wss")
fviz_nbclust(hhc, FUN = hcut, method = "silhouette")

gap_stat <- clusGap(hhc, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
###
names(hhc_test) # add relevanr vars
el_midd <- hhc_test %>%
  group_by(cluster) %>%
  summarise_at(vars(frl,PP2005,MHHI1,Wh_p,Hi_p,AIAN_p,B_p,AS_p,suspend_p, suspen_p_black,pass_ela_p,pass_math_p,
                    PWNH,PNW,PFB5,PBNH, ch_absent_p,
                    refferal_p,`e(0)`,Percent17Under,PercentOtherLang),"mean")

names(el_midd)
elem_midd <- el_midd %>%
  select(cluster, frl, Wh_p,AS_p,Hi_p, AIAN_p,B_p,suspend_p, suspen_p_black, pass_ela_p, pass_math_p, 
         ch_absent_p,refferal_p)

elem_midd_geo <- el_midd %>%
  select(cluster, pv_b_200=PP2005,med_hh_inc=MHHI1 ,white_nh=PWNH,black=PBNH,poc=PNW,foreign_b=PFB5,`e(0)`,Percent17Under,PercentOtherLang)
write_csv(elem_midd_geo, "/Users/josehernandez/Google Drive File Stream/My Drive/edData/elem_geo.csv")
write_csv(elem_midd, "/Users/josehernandez/Google Drive File Stream/My Drive/edData/elem_mid.csv")
##########
# look at the groupings 
head(schools_tract)

schools_loc <- schools_tract %>% 
  select(NCESSCH, LAT, LON) %>%
  mutate(NCESSCH = as.character(NCESSCH))

str(schools_loc$NCESSCH)

names(hhc_test)
hhc_test <- inner_join(hhc_test, schools_loc, by = c("ncesid"="NCESSCH")) %>%
  mutate(cluster = as.character(cluster),
         cluster = as.factor(cluster))
str(hhc_test$cluster)

pal <- colorFactor(
  palette = "Dark2",
  domain = hhc_test$cluster
)

leaflet(hhc_test) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -122.335167, lat=47.608013,zoom=11) %>%
  addPolygons(data=kc_tracts,weight=.5, smoothFactor = 0.5, opacity = .6) %>%
  addCircleMarkers(hhc_test, lng = hhc_test$LON, 
                   lat = hhc_test$LAT,
                   label = ~hhc_test$name,
                   radius = 10,
                   stroke = FALSE, fillOpacity = 0.5, color = ~pal(cluster)) %>%
  addLegend(pal = pal, values = ~cluster, opacity = 1)
#####
