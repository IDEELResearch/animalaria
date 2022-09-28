## Script to create tables and figures for animalaria

# Load packages-------------------------------------------------------
library(tidyverse)
library(arsenal)
library(tableone)
library(gplots)
library(Hmisc)
library(fastDummies)
library(gmodels)
library(readxl)
library(mgcv)
library(splines)
library(plotrix)
library(nloptr)
library(flextable)
library(RColorBrewer)
library(tableone)
library(eulerr)

# Load data------------------------------------------------------------------
dhs <- readRDS("./dhs_drc_adults.rds")

# Histograms of how many animals owned---------------------------------
# hv246b cows/bulls 
# hv246c horses/donkeys/mules
# hv246d goats
# hv246e sheep
# hv246f chickens
# hv246g pigs
# hv246h duck

# distribution of cattle
cowsdist <- dhs %>% 
  dplyr::filter(cattle ==1) %>% 
  ggplot(aes(x = hv246b, fill = "hv246b"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Cattle")+
  ylab("Number of participants")+
  ylim(c(0,100))+
  theme_bw()+
  theme(legend.position = "none") 

# distribution of horses
horsesdist <- dhs %>% 
  dplyr::filter(horses ==1) %>% 
  ggplot(aes(x = hv246c, fill = "hv246c"))+
  geom_histogram(position = "identity", binwidth = 0.5)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Horses")+
  ylab("")+ 
  ylim(c(0,100))+
  scale_x_continuous(breaks = seq(0,10,5))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# distribution of goats
goatsdist <- dhs %>% 
  dplyr::filter(goats ==1) %>% 
  ggplot(aes(x = hv246d, fill = "hv246d"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Goats")+
  ylab("")+
  ylim(c(0,1200))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# distribution of sheep
sheepdist <- dhs %>% 
  dplyr::filter(sheep == 1) %>% 
  ggplot(aes(x = hv246e, fill = "hv246e"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Sheep")+
  ylab("")+
  ylim(c(0,1200))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# distribution of chickens
chickensdist <- dhs %>% 
  dplyr::filter(chickens == 1) %>% 
  ggplot(aes(x = hv246f, fill = "hv246f"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Chickens")+
  ylab("Number of participants")+ # far left, keeping labels
  ylim(c(0,1200))+
  theme_bw()+
  theme(legend.position = "none")

# distribution of pigs
pigsdist <- dhs %>% 
  dplyr::filter(pigs == 1) %>% 
  ggplot(aes(x = hv246g, fill = "hv246g"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Pigs")+
  ylab("")+
  ylim(c(0,1200))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# distribution of ducks
ducksdist <- dhs %>% 
  dplyr::filter(ducks == 1) %>% 
  ggplot(aes(x = hv246h, fill = "hv246h"))+
  geom_histogram(position = "identity", binwidth = 1)+
  scale_fill_manual(values = "skyblue3")+
  xlab("Ducks")+
  ylab("")+
  ylim(c(0,1200))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

library(cowplot)
plot_grid( chickensdist,goatsdist, pigsdist, ducksdist, sheepdist, cowsdist, horsesdist, ncol = 5, nrow = 2)
ggsave('./plots/descriptive_hist.png', width=8, height=4) # dimensions are a bit off


# HT's histogram
output <- dhs %>% 
  mutate(ID = row_number()) %>%
  select(ID, hv246b:hv246h) %>%
  pivot_longer(cols = hv246b:hv246h, names_to = "animal", values_to = "n") %>%
  mutate(animal = case_when(animal == "hv246b" ~ "Cattle",
                            animal == "hv246c" ~ "Horses, donkeys, and mules",
                            animal == "hv246d" ~ "Goats",
                            animal == "hv246e" ~ "Sheep",
                            animal == "hv246f" ~ "Chickens",
                            animal == "hv246g" ~ "Pigs",
                            animal == "hv246h" ~ "Ducks")) %>%
  filter(n != 0) # remove those owning 0 animals

# check
table(output$animal, useNA = 'always')

# plot
library(ggbreak) # if use ggbreak() commented out below - new package to break up axes
# must cite S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large
# datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846 
ggplot() +
  geom_histogram(data = output, aes(x=n, y=..count..,
                                    fill=animal, color=animal), alpha=0.1, show.legend = F, bins=100) +
  facet_grid(animal~., scales = 'free', labeller = label_wrap_gen(8)) + 
  labs(x='Number of animals owned by the household',
       y='Number of participants') +
  theme_classic() #+
  #ggbreak::scale_x_break(c(65,90))+
  #scale_x_continuous(breaks = seq(0, 100, by=10))

ggsave('./plots/animal_hist.png', width=4, height=5)

# Table 1 -------------------------------------------------------
dhs$hh_weight <- dhs$hv005/1000000
library(survey)
library(srvyr)

designf <-svydesign(ids=dhs$hv001, strata=dhs$hv022 , weights=dhs$hh_weight,  data=dhs)

options(survey.lonely.psu="adjust")

designf_dhs2 <-as_survey_design(designf)

# basic stats
# overall weighted malaria prevalence
prop.table(svytable(~pfldh_adult, designf_dhs2))
svyciprop(~pfldh_adult, designf_dhs2, method="lo")

# animal ownership
# any
prop.table(svytable(~animalown, designf_dhs2))
svyciprop(~animalown, designf_dhs2, method="lo")

prop.table(svytable(~chickens, designf_dhs2))
svyciprop(~chickens, designf_dhs2, method="lo")

prop.table(svytable(~goats, designf_dhs2))
svyciprop(~goats, designf_dhs2, method="lo")

prop.table(svytable(~cattle, designf_dhs2))
svyciprop(~cattle, designf_dhs2, method="lo")
# check the others again
prop.table(svytable(~horses, designf_dhs2))
prop.table(svytable(~ducks, designf_dhs2))
prop.table(svytable(~sheep, designf_dhs2))

# cattle by pf
prop.table(svytable(~cattleherd5+adultmalaria, designf_dhs2))
prop.table(svytable(~cattleherd10+adultmalaria, designf_dhs2))

table(dhs$cattleherd10)
table(dhs$cattleherd5)
# label factor variables 
dhs <- dhs %>% 
  dplyr::mutate(
    hv270_f=factor(hv270, 
                   levels = c(1, 2,3,4,5), 
                   labels = c("Poorest", "Poorer", "Middle", "Richer","Richest")),
    landown_f=factor(landown,
                     levels = c(0,1),
                     labels = c("No agricultural land","Owns agricultural land")),
    hv024_f=factor(hv024,
                   levels = c(1,2,3,4,5,6,7,8,9,10,11),
                   labels = c("Kinshasa", "Bandundu", "Bas-Congo", "Equateur", "Kasai-Occidental", "Kasai-Oriental", "Katanga", "Maniema", "Nord-Kivu", "Orientale", "Sud-Kivu")))



# Weight Table 1 ----------------------------------------------
# create survey design
DHSdesign <- svydesign(id=~hv001, strata=~hv022, weights=~hh_weight, data=dhs) 

# without survey.lonely.psu options, function fails b/c of single clusters
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# 'adjust' recommended by DHS https://userforum.dhsprogram.com/index.php?t=msg&goto=12423&S=Google&t=msg&goto=12423&S=Google
options(survey.lonely.psu="adjust")

# create functions to calculate weighted n
# running functions pastes the results to clipboard which you can then copy into excel

# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), DHSdesign, na.rm=T, survey.lonely.psu="adjust") %>% write.table("clipboard",sep="\t")
}

# counts for n in dataset, stratified by malaria Y or N
survtable <- function(var){ 
  svyby(as.formula(paste0('~', var)),~pfldh_adult, DHSdesign, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% write.table("clipboard",sep="\t")
}

# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),DHSdesign, na.rm=T, survey.lonely.psu="adjust") %>% write.table("clipboard",sep="\t")
}

# mean for continuos vars in dataset, stratified by malaria Y or N
survmean <- function(var){ 
  svyby(as.formula(paste0('~', var)),~pfldh_adult, DHSdesign, svymean, na.rm=T, survey.lonely.psu="adjust") %>% write.table("clipboard",sep="\t")
}

# run for each var - output copies to clipboard, you can paste into excel
survtable_all("cattle")
survtable("cattle")

#  using clipr::write_clip() ; write.table() gives error

# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), DHSdesign, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# counts for n in dataset, stratified by malaria Y or N
survtable <- function(var){ 
  svyby(as.formula(paste0('~', var)),~pfldh_adult, DHSdesign, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),DHSdesign, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by malaria Y or N
survmean <- function(var){ 
  svyby(as.formula(paste0('~', var)),~pfldh_adult, DHSdesign, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}
# run for each var - output copies to clipboard, you can paste into excel
# ("adultmalaria", "hv009","hv014","sex",  "treatedbednet", "modernhousing", "hv270_f", 
                 #   "urbanrural", "landown_f", "hv024_f", "animalown", "cattle", "horses", "goats", "sheep", 
                 #   "chickens", "pigs", "ducks")

survtable_all("adultmalaria") # overall n
# continuous data: hv009, hv014
survmean_all("hv009") 
survmean("hv009")

survmean_all("hv014")
survmean("hv014")
survmean_all("hv105")
survmean("hv105")
# categorical data
survtable_all("sex") # this is for male sex, subtract from survtable_all("adultmalaria") to get female counts
survtable("sex")
survtable_all("treatedbednet") 
survtable("treatedbednet")
survtable_all("modernhousing") 
survtable("modernhousing")
survtable_all("hv270_f") 
survtable("hv270_f")
survtable_all("urbanrural") 
survtable("urbanrural")
survtable_all("landown_f") 
survtable("landown_f")
survtable_all("hv024_f") 
survtable("hv024_f")
survtable_all("animalown") 
survtable("animalown")
survtable_all("cattle") 
survtable("cattle")
survtable_all("chickens") 
survtable("chickens")
survtable_all("horses") 
survtable("horses")
survtable_all("goats") 
survtable("goats")
survtable_all("sheep") 
survtable("sheep")
survtable_all("pigs") 
survtable("pigs")
survtable_all("ducks") 
survtable("ducks")

# median + IQR for animal types
# need to work on writing this as a function
cattle_own<-subset(DHSdesign,hv246b>0&hv246b<98)
svyquantile(~hv246b, cattle_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246b, ~pfldh_adult, cattle_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

horses_own<-subset(DHSdesign,hv246c>0&hv246c<98)
svyquantile(~hv246c, horses_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246c, ~pfldh_adult, horses_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

goats_own<-subset(DHSdesign,hv246d>0&hv246d<98)
svyquantile(~hv246d, goats_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246d, ~pfldh_adult, goats_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

sheep_own<-subset(DHSdesign,hv246e>0&hv246e<98)
svyquantile(~hv246e, sheep_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246e, ~pfldh_adult, sheep_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

chickens_own<-subset(DHSdesign,hv246f>0&hv246f<98)
svyquantile(~hv246f, chickens_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246f, ~pfldh_adult, chickens_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

pigs_own<-subset(DHSdesign,hv246g>0&hv246g<98)
svyquantile(~hv246g, pigs_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246g, ~pfldh_adult, pigs_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")

ducks_own<-subset(DHSdesign,hv246h>0&hv246h<98)
svyquantile(~hv246h, ducks_own, quantiles=c(0.25,0.5,0.75),ci=F,vartype="ci",survey.lonely.psu="adjust")
svyby(~hv246h, ~pfldh_adult, ducks_own, svyquantile, quantiles=c(0.25,0.5,0.75),ci=T,vartype="ci",survey.lonely.psu="adjust")


# Table 2---------------------------------------------------------------------------
addmargins(table(dhs$horses, dhs$adultmalaria))
addmargins(table(dhs$sheep, dhs$adultmalaria))
addmargins(table(dhs$pigs, dhs$adultmalaria))
addmargins(table(dhs$ducks, dhs$adultmalaria))


# Maps------------------------------------------------------------------------------------------
library(sf)
library(gstat)
library(stars)
library(tidyverse)
library(patchwork)

# read in data
dat_sf <- dhs %>% st_as_sf(crs = st_crs(4326)) # DHS data as sf object

admin0 <- readRDS('./admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

# calculate prevalence per cluster
output <- dat_sf %>% 
  group_by(hv001) %>%
  # make variable for animal ownership
  mutate(ownership = case_when(animalown=='Owns animals'~1,
                               animalown=="Doesn't own"~0,
                               TRUE ~ NA_real_)) %>%
  dplyr::summarize(n=n(),
            prev = mean(pfldh_adult, na.rm=T)*100,
            ownership = mean(ownership, na.rm=T)*100,
            cattle = mean(cattle, na.rm=T)*100,
            horses = mean(horses, na.rm=T)*100,
            goats = mean(goats, na.rm=T)*100,
            sheep = mean(sheep, na.rm=T)*100,
            chickens = mean(chickens, na.rm=T)*100,
            pigs = mean(pigs, na.rm=T)*100,
            ducks = mean(ducks, na.rm=T)*100)

# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output, DRC, join = st_intersects) %>% filter(!is.na(Country))

# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps

# make variogram
m.vgm <- gstat::variogram(prev~1, output_points)

# fit a model to the sample variogram
# https://gisgeography.com/semi-variogram-nugget-range-sill/
m.fit <- gstat::fit.variogram(m.vgm, model=vgm(psill=480,"Exp",range=300, nugget=250))

# plot
plot(m.vgm,m.fit)

# simple kriging
spDRC <- as_Spatial(DRC)
grd <- makegrid(spDRC, n = 50000)# making grid of points
colnames(grd) <- c('x','y')
grd_pts <- SpatialPoints(coords = grd, 
                         proj4string=CRS(proj4string(spDRC)))

# find all points in `grd_pts` that fall within DRC outline
grd_pts_in <- grd_pts[spDRC, ]

# transform grd_pts_in back into a data frame
gdf <- as.data.frame(coordinates(grd_pts_in)) 

# conduct kriging: Pf prev
m.kriged <- gstat::krige(prev~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged$var1.pred)

# assign points into bins
krige <- m.kriged %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,80,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

# conduct kriging: animal ownership
m.kriged.own <- gstat::krige(ownership~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.own$var1.pred)

# assign points into bins
krige_own <- m.kriged.own %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,90,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

# PLOTS
# prevalence by cluster points
A <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev), alpha=0.8) + 
  labs(color='Pf prevalence') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# kriging
B <- ggplot() + 
  geom_tile(data=(krige %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=var1.pred)) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  labs(fill="Predicted Pf \nprevalence", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# animal ownership prevalence by cluster points
C <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=ownership), alpha=0.8) + 
  labs(color='Animal ownership') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(direction = 1) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# kriging of animal ownership
D <- ggplot() + 
  geom_tile(data=(krige_own %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=var1.pred)) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  labs(fill="Predicted animal \nownership", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(direction=1, labels=c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# piece plots together using library(patchwork)
A + B + C + D + plot_layout(nrow=2, ncol = 2) + plot_annotation(tag_levels = 'A')

# output
ggsave('./plots/prev_map.png', width=15, height=9)

# < with eulerr package --------------------------------------------------------
library(eulerr)
# read in data
dat <- readRDS('./dhs_drc_adults.rds')     # clean DHS data

# https://cran.r-project.org/web/packages/eulerr/vignettes/introduction.html
# Input as a matrix of logicals

# inspect tables
table(dat$chickens, dat$cattle)
table(dat$horses) # can take out horses - only 24

# remove missings n=2
dat2 <- dat %>% filter(!is.na(chickens) & 
                         !is.na(cattle) &
                         !is.na(goats) &
                         !is.na(sheep) &
                         !is.na(pigs) &
                         !is.na(ducks) &
                         !is.na(horses)) %>%
  dplyr::select(-horses)

# create combination matrix
mat <- cbind(
  chickens = as.logical(dat2$chickens),
  cattle = as.logical(dat2$cattle),
  goats = as.logical(dat2$goats),
  sheep = as.logical(dat2$sheep),
  pigs = as.logical(dat2$pigs),
  ducks = as.logical(dat2$ducks),
  horses = as.logical(dat2$horses)
)

# set different seeds to get different iterations until you find one you like
set.seed(5)

# create fit - circle
fit <- euler(mat)
# errors generally low but residuals pretty bad for some
fit

# plot
p <- plot(fit)

# not perfectly accurate - completely ignoring those with cattle but no chickens
p

ggsave('./plots/eulerr_circle.png', plot = p, width=4, height=4)

# set different seeds to get different iterations until you find one you like
set.seed(100)
# set.seed(1) 1 made a more accurate figure on my computer

# create fit  - ellipse
fit <- euler(mat, shape = "ellipse")
# errors generally low but residuals pretty bad for some
fit

# plot
p <- plot(fit)

p

ggsave('./plots/eulerr_ellipse.png', plot = p, width=4, height=4)


# < tile plot ------------------------------------------------------------------
# read in data
dat <- readRDS('./dhs_drc_adults.rds')     # clean DHS data

# helper function to get mode of character variable
getmode <- function(v) {
  # unique values
  uniqv <- unique(v) 
  # remove NAs
  uniqv <- uniqv[!is.na(uniqv)]
  # count and link with unique names
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


dat2 <- dat %>% dplyr::select(cattle:ducks, adultmalaria) %>%
  # classify malaria
  mutate(adultmalaria = case_when(adultmalaria == 'Pfldh positive' ~ 1,
                                  adultmalaria == 'Pfldh negative' ~ 0),
         ID = row_number()) %>%
  group_by(cattle, horses, goats, sheep, chickens, pigs, ducks) %>% 
  # calculate n and prev by group
  summarize(n=n(), 
            prev = mean(adultmalaria)) %>%
  rowwise() %>%
  mutate(t = sum(cattle, horses, goats, sheep, chickens, pigs, ducks)) %>%
  # remove low #s and 3+ combos
  filter(n>=10 & t<=2) %>%
  ungroup() %>%
  mutate(ID = row_number()) %>%
  # pivot lengthwise
  pivot_longer(cols = c(cattle:ducks), names_to = 'var', values_to = 'value') %>%
  filter(value !=0) %>%
  group_by(ID) %>%
  mutate(ID2 = row_number()) %>%
  ungroup() %>%
  # assign first and second animal choices
  mutate(var1 = ifelse(ID2==1, var, NA_character_),
         var2 = case_when(t==1 ~ var, 
                          ID2==2 ~ var)) %>%
  group_by(ID) %>%
  arrange(var2) %>%
  summarize(n = mean(n),
            prev = mean(prev),
            var1 = getmode(var1),
            var2 = getmode(var2))

dat3 <- dat2 %>% dplyr::select(ID, n, prev, var2, var1)
colnames(dat3) <- colnames(dat2)
dat4 <- rbind(dat2, dat3) %>% distinct()
 
# plot
# https://r-charts.com/correlation/heat-map-ggplot2/
ggplot(dat4, aes(x = factor(var1), y = factor(var2), fill = prev)) +
  geom_tile(color = "white",
          lwd = 1.5,
          linetype = 1) +
  geom_text(aes(label = round(prev,2)), color = "white", size = 4) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +  # ?scale_colour_gradient
  labs(x = 'animal type #1', y = 'animal type #2', fill = 'Pf prevalence',
       caption = 'combinations with n < 10 removed \n3+ combinations removed') + 
  coord_fixed() + 
  theme_classic()


ggsave('./plots/tile_plot.png', width=6, height=6)
