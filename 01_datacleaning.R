# renv init--------------
#install.packages("renv")
renv::init()

# Load packages--------------------------------------------------
library(tidyverse)
library(arsenal)
library(tableone)
library(EpiStats)
library(lmtest)
library(gplots)
library(Hmisc)
library(fastDummies)
library(gmodels)

# Load data------------------------------------------------------------------------

dhs <- readRDS("./dhs_drc_adults.rds")

## this file was created from the full DHS dataset with biospecimen analysis and subsetting to adults with Pf analyzed, with the following code:
# alladults <- DHS_pr_full_merge[DHS_pr_full_merge$hv105 > 5,] # remove children under 5
# dhs_adultspf_share <- alladults %>% filter(!is.na(pfldh_adult)) # remove those without pfldh testing (those not selected for biospecimen collection and those with insufficient DBS)
# dhs <- dhs_adultspf_share

# check consent for other biospecimen analysis
table(dhs$ha64) # women
table(dhs$hb64) # men
# all 1's (yes)

# new variables---------------------------------------------------------------------------
# final variables to keep: animalown, adultmalaria, hv105, sex, treatedbednet, landown, urbanrural, largeprovince, shnprovin
# Exposure: 
# Any animal ownership----------------------------
dhs$animalown <- as.factor(dhs$hv246)
dhs <- dhs %>% 
  dplyr::mutate(animalown=factor(
    hv246, 
    levels = c(0, 1),
    labels = c("Doesn't own", "Owns animals")))
# Cattle-------------
dhs = dhs %>%
  mutate(cattle = case_when(
    hv246b >0 & hv246b <97 ~ 1, # owns cattle
    hv246b == 0 ~ 0, # no animals
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$cattle, dhs$hv246b ,useNA = "always"))


# levels of cattle-----

dhs = dhs %>%
  mutate(cattleherd5 = case_when(
    hv246b >= 5 ~ 2, # has 5 or more cattle
    hv246b < 5 & hv246b >0 ~ 1, # 1-5 cows
    hv246b == 0 ~ 0, # no cows
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$cattleherd5, dhs$cattle ,useNA = "always"))

dhs = dhs %>%
  mutate(cattleherd10 = case_when(
    hv246b >= 10 ~ 2, # has 10 or more cattle
    hv246b < 10 & hv246b >0 ~ 1, # 1-10 cows
    hv246b == 0 ~ 0, # no cows
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$cattleherd10, dhs$cattle ,useNA = "always"))

dhs = dhs %>%
  mutate(cattleherd15 = case_when(
    hv246b >= 15 ~ 2, # has 15 or more cattle
    hv246b < 15 & hv246b >0 ~ 1, # 1-15 cows
    hv246b == 0 ~ 0, # no cows    
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$cattleherd15, dhs$cattle ,useNA = "always"))


# Goats----------
dhs = dhs %>%
  mutate(goats = case_when(
    hv246d >0 & hv246d <97 ~ 1, # owns goats
    hv246d == 0 ~ 0, # no goats
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$goats, dhs$hv246d ,useNA = "always"))
# Chickens-----------
dhs = dhs %>%
  mutate(chickens = case_when(
    hv246f >0 & hv246f <97 ~ 1, # owns chickens
    hv246f == 0 ~ 0, # no chickens
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$chickens, dhs$hv246f ,useNA = "always"))


dhs = dhs %>%
  mutate(onlychickens = case_when(
    chickens == 1 & cattle==0 & sheep==0 & ducks==0 & goats==0 & horses==0 & pigs==0 ~ 2, # only has chickens
    chickens == 0 ~ 0, # no chickens
    is.na(hv246f) ~ NA_real_,
    TRUE ~ 1
  ) %>% as.numeric()
  )
addmargins(table(dhs$onlychickens, dhs$hv246f ,useNA = "always"))
addmargins(table(dhs$onlychickens, dhs$chickens ,useNA = "always"))
addmargins(table(dhs$onlychickens, dhs$sheep ,useNA = "always"))
addmargins(table(dhs$onlychickens, dhs$cattle ,useNA = "always"))
addmargins(table(dhs$onlychickens, dhs$horses ,useNA = "always"))

# Horses-----------
dhs = dhs %>%
  mutate(horses = case_when(
    hv246c >0 & hv246c <97 ~ 1, # owns horses/donkeys/mules
    hv246c == 0 ~ 0, # no horses/donkeys/mules
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$horses, dhs$hv246c ,useNA = "always"))
# Sheep-----------
dhs = dhs %>%
  mutate(sheep = case_when(
    hv246e >0 & hv246e <97 ~ 1, # owns sheep
    hv246e == 0 ~ 0, # no sheep
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$sheep, dhs$hv246e ,useNA = "always"))
# Pigs-----------
dhs = dhs %>%
  mutate(pigs = case_when(
    hv246g >0 & hv246g <97 ~ 1, # owns pigs
    hv246g == 0 ~ 0, # no pigs
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$pigs, dhs$hv246g ,useNA = "always"))
# Ducks-----------
dhs = dhs %>%
  mutate(ducks = case_when(
    hv246h >0 & hv246h <97 ~ 1, # owns pigs
    hv246h == 0 ~ 0, # no pigs
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$ducks, dhs$hv246h ,useNA = "always"))

# Pf malaria(outcome) ---------------
dhs <- dhs %>% 
  dplyr::mutate(adultmalaria=factor(
    pfldh_adult, 
    levels = c(0, 1),
    labels = c("Pfldh negative", "Pfldh positive")))
# check coding
addmargins(table(dhs$animalown, dhs$adultmalaria,useNA = "always"))

# covariates
# Age----------------------------------------
# Linear: use hv105

# Categorical
# create categorical grouping in case: 15-24, 25-40, 41+
dhs = dhs %>%
  mutate(agecat = case_when(
    hv105 <=24 ~ 1, # 15-24
    hv105 >24 & hv105 <=40 ~ 2, # 25-40
    hv105 >40  ~ 3, # 41-49
    TRUE ~ hv105
  ) %>% as.numeric()
  )
addmargins(table(dhs$agecat, useNA = "always"))

# sex (hv104)----------------------------------------
addmargins(table(dhs$hv104,useNA = "always")) # from dhs codebook, male=1, female=2
class(dhs$hv104)
dhs$sex <- ifelse(dhs$hv104==2,0,dhs$hv104) #switch to 0/1 coding, female=0, male=1
addmargins(table(dhs$sex,useNA = "always"))

# Province----------------------------------------
dhs$largeprovince <- dhs$hv024
#grouping provinces together into 8 levels instead of 11
dhs = dhs %>%
  mutate(largeprovince = case_when(
    hv024 == 6 ~ 5,
    hv024 == 9 ~ 8,
    hv024 == 11 ~ 8,
    TRUE ~ hv024
  ) %>% as.numeric()
  )
addmargins(table(dhs$largeprovince, dhs$hv024 ,useNA = "always"))
# even bigger groupings - 5 total
dhs = dhs %>%
  mutate(provbigcat = case_when(
    largeprovince >0 & largeprovince <4 ~ 1, # Kinshasa, bas congo, bandudu
    largeprovince == 4 ~ 2, # Equateur
    largeprovince == 5 ~ 3, # Kasais
    largeprovince == 7 ~ 4, # Katanga
    largeprovince == 8 ~ 5, #Maniema and kivus
    largeprovince == 10 ~ 5,  # orientale with kivus
    TRUE ~ largeprovince
  ) %>% as.numeric()
  )

# bed net ----------------------------------------
# bednet hml12 and hml20
# hv227: has mosquito bed net for sleeping
table(dhs$hv227)
# hml1: number of mosquito bed nets 
table(dhs$hml1)
# type of mosquito bed net person slept under (0 did not sleep under one, 1 ITN, 2 both trted and untrt, 3 only trt)
addmargins(table(dhs$hml12, useNA = "always"))
addmargins(table(dhs$hml12, dhs$hml20 ,useNA = "always"))

dhs = dhs %>%
  mutate(trtnet = case_when(
    hml12 == 1 & hml20 == 1 ~ 2, # when both variables say treated net then = 2
    hml12 == 1 & hml20 == 0 ~ 1, # when one says treated and other not treated, then code as untreated (1)
    hml12 == 3 & hml20 == 0 ~ 1, # untreated coded as 1, untreated
    hml12 == 0 & hml20 == 0 ~ 0, # no net is 0
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
# check 
addmargins(table(dhs$hml12, dhs$trtnet ,useNA = "always"))

dhs$trtnet <- as.factor(dhs$trtnet)

# model for 3-level variable for bed net
bedtest <- glm(pfldh_adult~trtnet, family = binomial(link="identity"), data=dhs)
summary(bedtest)[["coefficients"]]
confint(bedtest)

#conclusion: only 228 of 17,000+ slept under untreated net so consolidating to treated net vs other

dhs = dhs %>%
  mutate(treatedbednet = case_when(
    trtnet == 2 ~ 1, # treated net
    trtnet == 1 ~ 0, # untreated as 0
    trtnet == 0 ~ 0, # no net code as 0
    TRUE ~ NA_real_
  ) %>% as.numeric()
  )
addmargins(table(dhs$treatedbednet, dhs$trtnet ,useNA = "always"))

# Agri land ----------------------------------------
# owns land for agriculture: hv244
addmargins(table(dhs$hv244, useNA = "always"))
dhs$landown <- as.factor(dhs$hv244)
dhs <- dhs %>% filter(!is.na(hv244)) #remove NaN
dhs <- dhs %>% 
  dplyr::mutate(landown=factor(
    hv244, 
    levels = c(0, 1),
    labels = c("Doesn't own land", "Owns land")))
addmargins(table(dhs$landown,dhs$hv244, useNA = "always"))


# urban/rural----------------------------------------
# hv025: urban(1), rural (2)
addmargins(table(dhs$hv025, useNA = "always"))

dhs$urbanrural <- ifelse(dhs$hv025==1,0,1) # 0 =urban, 1=rural

addmargins(table(dhs$urbanrural, dhs$hv025 ,useNA = "always"))

# Modern housing----------------------------------------

# MODERN HOUSING COMPOSITE VARIABLE
# main floor material hv213
# main wall material hv214
# main roof material hv215
# Deutsch-Feldman 2020 BMJ grouping
addmargins(table(dhs$hv213, useNA = "always"))
addmargins(table(dhs$hv214, useNA = "always"))
addmargins(table(dhs$hv215, useNA = "always"))

# Modern floor: vinyl, asphalt, ceramic tiles, cement, or carpet (hv213 = 32, 33, 34, 35)
dhs$modernfloor <- as.numeric(0)

dhs = dhs %>%
  mutate(modernfloor = case_when(
    hv213 >= 32 & hv213 <= 35 ~ 1,
    is.nan(hv213) ~ NA_real_,
    TRUE ~ 0
  ) %>% as.numeric()
  )
addmargins(table(dhs$modernfloor, dhs$hv213 ,useNA = "always"))
# Modern wall: cement, stone, bricks, or covered adobe (31, 32, 33, 34, 35)
dhs$modernwall <- as.numeric(0)

dhs = dhs %>%
  mutate(modernwall = case_when(
    hv214 >= 31 & hv214 <= 35 ~ 1,
    is.nan(hv214) ~ NA_real_,
    TRUE ~ 0
  ) %>% as.numeric()
  )
addmargins(table(dhs$modernwall, dhs$hv214 ,useNA = "always"))

# Modern roof: metal, zinc/cement, tiles/slate, or cement (31, 33, 34, 35)
dhs$modernroof <- as.numeric(0)

dhs = dhs %>%
  mutate(modernroof = case_when(
    hv215 == 31  ~ 1,
    hv215 >= 33 & hv215 <= 35 ~ 1,
    is.nan(hv215) ~ NA_real_,
    TRUE ~ 0
  ) %>% as.numeric()
  )
addmargins(table(dhs$modernroof, dhs$hv215 ,useNA = "always"))

# modern housing
dhs$modernhousing <- as.numeric(0)

dhs = dhs %>%
  mutate(modernhousing = case_when(
    modernfloor == 1 & modernroof == 1 & modernwall == 1 ~ 1,
    is.na(modernfloor) & is.na(modernroof) & is.na(modernwall) ~ NA_real_,
    TRUE ~ 0
  ) %>% as.numeric()
  )
addmargins(table(dhs$modernfloor, dhs$modernwall ,useNA = "always"))
addmargins(table(dhs$modernhousing, useNA = "always"))
# 6 missing floor, 5 missing wall

# test model with each part AND interaction of all 3
modernhouse_types <- glm(pfldh_adult~animalown+modernfloor+modernwall+modernroof+modernhousing, family = binomial(link="identity"), data=dhs)
summary(modernhouse_types)[["coefficients"]]
confint(modernhouse_types)
# modern housing 
modernhouse <- glm(pfldh_adult~animalown+modernfloor+modernwall+modernroof, family = binomial(link="identity"), data=dhs)
summary(modernhouse)[["coefficients"]]
confint(modernhouse_types)

# conclusion: best to use composite modernhousing 

# other covariates with no recoding required:
# wealth index is hv270
# anemia is ha57

saveRDS(dhs, "./dhs_drc_adults.rds") # full dataset with old and new variables

# subset to variables used in analysis
dhs2 <- dhs %>% 
dplyr::select("hhid", "ha64", "hb64",starts_with("hv"), "pfldh_adult", "animalown", starts_with("cattle"),
              "chickens", "onlychickens","horses", "goats", "sheep", "ducks", "pigs","hml12", "treatedbednet",starts_with("modern"), 
              "shnprovin", "urbanrural", "sex", "ha57", "geometry")

saveRDS(dhs2, "./dhs_drc_adults_sub.rds") # save version with subset of all variables to reduce file size




