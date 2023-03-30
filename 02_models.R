# Script to run regression models to estimate malaria prev by animals

# Load packages-------------------------------------------------------
library(survey)
library(srvyr)
library(tableone)
library(EpiStats)
library(lmtest)
library(gplots)
library(tidyverse)
library(fastDummies)
library(gmodels)
library(readxl)
library(mgcv)
library(splines)
library(plotrix)
library(nloptr)
library(lme4)
library(flextable)
library(RColorBrewer)

# Load data------------------------------------------------------------------

dhs2 <- readRDS("dhs_drc_adults_sub.rds")
# created in 01_datacleaning.R

# add new variables for below sections
dhs2 <- dhs2 %>% mutate(
  crossemm = case_when(
    hv244==1 & cattle==1 ~ "land and cattle",
    hv244==1 & cattle==0 ~ "land no cattle",
    hv244==0 & cattle==1 ~ "no land, cattle",
    hv244==0 & cattle==0 ~ "neither",
    TRUE ~ NA_character_))

dhs2 <- dhs2 %>% mutate(
  crossemm_chick = case_when(
    hv244==1 & chickens==1 ~ "land and chick",
    hv244==1 & chickens==0 ~ "land no chick",
    hv244==0 & chickens==1 ~ "no land, chick",
    hv244==0 & chickens==0 ~ "neither",
    TRUE ~ NA_character_))

# MODEL------------------------------------------------------------------
# weight data
dhs2$hh_weight <- dhs2$hv005/1000000
library(survey)
library(srvyr)

designf <-svydesign(ids=dhs2$hv001, strata=dhs2$hv022 , weights=dhs2$hh_weight,  data=dhs2)

options(survey.lonely.psu="adjust")

designf_dhs2 <-as_survey_design(designf) 
# pick variables
vars <- c('chickens','cattle','goats','sheep','pigs','ducks','horses')

# function for bivariate models
survmodel <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('pfldh_adult ~', var)), designf_dhs2, family=quasibinomial("identity"))
  cbind(tidy(m), confint(m)) %>% filter(term==var)}

# run
library(broom)
glmresults <- map_dfr(vars,survmodel) 
glmresults %>% print(noSpaces=T) 

# function for multivariate models
survmodeladjust <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('pfldh_adult ~', var, '+ modernhousing + sex + 
  treatedbednet + urbanrural + hv270')), designf_dhs2, family=quasibinomial("identity"))
  cbind(tidy(m), confint(m)) %>% filter(term==var)}

# run
glmresults <- map_dfr(vars,survmodeladjust) 
colnames(glmresults) <- c('term','estimate','std.error','statistic','p.value','LCI','UCI')
# sort by estimate
glmresults <- glmresults %>% arrange(estimate)
glmresults$ID <- row_number(rev(glmresults$estimate))
glmresults %>% print(noSpaces=T) 

# cattle herd size----------------------------------------------------------------------
# unadjusted
cat5 <- svyglm(pfldh_adult ~ as.factor(cattleherd5), designf_dhs2, family=quasibinomial("log"))
summary(cat5)
confint(cat5)

cat10 <- svyglm(pfldh_adult ~ as.factor(cattleherd10), designf_dhs2, family=quasibinomial("log"))
summary(cat10)
confint(cat10)
# adjusted
cat5_adj <- svyglm(pfldh_adult ~ as.factor(cattleherd5)+ modernhousing + sex + 
                 treatedbednet + urbanrural + hv270, designf_dhs2, family=quasibinomial("log")) # identity NOT CONVERGING
summary(cat5_adj)
confint(cat5_adj)

cat10 <- svyglm(pfldh_adult ~ as.factor(cattleherd10)+ modernhousing+ sex + 
                  treatedbednet + urbanrural + hv270, designf_dhs2, family=quasibinomial("log")) # also probably won't work
summary(cat10)
confint(cat10)

# modification by land ownership and modern housing--------------------------------------------------------------------------------
# weighted counts - using new variables above

#cattle 3 cross tab
svyby(~crossemm,~pfldh_adult, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
#chicken 3 cross tab
svyby(~crossemm_chick,~pfldh_adult, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()

# calc differences
modland <- svyglm(pfldh_adult ~ chickens+ modernhousing+ sex + 
                  treatedbednet + urbanrural + hv270 +landown, designf_dhs2, family=quasibinomial("logit")) # identity and log not converging
summary(modland)

modland_cat <- svyglm(pfldh_adult ~ cattle+ modernhousing+ sex + 
                    treatedbednet + urbanrural + hv270 +landown, designf_dhs2, family=quasibinomial("logit")) # identity and log not converging
summary(modland_cat)

# stratify to get CI
#owns land
ownsland <- designf_dhs2 %>% filter(landown==1)
chickenland <- svyglm(pfldh_adult ~ chickens+ modernhousing+ sex + 
                    treatedbednet + urbanrural + hv270, ownsland, family=quasibinomial("logit")) # identity and log not converging
summary(chickenland)
confint(chickenland)
#cattle
cattleland <- svyglm(pfldh_adult ~ cattle+ modernhousing+ sex + 
                        treatedbednet + urbanrural + hv270, ownsland, family=quasibinomial("logit")) # identity and log not converging
summary(cattleland)
confint(cattleland)

#doesn't own land
noland <- designf_dhs2 %>% filter(landown==0)
chickennoland <- svyglm(pfldh_adult ~ chickens+ modernhousing+ sex + 
                        treatedbednet + urbanrural + hv270, noland, family=quasibinomial("logit")) # identity and log not converging
summary(chickennoland)
confint(chickennoland)
#cattle
cattlenoland <- svyglm(pfldh_adult ~ cattle+ modernhousing+ sex + 
                       treatedbednet + urbanrural + hv270, noland, family=quasibinomial("logit")) # identity and log not converging
summary(cattlenoland)
confint(cattlenoland)

# histogram
library(ggtext)
# animal outline labels from: http://www.phylopic.org/image/a9297cbd-10ca-457b-b5d5-a0b038720df7/
# browse by animal species
labels <- c()
# create a label string telling R where to find the image file
for (i in 1:length(glmresults$term)){
  img.name <- glmresults$term[i]
  labels <- c(labels, paste0("<img src='./images/", glmresults$term[i], ".png", "' width='25' /><br>*", img.name,"*"))
}

ggplot(glmresults, aes(x=ID, y=estimate*100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=ID, y=estimate*100, ymin=LCI*100, ymax=UCI*100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2) + 
  geom_point(shape=15, size=5, aes(color=term), show.legend=F, alpha=0.7) + 
  coord_flip() + theme_bw() + 
  scale_x_continuous(breaks=glmresults$ID, labels=labels, trans = "reverse") + 
  labs(x="Animal", y="Adjusted risk difference") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) 
# have tried arrange, fct_inorder, and fct_inseq to order the animals by estimate not alphabetical

ggsave('./plots/model_hist.png', width=6, height=4)

# eps/vectorized formats for publication - use cairo to preserve transparency, ensure XQuartz installed
ggsave('./plots/model_hist.eps', width=6, height=4, device = cairo_ps,fallback_resolution = 600)


# does malaria prev vary if a household owns only chickens vs chickens+other animals vs no chickens?
## chickens only vs chicken+other-------------------
# unadjusted
onlychick <- svyglm(pfldh_adult ~ as.factor(onlychickens), designf_dhs2, family=quasibinomial("identity"))
summary(onlychick)
confint(onlychick)

#adjusted
onlychickadj <- svyglm(pfldh_adult ~ as.factor(onlychickens)+ modernhousing+ sex + 
                  treatedbednet + urbanrural + hv270, designf_dhs2, family=quasibinomial("identity")) # also probably won't work
summary(onlychickadj)
confint(onlychickadj)

# Conclusion: in unadj, both types of chicken ownership have higher PD; in adjusted, owning only chickens has PD above null compared with
# no chickens; and chickens+ other animals crosses null when compared to no chickens






