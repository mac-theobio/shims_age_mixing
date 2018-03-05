# =============================================================================
# Associations between age difference and relationship characteristics analysis
# =============================================================================

# ==============
# load libraries
# ==============
library(data.table)
library(tidyverse)
library(ordinal)    #for cumulative link mixed models
library(splines)    #or splines in models
library(survival)   #for cox ph model
library(effects)    #to do effects plots
library(cowplot)    #plot_grid
library(dotwhisker) #make dot whisker plots
library(broom)      #convert objects into tidy data frame: tidy()
library(visreg)     #getting "contrast" hazard ratios
#install_github("baptiste/egg")
library(egg)

# ==============
# load data
# ==============
load("T1.agemix.Rdata")
theme_set(theme_bw()) # set global plot theme

# ====================
# Data transformations
# ====================

# We want to tidy the data frame by subsetting and converting it 
# to long format so that each row represents a relationship.

T1.reldata <- T1.agemix %>% transmute(Uid,
                                      Gender,
                                      No.partners,
                                      Age.res.p1,
                                      Age.res.p2,
                                      Age.res.p3,
                                      Age.diff.p1,
                                      Age.diff.p2,
                                      Age.diff.p3,
                                      Condom.freq.p1,
                                      Condom.freq.p2,
                                      Condom.freq.p3,
                                      Sex.freq.p1,
                                      Sex.freq.p2,
                                      Sex.freq.p3,
                                      Partner.type.p1,
                                      Partner.type.p2,
                                      Partner.type.p3,
                                      Rel.dur.p1,
                                      Rel.dur.p2,
                                      Rel.dur.p3,
                                      Rel.ended.p1,
                                      Rel.ended.p2,
                                      Rel.ended.p3,
                                      Money.gifts.p1,
                                      Money.gifts.p2,
                                      Money.gifts.p3)
# code REF as NA
T1.reldata[T1.reldata == "REF"] <- NA

setDT(T1.reldata) #convert to a data.table for easy manipulation

DT.reldata <-  T1.reldata %>% 
  melt(measure = patterns("^Age.res", "^Age.diff",
                          "^Condom.freq", "^Sex.freq",
                          "^Partner.type", "^Rel.dur",
                          "^Rel.ended", "^Money.gifts"), 
       value.name = c("Age.participant", "Age.difference",
                      "Condom.frequency", "Sex.frequency",
                      "Partner.type", "Relationship.dur",
                      "Rel.ended", "Money.gifts"),  
       variable.name = "Partner"
  )


# ===========================================================
# Removing respondents who did not report relationship 1,2,3
# ===========================================================

DT.rel.men <- na.exclude(subset(DT.reldata, Gender == "Male" & Age.participant >= 15))
#DT.rel.men <- na.exclude(subset(DT.reldata, Gender == "Male"))


# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("casual partner","regular partner","husband/wife")

DT.reldata.men <- DT.rel.men %>% 
  transmute(Uid = as.factor(Uid),
            #Gender,
            No.partners,
            Partner,
            Age.participant,
            Age.difference,
            Relationship.dur,
            Rel.ended = as.factor(Rel.ended),
            Condom.frequency = ordered(Condom.frequency, levels = freqlevels),
            Sex.frequency = ordered(Sex.frequency, levels = sexlevels),
            Partner.type = ordered(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)
  )

n_distinct(DT.reldata.men$Uid) # to obtain the number of unique participants which will form the clusters

# ===============
# Condom Use
# ===============

# frequncies of the condom use levels
ggplot(data = DT.reldata.men) +
  geom_bar(aes(Condom.frequency))

# ====================================
# Step 1, ordinary partner level model
# ====================================
# A cumulative logit model that includes the effect of age difference on condom use

condom.M0 <- clm(Condom.frequency ~ Age.difference,
                 data = DT.reldata.men,
                 link = "logit")

summary(condom.M0)

# confidence intervals
confint(condom.M0, type = "Wald")
confint(condom.M0, type = "profile")

# ====================================
# Step 2, participant level model
# ====================================
# cumulative logit random intercept model

condom.M1 <- clmm(Condom.frequency ~ Age.difference + (1|Uid),
                  data = DT.reldata.men,
                  link = "logit",
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(condom.M1)


condom.M1b <- clmm(Condom.frequency ~ Age.difference + (1|Uid),
                   data = DT.reldata.men,
                   link = "logit",
                   Hess = T) #if you need to call summary

summary(condom.M1b)

# M1 is using is using the Gauss-Hermite  quadrature method to compute the likelihood while 
# M1b is using laplace approximation  
# The two fits are different. The Gauss-Hermite method performs better than laplace.

# likelihood ratio test to determine whether the random effect is necessary
anova(condom.M0,condom.M1)

ranef(condom.M1)

# the cumulative probability of sometimes or never use condom for an average participant
plogis(condom.M1$Theta[2] - condom.M1$beta)

