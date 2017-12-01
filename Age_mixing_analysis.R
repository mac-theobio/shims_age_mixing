library(data.table)
library(ggplot2)
library(dplyr)
library(nlme)

load("T1.agemix.Rdata")
##################################################################################################
# We want to tidy the data frame by subsetting and converting it to long format

T1.agemixing <- T1.agemix %>% transmute(Uid,
                                        Gender,
                                        Age.res.p1,
                                        Age.res.p2,
                                        Age.res.p3,
                                        Partner.age.p1,
                                        Partner.age.p2,
                                        Partner.age.p3)

setDT(T1.agemixing) #convert to a data.table for easy manipulation

DT.Agemix = T1.agemixing %>% melt( measure = patterns("^Age.res", "^Partner.age"),
                                   value.name = c("Participant.age", "Partner.age"),
                                   variable.name = "Partner")

##################################################################################################
# Scatterplot of partner age vs participant age
theme_set(theme_bw())

print(ggplot(na.exclude(DT.Agemix), aes(x=Participant.age, y=Partner.age))
      + geom_point()
      + facet_wrap(~Gender)
      + coord_equal()
      + xlab("")
      + ylab("")
      #+ ggtitle("Partner vs Participant age at onset of sexual relationship")
      )

#################################################################################################
# Fitting a linear mixed model
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- na.exclude(DT.Agemix[which(Gender == "Male" & Participant.age >= 15),])
DT.Agemix.men$Participant.age <- DT.Agemix.men$Participant.age - 15

model <- lme(Partner.age~ Participant.age,
             data = DT.Agemix.men,
             method = "REML",
             #weights = varPower(value = 0.5, form = ~Participant.age + 1),
             random = ~1|Uid)

summary(model)

# Extract slope = Beta-coefficent of the fixed effect from model
slope <- model$coefficients$fixed[2]

# Extract population intercepts = expected age of partner for a man starting a relationship at age 15
intercept <- model$coefficients$fixed[1]

# Extract power coefficient of variance function
power <- (attributes(model$apVar)$Pars["varStruct.power"])
power.lowerbound <- intervals(model)$varStruct[,1]
power.upperbound <- intervals(model)$varStruct[,3]

# Extract between-individual variance
between.var <- VarCorr(model)[1] %>% as.numeric()

# Extract residual variance = within-individual variance
within.var <- VarCorr(model)[2] %>% as.numeric()



# ==================================================
# Linear mixed effects model for age mixing analysis
# ==================================================

# ==============
# load libraries
# ==============
library(tidyverse)
library(lme4)
library(data.table)
# ==============
# load data
# ==============
load("T1.agemix.Rdata")

# ====================
# Data transformations
# ====================

# subset
T1.agemixing <- T1.agemix %>% transmute(Uid,
                                        Gender,
                                        Age.res.p1,
                                        Age.res.p2,
                                        Age.res.p3,
                                        Partner.age.p1,
                                        Partner.age.p2,
                                        Partner.age.p3,
                                        Partner.type.p1,
                                        Partner.type.p2,
                                        Partner.type.p3)

# convert to long format

setDT(T1.agemixing) #convert to a data.table for easy manipulation

DT.Agemix <- T1.agemixing %>% melt( measure = patterns("^Age.res", "^Partner.age", "^Partner.type"),
                                   value.name = c("Participant.age", "Partner.age", "Partner.type"),
                                   variable.name = "Partner") 
as.tibble(DT.Agemix)

# tidy dataset

DT.Agemix.men <- DT.Agemix %>% 
  filter(Gender == "Male" & Participant.age >= 15) %>% 
  mutate(Participant.age = Participant.age - 15) %>% 
  drop_na()

hist(DT.Agemix.men$Partner.age)
# =======================
# linear mixed model
# =======================

agemix.model <- lmer(Partner.age ~ Participant.age + Partner.type + (1|Uid),
                     data = DT.Agemix.men,
                     REML = FALSE)

summary(agemix.model)

coef(agemix.model)

plot(agemix.model)
plot(agemix.model,Partner.age ~ fitted(.))


agemix.model.null <- lmer(Partner.age ~  Participant.age + (1|Uid),
                     data = DT.Agemix.men,
                     REML = FALSE)

summary(agemix.model.null)

anova(agemix.model.null, agemix.model)
# adding partner type does not improve the model
# random slope not possible because of very few observations per subject