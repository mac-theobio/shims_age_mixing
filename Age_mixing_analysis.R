# ==================================================
# Linear mixed effects model for age mixing analysis
# ==================================================

# ==============
# load libraries
# ==============
library(tidyverse)
library(data.table)
library(lattice) #xyplot
library(nlme) # fitting lmm
# ==============
# load data
# ==============
load("T1.agemix.Rdata")
theme_set(theme_bw()) # set global plot theme
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
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- DT.Agemix %>% 
  filter(Gender == "Male" & Participant.age >= 15) %>% 
  mutate(Participant.age = Participant.age - 15) %>% 
  select(-Gender) %>% # drop redundant gender variable
  drop_na()

summary(DT.Agemix.men)
# the distribution of the partner age: the response
ggplot(DT.Agemix.men,aes(x= Partner.age)) +
  geom_histogram(binwidth = 1)

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Age") +
  ylab("Partner age")


n_distinct(DT.Agemix.men$Uid) # to obtain the number of unique participants which will form the clusters

# men who reported more than 1 partner
sum(table(DT.Agemix.men$Uid)>1)

# =======================
# Random intercept model
# =======================
agemix.model.nlme <- lme(Partner.age~ Participant.age, 
                         data = DT.Agemix.men,
                         method = "REML",
                         random = ~1|Uid)

summary(agemix.model.nlme)

ranef(agemix.model.nlme) # to extract random effects from the model

fixef(agemix.model.nlme) # to extract the fixed effects estimates

fitted(agemix.model.nlme) # to obtain the fitted values

AIC(agemix.model.nlme)

coef(agemix.model.nlme) # to obtain the coefficient for each participant

# to fit using Maximum likelihood use update
agemix.model.nlmeML <- update(agemix.model.nlme, method = "ML")
summary(agemix.model.nlmeML)

library(effects)
plot(allEffects(agemix.model.nlme),rug=F)

# # a convenient way to plot the random eﬀects using lmer with 95% conﬁdence intervals along with the estimated random eﬀects.
# dotplot(ranef(agemix.model.1, condVar=T),
#         ylab = "Participant ID",
#         scales = list(y = list(draw = FALSE)))

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Age") +
  ylab("Partner age")
  geom_abline(intercept = fixef(agemix.model.nlme)[1], slope = fixef(agemix.model.nlme)[2], col = "red")+
  geom_abline(intercept = coef(agemix.model.nlme)["0000208",]$`(Intercept)`,
              slope = coef(agemix.model.nlme)["0000208",]$Participant.age, 
              col = "blue") +
  geom_abline(intercept = coef(agemix.model.nlme)["0012036",]$`(Intercept)`,
              slope = coef(agemix.model.nlme)["0012036",]$Participant.age, 
              col = "green")

agemix.model.2nlme <- lme(Partner.age~ Participant.age + Partner.type, 
                          data = DT.Agemix.men,
                          method = "REML",
                          random = ~1|Uid)

summary(agemix.model.2nlme)

agemix.model.2nlmeML <- update(agemix.model.2nlme, method = "ML")

anova(agemix.model.nlmeML, agemix.model.2nlmeML)
# adding partner type does not improve the model

# fit a marginal model using gls
agemix.model.gls <- gls(Partner.age~ Participant.age, 
                        data = DT.Agemix.men)

summary(agemix.model.gls)

# do a likelihood ratio test 
anova(agemix.model.gls, agemix.model.nlme)
# the result suggests that random participant effect should be retained (P < 0.05)
# ==================
# Heteroscedasticity
# ==================

# heteroscedastic errors in nlme
# plotting residuals against the fitted values - detecting heteroscedasticity
plot(agemix.model.nlme, residuals(.) ~ fitted(.), abline = 0 )

# check why there is heteroscedasticity
plot(agemix.model.nlme, residuals(.) ~ Participant.age, abline = 0)
# confirms that the within group variability increases with participant age 
# var(eij) increases when participant age increases

agemix.model.nlme.hetero <- lme(Partner.age~ Participant.age, 
                                 data = DT.Agemix.men,method = "REML",
                                 weights = varPower(value = 0, # this starting point is the homoscedastic form
                                                    form = ~Participant.age),
                                 random = ~1|Uid)

summary(agemix.model.nlme.hetero)

#=====
# proving how the variance function is computed. This shows if you are older the variance is 
# much higher 

DT.Agemix.men.new <- DT.Agemix.men
DT.Agemix.men.new$varweight = 1/abs(DT.Agemix.men$Participant.age)^0.6198723 

#=====

agemix.model.nlme.hetero1 <- lme(Partner.age~ Participant.age, 
                            data = DT.Agemix.men,method = "REML",
                            weights = varConstPower(form = ~Participant.age, fixed = list(const =1)),
                            random = ~1|Uid)

summary(agemix.model.nlme.hetero1)


agemix.model.nlme.hetero2 <- lme(Partner.age~ Participant.age, 
                                data = DT.Agemix.men,method = "REML",
                                weights = varPower(value = 0, # this starting point is the homoscedastic form
                                                   form = ~1 + Participant.age),
                                random = ~1|Uid)

summary(agemix.model.nlme.hetero2)
# test the significance of the heteroscedastic model. heteroscedastic model is much better 
# as shown by the significant decrease in AIC.
anova(agemix.model.nlme,agemix.model.nlme.hetero1)

# compare the heteroscedastic models
anova(agemix.model.nlme.hetero,agemix.model.nlme.hetero1)
anova(agemix.model.nlme.hetero1,agemix.model.nlme.hetero2)
# plot the standardized residuals shows a reasonably homogeneous pattern of the variability
# of the residuals

plot(agemix.model.nlme.hetero, resid(., type = "normalized") ~ Participant.age, abline = 0)
plot(agemix.model.nlme.hetero1, resid(., type = "normalized") ~ Participant.age, abline = 0)

# assess the variability of the variance parameter estimate
intervals(agemix.model.nlme.hetero1)


