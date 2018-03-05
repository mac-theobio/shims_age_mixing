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


# =========================
# Exploratory data analysis
# =========================
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

boxplot(DT.Agemix.men$Partner.age, horizontal = T)

# remove outliers (major = 3*IQR)
DT.Agemix.men <- filter(DT.Agemix.men, Partner.age <= 37)

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

# ================================
# Step 1, model with no covariates
# ================================
agemix.M0 <- lme(Partner.age ~ 1, 
                              data = DT.Agemix.men,
                              method = "REML",
                              random = ~1|Uid)

summary(agemix.M0)
# null model helps us understand the structure of the data. Gives baseline AIC/BIC values
ICC <- 1.410639^2/(1.410639^2 + 3.921109^2)
ICC
# ICC = 0.11 which means that the correlation of partner age score within an individual is 0.11


# fit a marginal model using gls
agemix.M0.gls <- gls(Partner.age ~ 1, 
                        data = DT.Agemix.men)

summary(agemix.M0.gls)

# do a likelihood ratio test 
anova(agemix.M0.gls, agemix.M0)
# the result suggests that random participant effect should be retained (P < 0.05)

# ===================================
# Step 2, adding level one covariate
# ===================================
agemix.M1 <- lme(Partner.age ~  Partner.type, 
                         data = DT.Agemix.men,
                         method = "REML",
                         random = ~1|Uid)

summary(agemix.M1)

anova(update(agemix.M0, method= "ML"),update(agemix.M1, method= "ML"))
# not significant so we keep our null model

# =================================
# Step 3, adding level 2 covariate
# =================================

agemix.M2 <- lme(Partner.age ~  Participant.age, 
                         data = DT.Agemix.men,
                         method = "REML",
                         random = ~1|Uid)

summary(agemix.M2)

intervals(agemix.M2)
ranef(agemix.M2) # to extract random effects from the model

fixef(agemix.M2) # to extract the fixed effects estimates

fitted(agemix.M2) # to obtain the fitted values

coef(agemix.M2) # to obtain the coefficient for each participant

# to fit using Maximum likelihood use update and do a likelihood ratio test 
anova(update(agemix.M0, method= "ML"), update(agemix.M2, method= "ML"))
# the result suggests that the new model should be retained (P < 0.0001)

library(effects)
plot(allEffects(agemix.M2),rug=F)

# # a convenient way to plot the random eﬀects using lmer with 95% conﬁdence intervals along with the estimated random eﬀects.
# dotplot(ranef(agemix.model.1, condVar=T),
#         ylab = "Participant ID",
#         scales = list(y = list(draw = FALSE)))


# agemix.M3 <- lme(Partner.age~ Participant.age + Partner.type, 
#                           data = DT.Agemix.men,
#                           method = "REML",
#                           random = ~1|Uid)
# 
# summary(agemix.M3)
# 
# agemix.M3ML <- update(agemix.M3, method = "ML")
# 
# anova(agemix.M2ML, agemix.M3ML)
# # adding partner type does not improve the model


# ==================
# Heteroscedasticity
# ==================

# heteroscedastic errors in nlme
# plotting residuals against the fitted values - detecting heteroscedasticity

png("detecthetero.png", 
    width = 6.25,
    height = 5.25,
    units = "in",
    res = 1200, 
    pointsize = 4)
plot(agemix.M2, residuals(.) ~ fitted(.), abline = 0 )
dev.off()

# check why there is heteroscedasticity

png("detecthetero1.png", 
    width = 6.25,
    height = 5.25,
    units = "in",
    res = 1200, 
    pointsize = 4)
plot(agemix.M2, residuals(.) ~ Participant.age, abline = 0)
dev.off()

# confirms that the within group variability increases with participant age 
# var(eij) increases when participant age increases

agemix.M4 <- lme(Partner.age~ Participant.age, 
                                 data = DT.Agemix.men,method = "REML",
                                 weights = varPower(value = 0, # this starting point is the homoscedastic form
                                                    form = ~Participant.age),
                                 random = ~1|Uid)

summary(agemix.M4)

# compare the homoscedastic(constant variance) and heteroscedastic models
anova(agemix.M2,agemix.M4)
# the result of this likelihood ratio test is significant (p<0.001), we choose the 
# heteroscedastic model as our preferred model.

#=====
# proving how the variance function is computed. This shows if you are older the variance is 
# much higher 
# 
# DT.Agemix.men.new <- DT.Agemix.men
# DT.Agemix.men.new$varweight = 1/abs(DT.Agemix.men$Participant.age)^0.6198723 
#
#=====

agemix.M5 <- lme(Partner.age~ Participant.age, 
                                 data = DT.Agemix.men,method = "REML",
                                 weights = varPower(value = 0, # this starting point is the homoscedastic form
                                                    form = ~ Participant.age + 1),
                                 random = ~1|Uid)

summary(agemix.M5)

# compare the heteroscedastic models
AIC(agemix.M4,agemix.M5)
# new model, agemix.M5, has a much smaller AIC/BIC and so we choose it as our preferred
# model
 
agemix.M6 <- lme(Partner.age~ Participant.age, 
                            data = DT.Agemix.men,method = "REML",
                            weights = varConstPower(form = ~Participant.age, fixed = list(const =1)),
                            random = ~1|Uid)

summary(agemix.M6)

# compare the heteroscedastic models
AIC(agemix.M5,agemix.M6)
# these two models have the same AIC so we retain our old model as our preferred model.

# plot the standardized residuals shows a reasonably homogeneous pattern of the variability
# of the residuals

plot(agemix.M5, resid(., type = "normalized") ~ Participant.age, abline = 0)

# assess the variability of the variance parameter estimate
intervals(agemix.M5)

# =======
# plots
# =======

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Participant's age") +
  ylab("Partner's age") + 
  scale_x_continuous(labels = function(x)x+15, breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  coord_fixed()+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) + 
  geom_abline(aes(intercept = fixef(agemix.M5)[["(Intercept)"]],slope = fixef(agemix.M5)[["Participant.age"]], color = "Population average"),
              size = 1.25) +
  geom_abline(aes(intercept = 15,slope =1,color = "Same age (x = y)"), size = 1.25) +
  scale_colour_manual(name="Line Colour",
                      values=c("Population average" = "red", "Same age (x = y)" ="#1B9E77")) 

ggsave("Agemixing.png", width = 6.25, height = 5.25,dpi = 1200)
