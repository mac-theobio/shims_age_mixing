# ==================================================
# Linear mixed effects model for age mixing analysis
# ==================================================

# ==============
# load libraries
# ==============
library(tidyverse)
library(lattice) #xyplot
library(nlme) # fitting lmm
# ==============
# load data
# ==============
#load("~/Documents/shims_age_mixing/DT.Agemix.men.5.Rdata")

load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata")
theme_set(theme_bw()) # set global plot theme

# =========================
# Subset and Exploratory data analysis
# =========================
DT.Agemix.men <- select(DT.Agemix.men,
                        "Uid",
                        "Participant.age",  
                        "Partner.age") %>% drop_na(Partner.age)

summary(DT.Agemix.men)
# men who reported 1,2,3 partner
sum(table(DT.Agemix.men$Uid) == 1)
sum(table(DT.Agemix.men$Uid) == 2)
sum(table(DT.Agemix.men$Uid) == 3)

# the distribution of the partner age: the response
ggplot(DT.Agemix.men,aes(x= Partner.age)) +
  geom_histogram(binwidth = 1)

boxplot(DT.Agemix.men$Partner.age, horizontal = T)

# remove extreme outliers (major = 3*IQR)
H = 3*IQR(DT.Agemix.men$Partner.age)
U = quantile(DT.Agemix.men$Partner.age, probs = 0.75) + H
L = quantile(DT.Agemix.men$Partner.age, probs = 0.25) - H

DT.Agemix.men <- filter(DT.Agemix.men, Partner.age >= L & Partner.age <= U)

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Age") +
  ylab("Partner age") +
  scale_x_continuous(labels = function(x)x+15, breaks = scales::pretty_breaks(n = 10)) 

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
ICC <- as.numeric(VarCorr(agemix.M0)[1])/(as.numeric(VarCorr(agemix.M0)[1]) + as.numeric(VarCorr(agemix.M0)[2]))
ICC
# ICC = 0.49 which means that the correlation of partner age score within an individual


# fit a marginal model using gls
agemix.M0.gls <- gls(Partner.age ~ 1, 
                        data = DT.Agemix.men)

summary(agemix.M0.gls)

# do a likelihood ratio test 
anova(update(agemix.M0, method= "ML"),update(agemix.M0.gls, method = "ML"))
# the result suggests that random participant effect should be retained (P < 0.0001)

# # ===================================
# # Step 2, adding level one covariate
# # ===================================
# agemix.M1 <- lme(Partner.age ~  Partner.type, 
#                          data = DT.Agemix.men,
#                          method = "REML",
#                          random = ~1|Uid)
# 
# summary(agemix.M1)
# 
# anova(update(agemix.M0, method= "ML"),update(agemix.M1, method= "ML"))
# # not significant so we keep our null model
# 
# =================================
# Step 3, adding level 2 covariate
# =================================

agemix.M2 <- lme(Partner.age ~  Participant.age, 
                         data = DT.Agemix.men,
                         method = "REML",
                         random = ~1|Uid)

summary(agemix.M2)

plot(agemix.M2)

intervals(agemix.M2)
#ranef(agemix.M2) # to extract random effects from the model

fixef(agemix.M2) # to extract the fixed effects estimates

fitted(agemix.M2) # to obtain the fitted values

coef(agemix.M2) # to obtain the coefficient for each participant
VarCorr(agemix.M2)
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

# extract the fitted values and the residuals from the agemix.M2 model
heteroscedastic <- tibble(
  residuals.M2 = residuals(agemix.M2),
  fitted.M2 = fitted(agemix.M2),
  Participant.age = agemix.M2$data$Participant.age
)

# plotting residuals against the fitted values - detecting heteroscedasticity

ggplot(heteroscedastic, aes(fitted.M2, residuals.M2)) +
  geom_point(size=3.5,color="black",alpha = 0.5)+
  geom_hline(yintercept = 0) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) + 
  
ggsave("detecthetero.png", width = 6.25, height = 5.25,dpi = 600)

# check why there is heteroscedasticity

ggplot(heteroscedastic, aes(Participant.age, residuals.M2)) +
  geom_point(size=3.5,color="black",alpha = 0.5) +
  geom_hline(yintercept = 0) +
  xlab("Participant age") +
  ylab("Residuals") +
  scale_x_continuous(labels = function(x)x+15, breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  coord_fixed()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) + 

ggsave("detecthetero1.png", width = 6.25, height = 5.25,dpi = 600)

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
                            weights = varConstPower(form = ~Participant.age, fixed = list(const = 1)),
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_fixed()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) + 
  geom_abline(aes(intercept = fixef(agemix.M5)[["(Intercept)"]],slope = fixef(agemix.M5)[["Participant.age"]], color = "Population average"),
              size = 1.25) +
  geom_abline(aes(intercept = 15,slope =1,color = "Same age (x = y)"), size = 1.25) +
  scale_colour_manual(name="Line Colour",
                      values=c("Population average" = "#D95F02", "Same age (x = y)" ="dodgerblue")) 

ggsave("Agemixing.png", width = 6.25, height = 5.25,dpi = 600)


# function to compute within group error variance(standard deviation)

sdij <- function(participant.age = 15){
  sd = sqrt(agemix.M5[["sigma"]]^2 * (abs(participant.age-15 +1))^(2* as.numeric(agemix.M5[["modelStruct"]][["varStruct"]])))
  return(sd)
  }
sdij(participant.age = 30)

lci.15 = fixef(agemix.M5)[["(Intercept)"]] - 1.96 * sdij(participant.age = 15)
uci.15 = fixef(agemix.M5)[["(Intercept)"]] + 1.96 * sdij(participant.age = 15)
lci.30 = (fixef(agemix.M5)[["(Intercept)"]] + fixef(agemix.M5)[["Participant.age"]]*(30-15)) - (1.96 * sdij(participant.age = 30))
uci.30 = (fixef(agemix.M5)[["(Intercept)"]] + fixef(agemix.M5)[["Participant.age"]]*(30-15)) + (1.96 * sdij(participant.age = 30))

ci <- function(model = agemix.M5, participant.age = 15){
  lci = (fixef(model)[["(Intercept)"]] + fixef(model)[["Participant.age"]]*(participant.age-15)) - (1.96 * sdij(participant.age))
  uci = (fixef(model)[["(Intercept)"]] + fixef(model)[["Participant.age"]]*(participant.age-15)) + (1.96 * sdij(participant.age))
  conf_int = data.frame(participant.age,lci, uci)
  return(conf_int)
}

part <- seq(15,49,1)
confint <- ci(model = agemix.M5, participant.age = part)

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Participant's age") +
  ylab("Partner's age") + 
  scale_x_continuous(labels = function(x)x+15, breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_fixed()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) + 
  geom_abline(aes(intercept = fixef(agemix.M5)[["(Intercept)"]],slope = fixef(agemix.M5)[["Participant.age"]], color = "Population average"),
              size = 1.25) +
  #geom_abline(aes(intercept = 15,slope =1,color = "Same age (x = y)"), size = 1.25) +
  scale_colour_manual(name="Line Colour",
                      values=c("Population average" = "#D95F02", "Same age (x = y)" ="dodgerblue"))+
  geom_line(data = confint, aes(x=participant.age-15, y=lci), linetype = "dashed", color="dodgerblue") +
  geom_line(data = confint, aes(x=participant.age-15, y=uci), linetype = "dashed", color="dodgerblue") +
  geom_segment(aes(x=15-15, y = uci.15, xend = 15-15,yend=lci.15, color = "participant age = 15"), color = "dodgerblue")+
  geom_segment(aes(x=30-15, y = uci.30, xend = 30-15,yend=lci.30), color = "dodgerblue")+
  geom_point(aes(x=15-15,y=fixef(agemix.M5)[["(Intercept)"]]), size=4, color = "dodgerblue")+
  geom_point(aes(x=30-15,y=fixef(agemix.M5)[["(Intercept)"]]+fixef(agemix.M5)[["Participant.age"]]*15), size=4, color = "dodgerblue")

ggsave("AgemixingCI.png", width = 6.25, height = 5.25,dpi = 600)
