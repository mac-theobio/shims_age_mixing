# ==================================================
# Linear mixed effects model for age mixing analysis
# ==================================================

# ==============
# load libraries
# ==============
library(tidyverse)
library(lattice) #xyplot
library(nlme) # fitting lmm
library(gridExtra) # grid arrange
library(gtable) # format the extracted legends
# ==============
# load data
# ==============
#load("~/Documents/shims_age_mixing/DT.Agemix.men.5.Rdata")

load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata")
#load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.2.Rdata")
theme_set(theme_bw()) # set global plot theme

# number of male participants
length(table(DT.Agemix.men$Uid))
partlevels = c("casual partner","regular partner","husband/wife")
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
# =========================
# Subset and Exploratory data analysis
# =========================
DT.Agemix.men.1 <- DT.Agemix.men[is.na(DT.Agemix.men$Partner.age),]
summary(DT.Agemix.men.1)
(table(DT.Agemix.men.1$Partner.type)/553)*100
(table(DT.Agemix.men.1$Condom.frequency)/553)*100
(table(DT.Agemix.men.1$Sex.frequency)/553)*100


DT.Agemix.men <- transmute(DT.Agemix.men,
                          Uid,
                          HHid,
                          SHIMS.EA,
                          Participant.age,
                          Age.difference,
                          Partner.age,
                          Condom.frequency = ordered(Condom.frequency, levels = freqlevels),
                          Sex.frequency = ordered(Sex.frequency, levels = sexlevels),
                          Partner.type = ordered(Partner.type, levels = partlevels, 
                                                 labels =c("casual partner","regular partner", "spouse"))) %>% 
  drop_na(c(Partner.age))

summary(DT.Agemix.men)

# men who reported 1,2,3 partner

x <- count(DT.Agemix.men, Uid)
sum(count(DT.Agemix.men, Uid)$n == 1)
sum(count(DT.Agemix.men, Uid)$n == 2)
sum(count(DT.Agemix.men, Uid)$n == 3)

# (table(DT.Agemix.men$Condom.frequency)/5235)*100
# (table(DT.Agemix.men$Partner.type)/5235)*100
# (table(DT.Agemix.men$Sex.frequency)/5235)*100

# the distribution of the partner age: the response
ggplot(DT.Agemix.men,aes(x= Partner.age)) +
  geom_histogram(binwidth = 1)

boxplot(DT.Agemix.men$Partner.age, horizontal = T)

# remove extreme outliers (major = 3*IQR)
H = 3*IQR(DT.Agemix.men$Partner.age)
U = quantile(DT.Agemix.men$Partner.age, probs = 0.75) + H
L = quantile(DT.Agemix.men$Partner.age, probs = 0.25) - H

DT.Agemix.men <- filter(DT.Agemix.men, Partner.age >= L & Partner.age <= U)


# For dataset which we didnt discard relationships where the man was younger than 12 years
# DT.Agemix.men.2 <- select(DT.Agemix.men.2,
#                         "Uid",
#                         "Participant.age",
#                         "Age.difference",
#                         "Partner.age") %>%  drop_na(c(Partner.age,Participant.age))
# 
# HH = 3*IQR(DT.Agemix.men.2$Partner.age)
# UU = quantile(DT.Agemix.men.2$Partner.age, probs = 0.75) + HH
# LL = quantile(DT.Agemix.men.2$Partner.age, probs = 0.25) - HH
# 
# DT.Agemix.men.2 <- filter(DT.Agemix.men.2, Partner.age >= LL & Partner.age <= UU)
# DT.Agemix.men.2 <- filter(DT.Agemix.men.2, Participant.age > -9)
# DT.Agemix.men.2$Participant.age.more.15 <- as.factor(ifelse(DT.Agemix.men.2$Participant.age > 0, 1, 0))

ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(aes(color = Partner.type), size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner's age at relationship formation") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed()

# ggsave("rawdata.png", width = 6.25, height = 5.25,dpi = 600)

# Alternatively
ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner's age at relationship formation") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_fixed() + 
  theme(panel.grid.minor = element_blank())+
  facet_grid(~ Partner.type)

# ggsave("rawdata_facet.png", width = 8.25, height = 3.25,dpi = 600)

# Age difference
summary(DT.Agemix.men)

ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
  geom_jitter(aes(color = Partner.type), size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner age difference") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed()+
  geom_hline(yintercept = 0, linetype = "dashed")

# ggsave("rawdata.png", width = 6.25, height = 5.25,dpi = 600)

# Alternatively
ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
  geom_jitter(size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner age difference") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed() + 
  theme(panel.grid.minor = element_blank())+
  #facet_grid(~ Condom.frequency) +
  #facet_grid(~ Sex.frequency) +
  facet_grid(~ Partner.type) +
  geom_hline(yintercept = 0, linetype = "dashed")

# ggsave("rawdata_facet.png", width = 8.25, height = 3.25,dpi = 600)

# 2D Density plots
# to avoid overplotting when you have many data points
# calculates the denstity of obsevations in each region of the plot and fills with color
# color corresponds to the density of the data
# for ref: https://www.sharpsightlabs.com/blog/density-plot-in-r/


# ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
#   stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
#   scale_fill_distiller(palette = "Spectral", direction = -1)
# 
# ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
#   stat_density_2d(aes(fill = ..density..), geom = "tile", contour = F) +
#   scale_fill_distiller(palette = "Spectral", direction = -1)
# 
# # geom tile or raster gave same output

ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
  stat_density_2d(aes(fill = ..density..),geom = "raster", contour = F) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  facet_grid(~ Partner.type) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner age difference") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed() 

# ggsave("rawdata_parttype.png", width = 8.25, height = 3.25,dpi = 600)

ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
  stat_density_2d(aes(fill = ..density..),geom = "raster", contour = F) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  facet_grid(~ Condom.frequency) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner age difference") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed() 

# ggsave("rawdata_condomfreq.png", width = 8.25, height = 3.25,dpi = 600)

ggplot(DT.Agemix.men,aes(Participant.age,Age.difference)) +
  stat_density_2d(aes(fill = ..density..),geom = "raster", contour = F) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  facet_grid(~ Sex.frequency) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner age difference") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_fixed() 

# ggsave("rawdata_sexfreq.png", width = 8.25, height = 3.25,dpi = 600)

# transforming the data to see if we can rectify the spread

# log transformation
ggplot(DT.Agemix.men,aes(Participant.age,log(Partner.age))) +
  geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
  xlab("Age") +
  ylab("Partner age") +
  scale_x_continuous(labels = function(x)x+12, breaks = scales::pretty_breaks(n = 10)) 

# Box Cox
# distBCMod <- caret::BoxCoxTrans(DT.Agemix.men$Partner.age)
# print(distBCMod)

# DT.Agemix.men_new <- cbind(DT.Agemix.men, Partner.age_new = predict(distBCMod, DT.Agemix.men$Partner.age))
# 
# ggplot(DT.Agemix.men_new,aes(Participant.age,Partner.age_new)) +
#   geom_jitter(size=3,color="black", width = 0.25, height = 0.25, alpha = 0.5) +
#   xlab("Age") +
#   ylab("Partner age") +
#   scale_x_continuous(labels = function(x)x+12, breaks = scales::pretty_breaks(n = 10)) 

# very bad.
# we model with robust standard errors (non constant variance of errors). This way, we account for
# heteroskedasticity as it exists
# The variability of partner ages among older participants is large
# the distribution of the age of the male participants

summary(DT.Agemix.men)

ggplot(data = DT.Agemix.men, aes(Participant.age + 12)) +
  geom_histogram(bins = 30, col = "gray66", fill = "gray68") +
  xlab("Participant's age") +
  ylab("Relationships") +
  theme(axis.text.x = element_text(size=19), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(10,50)) + 
  scale_y_continuous(breaks = c(0,200,400,600,800), limits = c(0,600))

# ggsave("participanthist.png", width = 6.25, height = 5.25,dpi = 600)

ggplot(data = DT.Agemix.men, aes(Partner.age)) +
  geom_histogram(bins = 30, na.rm = T,col = "gray66", fill = "gray68") +
  xlab("Partner's age") +
  ylab("Relationships") +
  theme(axis.text.x = element_text(size=19), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(10,40)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5.5))

# ggsave("partnerhist.png", width = 6.25, height = 5.25,dpi = 600)

# measures of central tendency and dispersion
summary(DT.Agemix.men$Partner.age)
IQR(DT.Agemix.men$Partner.age)

summary(DT.Agemix.men$Participant.age +12)
IQR(DT.Agemix.men$Participant.age +12)
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

# agemix.M01 <- lme(Partner.age ~  1, 
#                  data = DT.Agemix.men,
#                  method = "REML",
#                  random = list(HHid = ~1,
#                                Uid = ~1))
# summary(agemix.M01)
# 
x <-  lme4::lmer(Partner.age ~ 1 + (1|Uid) + (1|HHid) + (1|SHIMS.EA),
            data = DT.Agemix.men)
sjstats::icc(x)  #from sjstats library

# ICC very low and so we can ignore clustering at higher levels

# agemix.M02 <- lme(Partner.age ~  1, 
#                  data = DT.Agemix.men,
#                  method = "REML",
#                  random = list(SHIMS.EA = ~1,
#                                HHid = ~1,
#                                Uid = ~1))
# summary(agemix.M02)
# fit a marginal model using gls
agemix.M0.gls <- gls(Partner.age ~ 1, 
                        data = DT.Agemix.men)

summary(agemix.M0.gls)

# do a likelihood ratio test 
anova(update(agemix.M0, method= "ML"),update(agemix.M0.gls, method = "ML"))
# the result suggests that random participant effect should be retained (P < 0.0001)

# =================================
# Step 2, adding level 2 covariate
# =================================

agemix.M2 <- lme(Partner.age ~  Participant.age, 
                         data = DT.Agemix.men,
                         method = "REML",
                         random = ~1|Uid)

# agemix.M2 <- lme(Partner.age ~  Participant.age, 
#                  data = DT.Agemix.men,
#                  method = "REML",
#                  random = list(HHid = ~1,
#                                Uid = ~1))
# 
# agemix.M2 <- lme(Partner.age ~  Participant.age, 
#                  data = DT.Agemix.men,
#                  method = "REML",
#                  random = list(SHIMS.EA = ~1,
#                                HHid = ~1,
#                                Uid = ~1))
# summary(agemix.M2)

plot(agemix.M2)

intervals(agemix.M2)
#ranef(agemix.M2) # to extract random effects from the model

fixef(agemix.M2) # to extract the fixed effects estimates

# fitted(agemix.M2) # to obtain the fitted values

# coef(agemix.M2) # to obtain the coefficient for each participant
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
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13)) +
  theme(text=element_text( size=13)) 
  
# ggsave("detecthetero.png", width = 6.25, height = 5.25,dpi = 600)

# check why there is heteroscedasticity

ggplot(heteroscedastic, aes(Participant.age, residuals.M2)) +
  geom_jitter(color = "black",size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  geom_hline(yintercept = 0) +
  xlab("Participant age") +
  ylab("Residuals") +
  scale_x_continuous(labels = function(x)x+12, breaks = scales::pretty_breaks(n = 10), limits = c(0,38)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_fixed()+
  theme(axis.text.x = element_text(size=13), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=13)) +
  theme(text=element_text( size=13))  

# ggsave("detecthetero1.png", width = 6.25, height = 5.25,dpi = 600)

# confirms that the within group variability increases with participant age 
# var(eij) increases when participant age increases

agemix.M4 <- lme(Partner.age~ Participant.age, 
                                 data = DT.Agemix.men,method = "REML",
                                 weights = varPower(value = 0, # this starting point is the homoscedastic form
                                                    form = ~Participant.age),
                                 random = ~1|Uid)

summary(agemix.M4)
intervals(agemix.M4)
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
intervals(agemix.M6)
# compare the heteroscedastic models
AIC(agemix.M5,agemix.M6)
# these two models have the same AIC so we retain our old model as our preferred model.

# plot the standardized residuals shows a reasonably homogeneous pattern of the variability
# of the residuals

# plot(agemix.M5, resid(., type = "normalized") ~ Participant.age, abline = 0)

# assess the variability of the variance parameter estimate
intervals(agemix.M5)

# =======
# plots
# =======

# function to compute within group error variance(standard deviation)
# centered participant ages (participant age - 12)

sdij <- function(participant.age = 12){
  sd = sqrt(agemix.M5[["sigma"]]^2 * (abs(participant.age - 12 +1))^(2* as.numeric(agemix.M5[["modelStruct"]][["varStruct"]])))
  return(sd)
  }
sdij(participant.age = 35)

ci <- function(model = agemix.M5, participant.age = 12){
  lci = (fixef(model)[["(Intercept)"]] + (fixef(model)[["Participant.age"]]*(participant.age - 12)) - (1.96 * sdij(participant.age)))
  uci = (fixef(model)[["(Intercept)"]] + (fixef(model)[["Participant.age"]]*(participant.age - 12)) + (1.96 * sdij(participant.age)))
  conf_int = data.frame(participant.age,lci, uci)
  return(conf_int)
}

part <- seq(12, 48,1)
confint <- ci(model = agemix.M5, participant.age = part)
confint.between <- data.frame(participant.age = part,
                              lci.1 = fixef(agemix.M5)[["(Intercept)"]] + (fixef(agemix.M5)[["Participant.age"]]*(part) - 1.96 * sqrt(as.numeric(VarCorr(agemix.M5)[1]))),
                              uci.1 = fixef(agemix.M5)[["(Intercept)"]] + (fixef(agemix.M5)[["Participant.age"]]*(part) + 1.96 * sqrt(as.numeric(VarCorr(agemix.M5)[1]))))


Agemix.plot <- ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
  geom_jitter(color = "black",size=1, width = 0.25, height = 0.25, alpha = 0.2) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner's age at relationship formation") + 
  scale_x_continuous(labels = function(x)x+12, breaks = seq(-2,50, by=5), expand = c(0,0), limits = c(0,40)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0,0), limits = c(0,45)) +
  coord_fixed()+ 
  theme(axis.text.x = element_text(size=11),panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=11), legend.position = "right") +
  theme(text=element_text(size=11)) + 
  geom_abline(aes(intercept = fixef(agemix.M5)[["(Intercept)"]],slope = fixef(agemix.M5)[["Participant.age"]],color = "Population average"), size = 1.25) +
  geom_abline(aes(intercept = 12,slope =1, color = "Same age"), size = 1.25) +
  geom_line(data = confint, aes(x=participant.age-12, y=lci), size = 1,linetype = "dashed", color="orangered2") +
  geom_line(data = confint, aes(x=participant.age-12, y=uci), size = 1,linetype = "dashed", color="orangered2") +
  scale_colour_manual(name= element_blank(),
                      values=c("Population average" = "orangered2", "Same age" ="#0288d1")) 

Agemix.plot

# ggsave("Agemixing.png", width = 5.25, height = 5.25,dpi = 600)


######
# To display full data (including participants who were less than 12 years) and model fit


Agemix.plot2 <- ggplot(DT.Agemix.men.2,aes(Participant.age,Partner.age)) +
  geom_jitter(aes(color = Participant.age.more.15),size=3, width = 0.25, height = 0.25, alpha = 0.5, show.legend = T) +
  xlab("Participant's age at relationship formation") +
  ylab("Partner's age at relationship formation") + 
  scale_x_continuous(labels = function(x)x+12, breaks = scales::pretty_breaks(n = 10), expand = c(0,0),limits = c(-15,35)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0,0), limits = c(0,45)) +
  coord_fixed()+ 
  theme(axis.text.x = element_text(size=12),panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) + 
  geom_abline(aes(intercept = fixef(agemix.M5)[["(Intercept)"]],slope = fixef(agemix.M5)[["Participant.age"]],color = "Population average"), color = "orangered2", size = 1.25) +
  geom_abline(aes(intercept = 12,slope =1,color = "Same age"), color = "#0288d1", size = 1.25) +
  geom_line(data = confint, aes(x=participant.age-12, y=lci), size = 1,linetype = "dashed", color="orangered2") +
  geom_line(data = confint, aes(x=participant.age-12, y=uci), size = 1,linetype = "dashed", color="orangered2") +
  scale_colour_manual("Participant age", labels = c("Less than 12 years", "12 years or more"),
                      values=c("green","black"))
Agemix.plot2


