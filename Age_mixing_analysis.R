# ==================================================
# Linear mixed effects model for age mixing analysis
# ==================================================

# ==============
# load libraries
# ==============
library(tidyverse)
library(lme4)
library(data.table)
library(lattice) #xyplot
library(nlme)
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
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- DT.Agemix %>% 
  filter(Gender == "Male" & Participant.age >= 15) %>% 
  mutate(Participant.age = Participant.age - 15) %>% 
  drop_na()

# the distribution of the partner age: the response
hist(DT.Agemix.men$Partner.age)

n_distinct(DT.Agemix.men$Uid) # to obtain the number of unique participants which will form the clusters

# men who reported more than 1 partner
sum(table(DT.Agemix.men$Uid)>1)

# =======================
# Random intercept model
# =======================
agemix.model.nlme <- lme(Partner.age~ Participant.age, 
                         data = DT.Agemix.men,method = "REML",
                         #weights = varPower(value = 0.5, form = ~Participant.age + 1),
                         random = ~1|Uid)

summary(agemix.model.nlme)


agemix.model.1 <- lmer(Partner.age ~  Participant.age + (1|Uid),
                       data = DT.Agemix.men)

summary(agemix.model.1)

ranef(agemix.model.1) # to obtain the actual bj for all the groups

fitted(agemix.model.1) # to obtain the fitted values

AIC(agemix.model.1)

coef(agemix.model.1) # to obtain the coefficient for each participant

# to fit using Maximum likelihood use update
agemix.model.1ML <- update(agemix.model.1, REML = FALSE)
summary(agemix.model.1ML)

# assessing the variability of the parameters by profilling the fitted model
pr01 <- profile(agemix.model.1ML)
xyplot(pr01, aspect = 1.3, layout = c(4,1)) #profile zeta plot
# The vertical lines in the panels delimit the 50%, 80%, 90%, 95% and 99%
# confidence intervals derived from the test statistic.
# To get the actual confidence intervals use confint()
confint(pr01) 

xyplot(pr01, aspect = 1.3, layout = c(4,1), absVal = TRUE) # plot of abs of zeta to visualize confidence intervals easily

splom(pr01)

library(effects)
plot(allEffects(agemix.model.1),rug=F)


# a convenient way to plot the random eﬀects with 95% conﬁdence intervals along with the estimated random eﬀects.
dotplot(ranef(agemix.model.1, condVar=T),
        ylab = "Participant ID",
        scales = list(y = list(draw = FALSE)))
#or
qqmath(ranef(agemix.model.1, condVar=TRUE))

ggplot(data = DT.Agemix.men, aes(x = Participant.age, y = Partner.age)) +
  geom_point()+
  theme_set(theme_bw()) +
  geom_abline(intercept = fixef(agemix.model.1)[1], slope = fixef(agemix.model.1)[2], col = "red")+
  geom_abline(intercept = coef(agemix.model.1)$Uid["0000208",]$`(Intercept)`,
              slope = coef(agemix.model.1)$Uid["0000208",]$Participant.age, 
              col = "blue") +
  geom_abline(intercept = coef(agemix.model.1)$Uid["0012036",]$`(Intercept)`,
              slope = coef(agemix.model.1)$Uid["0012036",]$Participant.age, 
              col = "green")


agemix.model.2 <- lmer(Partner.age ~ Participant.age + Partner.type + (1|Uid),
                     data = DT.Agemix.men)

summary(agemix.model.2)

agemix.model.2ML <- update(agemix.model.2, REML = FALSE)

anova(agemix.model.1ML, agemix.model.2ML)
# adding partner type does not improve the model

# heteroskedastic errors in nlme
# plotting residuals against the fitted values - detecting heteroskedasticity
plot(agemix.model.nlme, residuals(.) ~ fitted(.), abline = 0 )

# check why there is heteroskedasticity
plot(agemix.model.nlme, residuals(.) ~ Participant.age, abline = 0)
# confirms that the within group variability increases with participant age 
# var(eij) increases when participant age increases

agemix.model.nlme.hetero <- lme(Partner.age~ Participant.age, 
                                 data = DT.Agemix.men,method = "REML",
                                 weights = varPower(value = 0, # this starting point is the homoskedastic form
                                                    form = ~Participant.age),
                                 random = ~1|Uid)

summary(agemix.model.nlme.hetero)

agemix.model.nlme.hetero1 <- lme(Partner.age~ Participant.age, 
                            data = DT.Agemix.men,method = "REML",
                            weights = varConstPower(form = ~Participant.age),
                            random = ~1|Uid)

summary(agemix.model.nlme.hetero1)

# test the significance of the heteroskedastic model. heteroskedastic model is much better 
# as shown by the significant decrease in AIC.
anova(agemix.model.nlme.hetero,agemix.model.nlme.hetero1)

# plot the standardized residuals shows a reasonably homogeneous pattern of the variability
# of the residuals

plot(agemix.model.nlme.hetero, resid(., type = "normalized") ~ Participant.age, abline = 0)
plot(agemix.model.nlme.hetero1, resid(., type = "normalized") ~ Participant.age, abline = 0)
# assess the variability of the variance parameter estimate
intervals(agemix.model.nlme.hetero1)


