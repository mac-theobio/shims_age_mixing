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
library(lattice) #xyplot
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

length(unique(DT.Agemix.men$Uid)) # to obtain the number of unique participants which will form the clusters

# =======================
# Random intercept model
# =======================

agemix.model.1 <- lmer(Partner.age ~  Participant.age + (1|Uid),
                       data = DT.Agemix.men)

summary(agemix.model.1)

ranef(agemix.model.1) # to obtain the actual bj for all the groups

fitted(agemix.model.1) # to obtain the fitted values
AIC(agemix.model.1)
# Parameter specific p-values
# to estimate the p-value for the slope when you have very large data set(thousands) we can obtain an upper bound
# for the degrees of freedom as number of observations M- number of fixed-effects parameters (2).

2 * (1-pt(abs(8.06), 240-2)) # very small p value. participant age is a significant predictor

















coef(agemix.model.1) # to obtain the coefficient for each participant


# to fit using Maximum likelihood use update
agemix.model.1ML <- update(agemix.model.1, REML = FALSE)
summary(agemix.model.1ML)

# assessing the variability of the parameters
pr01 <- profile(agemix.model.1ML)
xyplot(pr01, aspect = 1.3, layout = c(4,1)) #profile zeta plot
# The vertical lines in the panels delimit the 50%, 80%, 90%, 95% and 99%
# confidence intervals derived from the test statistic.
# To get the actual confidence intervals use confint()
confint(pr01) 

xyplot(pr01, aspect = 1.3, layout = c(4,1), absVal = TRUE) # plot of abs of zeta to visualize confidence intervals easily

splom(pr01)

# a convenient way to plot the random eﬀects with 95% conﬁdence intervals along with the estimated random eﬀects.
dotplot.ranef.mer(ranef(agemix.model.1, condVar=T),
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


#xyplot(Partner.age ~ Participant.age | Uid,data=DT.Agemix.men)

plot(agemix.model.1)
plot(agemix.model,Partner.age ~ fitted(.))

agemix.model.2 <- lmer(Partner.age ~ Participant.age + Partner.type + (1|Uid),
                     data = DT.Agemix.men)

summary(agemix.model.2)

agemix.model.2ML <- update(agemix.model.2, REML = FALSE)

anova(agemix.model.1ML, agemix.model.2ML)
# adding partner type does not improve the model
# random slope not possible because of very few observations per subject