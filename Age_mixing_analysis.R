library(data.table)
library(ggplot2)
library(dplyr)
library(nlme)

setwd("/home/emanuel/Desktop/SHIMS/shims_age_mixing")
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
# Subtract 15 from all respondent ages, so that “respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- na.exclude(DT.Agemix[which(Gender == "Male" & Participant.age >= 15),])
DT.Agemix.men$Participant.age <- DT.Agemix.men$Participant.age - 15

model <- lme(Partner.age~Participant.age,
             data = DT.Agemix.men,
             method = "REML",
             weights = varPower(value = 0.5, form = ~Participant.age + 1),
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

#####################################################################################################
# visualizing output

fixParam <- fixef(model)
ranParam <- ranef(model)
params <- ranParam[1]+fixParam[1]

p <- ggplot(DT.Agemix.men,aes(Participant.age,Partner.age)) +
      geom_point(size=3,color="black") +
      xlab("") +
      ylab("") +
      scale_x_continuous(labels = function(x)x+15)

subNum <- unique(DT.Agemix.men$Uid)

for(i in 1:length(subNum)){
  p <- p + geom_abline(intercept = params[i,1],slope = fixParam[2],color ="lightskyblue2")
}

p <- p + geom_abline(intercept = fixParam[1],slope = fixParam[2],color = "blue",size=1.5)
print(p)



##### confidence intervals for an individual
age <- seq(15,50,0.1)
within.sd <- sqrt(within.var * (1+(age-15))^power)


expected.partner.age.person.i <- intercept + age * fixParam[2]

UL95pred.interval <- expected.partner.age.person.i + 1.96 * within.sd
LL95pred.interval <- expected.partner.age.person.i - 1.96 * within.sd










