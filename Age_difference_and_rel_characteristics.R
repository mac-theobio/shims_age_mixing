library(data.table)
library(tidyverse)
# library(dplyr)
# library(tidyr)
# library(tibble)
# library(ggplot2)
library(ordinal)    #for cumulative link mixed models
library(splines)    #or splines in models
library(survival)   #for cox ph model
library(effects)    #to do effects plots
library(cowplot)    #plot_grid
library(dotwhisker) #make dot whisker plots
#library(survminer)

setwd("/Users/user/Documents/shims_age_mixing")
source("Functions_for_SHIMS_study.R")
load("T1.agemix.Rdata")

# ===================
# Tidy the dataframe 
# ===================

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
                                      Ongoing.p1,
                                      Ongoing.p2,
                                      Ongoing.p3,
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
                          "^Ongoing", "^Money.gifts"), 
       value.name = c("Age.participant", "Age.difference",
                      "Condom.frequency", "Sex.frequency",
                      "Partner.type", "Relationship.dur",
                      "Ongoing.rel", "Money.gifts"),  
      variable.name = "Partner"
        )


# ===========================================================
# Removing respondents who did not report relationship 1,2,3
# ===========================================================

# subset the male data only

DT.reldata.men <- na.exclude(DT.reldata[which(Gender == "Male"),])

# ordering levels
freqlevels = c("always","sometimes","never")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("husband/wife","regular partner","casual partner")

DT.reldata.men <- DT.reldata.men %>% 
  transmute(Uid = as.factor(Uid),
            #Gender,
            No.partners,
            Partner,
            Age.participant,
            Age.difference,
            Relationship.dur,
            Ongoing.rel = as.factor(Ongoing.rel),
            Condom.frequency = ordered(Condom.frequency, levels = freqlevels),
            Sex.frequency = ordered(Sex.frequency, levels = sexlevels),
            Partner.type = ordered(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)
  )
  
# ===========================
# Fitting the models 
# ===========================
# Part I: CONDOM FREQUENCY
# (a) Effect of age difference on condom frequency with participant as random
# effect
# (b) Effect of age difference, age of participant and number of partners on 
# condom frequency

Condmod.1 <- clmm(Condom.frequency ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Condmod.1)

Condmod.2 <- clmm(Condom.frequency ~ ns(Age.difference,df = 4) + 
                    ns(Age.participant,df = 4) +
                    #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                    No.partners + 
                    (1|Uid),
                  data = DT.reldata.men,
                  Hess = TRUE,
                  nAGQ = 7)

summary(Condmod.2)

# Part II: SEX FREQUENCY
# (a) Effect of age difference on sex frequency with participant as random effect
# (b) Effect of age difference, age of participant and number of partners on sex 
# frequency with participant as random effect

Sexmod.1 <- clmm(Sex.frequency ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Sexmod.1)

Sexmod.2 <- clmm(Sex.frequency ~ ns(Age.difference,df = 4) + 
                   ns(Age.participant,df = 4) +
                   #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                   No.partners + 
                   (1|Uid),
                 data = DT.reldata.men,
                 Hess = TRUE,
                 nAGQ = 7)

summary(Sexmod.2)

# Part III: PARTNER TYPE
# (a) Effect of age difference on partner type with participant as random effect
# (b) Effect of age difference, age of participant and number of partners on partner type
# with participant as random effect

Partmod.1 <- clmm(Partner.type ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Partmod.1)

Partmod.2 <- clmm(Partner.type ~ ns(Age.difference,df = 4) + 
                    ns(Age.participant,df = 4) +
                    #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                    No.partners + 
                    (1|Uid),
                  data = DT.reldata.men,
                  Hess = TRUE,
                  nAGQ = 7)

summary(Partmod.2)

# Part IV: MONEY/GIFTS
# (a) Effect of age difference on money/gifts with participant as random effect
# (b) Effect of age difference, age of participant and number of partners on money/gifts
# with participant as random effect

Moneymod.1 <- clmm(Money.gifts ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Moneymod.1)

Moneymod.2 <- clmm(Money.gifts ~ ns(Age.difference,df = 4) + 
                     ns(Age.participant,df = 4) +
                     #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                     No.partners + 
                     (1|Uid),
                   data = DT.reldata.men,
                   Hess = TRUE,
                   nAGQ = 7)

summary(Moneymod.2)

# Part V: RELATIONSHIP DURATION
# (a) Effect of age difference on relationship duration with participant as random effect
# (b) Effect of age difference, age of participant and number of partners on relationship 
# duration with participant as random effect
#-fit the cox model
# censoring status, 1=censored, 0=relationship ended

Reldurmod.1 <- coxph(Surv(Relationship.dur, Ongoing.rel == 0) ~ 
                       ns(Age.difference,df = 4),
                     data = DT.reldata.men)

summary(Reldurmod.1)

Reldurmod.2 <- coxph(Surv(Relationship.dur, Ongoing.rel == 0) ~ 
                       ns(Age.difference,df = 4) + 
                       ns(Age.participant,df = 4) +
                       #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4)+ 
                       No.partners,
                     data = DT.reldata.men)

summary(Reldurmod.2)

# ==================================
# Tidying model outputs for plotting
# ==================================

# Part I: CONDOM FREQUENCY

# (a) for univariate model
tidycond.1 <- Effect("Age.difference", Condmod.1, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

# (b) for multivariate model

# (i) Age difference effect
tidycond.2a <- Effect("Age.difference", Condmod.2, 
                      xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value)
# (ii) Age of participant effect
tidycond.2b <- Effect("Age.participant", Condmod.2, 
                      xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

#(c) Predicted effects on condom frequency

#(i) for univariate
tidycond.3 <- OrdPred(Condmod.1,"Age.difference",DT.reldata.men)

#(ii) for multivarite
# Age difference
tidycond.3a <- OrdPred(Condmod.2,"Age.difference",DT.reldata.men)

# Age participant
tidycond.3b <- OrdPred(Condmod.2,"Age.participant",DT.reldata.men)

# Part II: SEX FREQUENCY

# (a) for univariate model
tidysf.1 <- Effect("Age.difference", Sexmod.1, 
                   xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

# (b) for multivariate model

# (i) Age difference effect

tidysf.2a <- Effect("Age.difference", Sexmod.2, 
                    xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

# (ii) Age of participant effect

tidysf.2b <- Effect("Age.participant", Sexmod.2, 
                    xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

#(c) Predicted effects on sex frequency

#(i) for univariate
tidysex.3 <-  OrdPred(Sexmod.1,"Age.difference",DT.reldata.men)

#(ii) for multivarite
# Age difference

tidysex.3a <-  OrdPred(Sexmod.2,"Age.difference",DT.reldata.men)

# Age of participant
tidysex.3b <-  OrdPred(Sexmod.2,"Age.participant",DT.reldata.men)

# Part III: PARTNER TYPE

# (a) for univariate model
tidypart.1 <- Effect("Age.difference", Partmod.1, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("husband.wife","regular.partner","casual.partner"))%>%
           plyr::mapvalues(from = c("husband.wife","regular.partner","casual.partner"),
                           to = partlevels)) %>% 
  spread(fit, value) 

# (b) for multivariate model

# (i) Age difference effect
tidypart.2a <- Effect("Age.difference", Partmod.2, 
                      xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("husband.wife","regular.partner","casual.partner"))%>%
           plyr::mapvalues(from = c("husband.wife","regular.partner","casual.partner"),
                           to = partlevels)) %>% 
  spread(fit, value) 

# (ii) Age of participant effect
tidypart.2b <- Effect("Age.participant", Partmod.2, 
                      xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("husband.wife","regular.partner","casual.partner"))%>%
           plyr::mapvalues(from = c("husband.wife","regular.partner","casual.partner"),
                           to = partlevels)) %>% 
  spread(fit, value) 

#(c) Predicted effects on partner type

#(i) for univariate

tidypart.3 <- OrdPred(Partmod.1,"Age.difference", DT.reldata.men)

#(ii) for multivarite
# Age difference

tidypart.3a <- OrdPred(Partmod.2,"Age.difference", DT.reldata.men)

# Age of participant
tidypart.3b <- OrdPred(Partmod.2,"Age.participant", DT.reldata.men)

# Part IV: MONEY/GIFTS

# (a) for univariate model
tidymon.1 <- Effect("Age.difference", Moneymod.1, 
                    xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

# (b) for multivariate model

# (i) Age difference effect
tidymon.2a <- Effect("Age.difference", Moneymod.2, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

# (ii) Age of participant effect
tidymon.2b <- Effect("Age.participant", Moneymod.2, 
                     xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

#(c) Predicted effects on money/gifts

#(i) for univariate

tidymon.3 <- OrdPred(Moneymod.1, "Age.difference", DT.reldata.men)

#(ii) for multivarite
# Age difference

tidymon.3a <- OrdPred(Moneymod.2, "Age.difference", DT.reldata.men)

# Age of participat
tidymon.3b <- OrdPred(Moneymod.2, "Age.participant", DT.reldata.men)

# Part V: RELATIONSHIP DURATION

# =========================================
# Effects plots, predictions from the models
# =========================================
theme_set(theme_bw())

# Part I: CONDOM FREQUENCY
# (a) Univariate

cond.1a <- tidycond.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

cond.pred.3 <- tidycond.3 %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Condom Use Score")

plot_grid(cond.1a, 
          cond.pred.3,
          labels = c("a","b"),
          ncol = 1)

# (b) Multivariate

cond.2a <- tidycond.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

cond.2b <- tidycond.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

cond.pred.3a <- tidycond.3a %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Condom Use Score")

cond.pred.3b <- tidycond.3b %>%
  ggplot(aes(x = Age.participant, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age of Participant") +
  ylab("Condom Use Score")

plot_grid(cond.2a,
          cond.2b,
          cond.pred.3a,
          cond.pred.3b,
          labels = c("a", "b", "c", "d"),
          ncol = 2)

# Part II: SEX FREQUENCY
# Effects plot for sex frequency model

# univariate
sex.1a <- tidysf.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2") 

sex.pred.3 <- tidysex.3 %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Sex Frequency Score")

plot_grid(sex.1a, 
          sex.pred.3,
          labels = c("a","b"),
          ncol = 1)
# multivariate

sex.2a <- tidysf.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2") 

sex.2b <- tidysf.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2")

sex.pred.3a <- tidysex.3a %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Sex Frequency Score")

sex.pred.3b <- tidysex.3b %>%
  ggplot(aes(x = Age.participant, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age Participant") +
  ylab("Sex Frequency Score")

plot_grid(sex.2a,
          sex.2b,
          sex.pred.3a,
          sex.pred.3b,
          labels = c("a", "b", "c", "d"),
          ncol = 2)

# Part III: PARTNER TYPE
# Effects plot for partner type models

#univariate
part.1a <- tidypart.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2") 

part.pred.3 <- tidypart.3 %>% 
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Partner Type Score")

plot_grid(part.1a, 
          part.pred.3,
          labels = c("a","b"),
          ncol = 1)

#multivariate
part.2a <- tidypart.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2") 

part.2b <- tidypart.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2")

part.pred.3a <- tidypart.3a %>% 
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Partner Type Score")

part.pred.3b <- tidypart.3b %>% 
  ggplot(aes(x = Age.participant, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age of Participant") +
  ylab("Partner Type Score")

plot_grid(part.2a,
          part.2b,
          part.pred.3a,
          part.pred.3b,
          labels = c("a", "b", "c", "d"),
          ncol = 2)

# Part IV: MONEY/GIFTS
# Effects plot for money gifts model

# univariate
mon.1a <- tidymon.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/Gifts", 
                    palette = "Dark2") 

mon.pred.3 <- tidymon.3 %>% 
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Money/Gifts Score")

plot_grid(mon.1a, 
          mon.pred.3,
          labels = c("a","b"),
          ncol = 1)

# multivariate
mon.2a <- tidymon.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/gifts", 
                    palette = "Dark2") 

mon.2b <- tidymon.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/gifts", 
                    palette = "Dark2")

mon.pred.3a <- tidymon.3a %>% 
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Money/Gifts Score")

mon.pred.3b <- tidymon.3b %>% 
  ggplot(aes(x = Age.participant, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age of Participant") +
  ylab("Money/Gifts Score")

plot_grid(mon.2a,
          mon.2b,
          mon.pred.3a,
          mon.pred.3b,
          labels = c("a", "b", "c", "d"),
          ncol = 2)

# Part V: RELATIONSHIP DURATION



#==========================================
# Tidy dataframes for the non spline terms
# turns out not to be informative since we only have one non spline term in our models.
#==========================================

tidycond2 <- TidyCLMM(Condmod.2) %>% 
  filter(!grepl('ns', term)) 

tidysex2 <- TidyCLMM(Sexmod.2) %>%  
  filter(!grepl('ns', term))

tidypart2 <- TidyCLMM(Partmod.2) %>%  
  filter(!grepl('ns', term))

tidymon2 <- TidyCLMM(Moneymod.2) %>%  
  filter(!grepl('ns', term))

tidydur2 <- TidyCLMM(Reldurmod.2) %>%  
  filter(!grepl('ns', term))
