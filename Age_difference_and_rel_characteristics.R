library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ordinal)    #for cumulative link mixed models
library(splines)    #or splines in models
library(survival)   #for cox ph model
library(effects)    #to do effects plots
library(cowplot)    #plot_grid
#library(survminer)

setwd("/home/emanuel/Documents/SHIMS/shims_age_mixing")
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
# Fitting the models: part 1  
# ===========================

# (a) Effect of age difference on condom frequency with participant as random
# effect
# df <- model.frame(Condom.frequency ~ Age.difference + Uid,
#                   data = DT.reldata.men,
#                   na.action = na.exclude, 
#                   drop.unused.levels = TRUE)

Condmod.1 <- clmm(Condom.frequency ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Condmod.1)



# (b) Effect of age difference on sex frequency with participant as random effect

Sexmod.1 <- clmm(Sex.frequency ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Sexmod.1)

# (c) Effect of age difference on partner type with participant as random effect

Partmod.1 <- clmm(Partner.type ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Partmod.1)

# (d) Effect of age difference on money gifts with participant as random effect

Moneymod.1 <- clmm(Money.gifts ~ ns(Age.difference,df = 4) + (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Moneymod.1)


# (e) Effect of age difference on relationship duration

#-fit the cox model
# censoring status, 1=censored, 0=relationship ended
# DT.reldata.clean$Surv.obj <- with(DT.reldata.clean,
#                                   Surv(Relationship.dur, Ongoing.rel == 0))

Reldurmod.1 <- coxph(Surv(Relationship.dur, Ongoing.rel == 0) ~ 
                       ns(Age.difference,df = 4),
                     data = DT.reldata.men)

summary(Reldurmod.1)

# ==========================
# Fitting the models: part 2 
# ==========================

# (a) Effect of age difference on condom frequency with participant as random 
# effect
# df <- model.frame(Condom.frequency ~ 
#                     Age.difference + Age.participant + No.partners + Uid,
#                   data = DT.reldata.men,
#                   na.action = na.exclude, 
#                   drop.unused.levels = TRUE)


Condmod.2 <- clmm(Condom.frequency ~ ns(Age.difference,df = 4) + 
                    ns(Age.participant,df = 4) +
                    #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                    No.partners + 
                    (1|Uid),
                data = DT.reldata.men,
                Hess = TRUE,
                nAGQ = 7)

summary(Condmod.2)



# (b) Effect of age difference on sex frequency with participant as random effect

Sexmod.2 <- clmm(Sex.frequency ~ ns(Age.difference,df = 4) + 
                   ns(Age.participant,df = 4) +
                   #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                   No.partners + 
                   (1|Uid),
                 data = DT.reldata.men,
                 Hess = TRUE,
                 nAGQ = 7)

summary(Sexmod.2)

# (c) Effect of age difference on partner type with participant as random effect

Partmod.2 <- clmm(Partner.type ~ ns(Age.difference,df = 4) + 
                    ns(Age.participant,df = 4) +
                    #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                    No.partners + 
                    (1|Uid),
                  data = DT.reldata.men,
                  Hess = TRUE,
                  nAGQ = 7)

summary(Partmod.2)

# (d) Effect of age difference on money gifts with participant as random effect

Moneymod.2 <- clmm(Money.gifts ~ ns(Age.difference,df = 4) + 
                     ns(Age.participant,df = 4) +
                     #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4) + 
                     No.partners + 
                     (1|Uid),
                   data = DT.reldata.men,
                   Hess = TRUE,
                   nAGQ = 7)

summary(Moneymod.2)


# (e) Effect of age difference on relationship duration

  #-fit the cox model
  # censoring status, 1=censored, 0=relationship ended
# DT.reldata.clean$Surv.obj <- with(DT.reldata.clean,
#                                   Surv(Relationship.dur, Ongoing.rel == 0))

Reldurmod.2 <- coxph(Surv(Relationship.dur, Ongoing.rel == 0) ~ 
                       ns(Age.difference,df = 4) + 
                       ns(Age.participant,df = 4) +
                       #ns(Age.difference,df = 4) * ns(Age.participant, ds = 4)+ 
                       No.partners,
                 data = DT.reldata.men)

summary(Reldurmod.2)

# ===================================
# Effects plots for univariate models
# ===================================

# Effects plot for condom model
theme_set(theme_bw())

tidycond.1 <- Effect("Age.difference", Condmod.1, 
                    xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

cond.1a <- tidycond.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

# Effects plot for sex frequency model

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

sex.1a <- tidysf.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2") 


# Effects plot for age difference only models

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

part.1a <- tidypart.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2") 


# Effects plot for money gifts model
tidymon.1 <- Effect("Age.difference", Moneymod.1, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

mon.1a <- tidymon.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/Gifts", 
                    palette = "Dark2") 

plot_grid(cond.1a,
          sex.1a,
          part.1a,
          mon.1a,
          labels = c("a", "b", "c", "d"),
          ncol = 2)

# =====================================
# Effects plots for multivariate models
# =====================================

# Effects plot for condom model
theme_set(theme_bw())

tidycond.2a <- Effect("Age.difference", Condmod.2, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

cond.2a <- tidycond.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

tidycond.2b <- Effect("Age.participant", Condmod.2, 
                      xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

cond.2b <- tidycond.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")

plot_grid(cond.2a,cond.2b)

# sex frequency multivariate model
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

sex.2a <- tidysf.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2") 

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

sex.2b <- tidysf.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex Frequency", 
                    palette = "Dark2")

plot_grid(sex.2a,sex.2b)

# partner type multivariate model

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

part.2a <- tidypart.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2") 

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

part.2b <- tidypart.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2")

plot_grid(part.2a,part.2b)

# money gifts effects in multivariate model
tidymon.2a <- Effect("Age.difference", Moneymod.2, 
                    xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

mon.2a <- tidymon.2a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/gifts", 
                    palette = "Dark2") 


tidymon.2b <- Effect("Age.participant", Moneymod.2, 
                      xlevels = list(Age.participant = 40)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.participant) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

mon.2b <- tidymon.2b %>%
  ggplot(aes(x = Age.participant, y = prob, fill = cond)) +
  geom_area() +
  xlab("Age") +
  ylab("Probability") +
  scale_fill_brewer(name = "Money/gifts", 
                    palette = "Dark2")

plot_grid(mon.2a,mon.2b)
