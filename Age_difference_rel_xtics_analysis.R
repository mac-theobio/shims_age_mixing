# Libraries, data and functions -------------------------------------------

# Associations between age difference and relationship characteristics analysis

#library(data.table)
library(tidyverse)
library(ordinal)      #for cumulative link mixed models
library(splines)      #or splines in models
library(survival)     #for cox ph model
library(effects)      #to do effects plots
library(cowplot)      #plot_grid, get_legend
library(dotwhisker)   #make dot whisker plots
library(broom)        #convert objects into tidy data frame: tidy()
library(visreg)       #getting "contrast" hazard ratios
library(mgcv)         #for fitting GAMs
library(lubridate)
library(survminer)

## load data and functions

#load("~/Documents/shims_age_mixing/DT.Agemix.men.5.Rdata") # 5% random sample
load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata") # full dataset
theme_set(theme_bw()) # set global plot theme 
mycols <- c("dodgerblue", "orangered2", "#1B9E77", "#D80491","#480066")
# mycols <- c("#5C75F4", "orangered2", "#1B9E77", "#D80491","#480066")
mycols1 <- c("#D80491", "#1B9E77","orangered2","dodgerblue")
source("Functions_for_SHIMS_study.R")

summary(DT.Agemix.men)
filter(DT.Agemix.men, Age.difference >= 10)

t.test(DT.Agemix.men$Age.difference) #CI
# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("casual partner","regular partner","husband/wife")

# Condom Use Analysis -----------------------------------------------------

# ** Subset and Exploratory data analysis------------------------------------

DT.reldata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            No.partners,
            EnrollmentDate,
            Start.rel.date,
            Timedifference = interval(Start.rel.date,EnrollmentDate)  %/% months(1),
            Participant.age = Participant.age + 12,
            Age.difference,
            Condom.frequency = ordered(Condom.frequency, levels = freqlevels),
            Partner.type = factor(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Condom.frequency,No.partners)


summary(DT.reldata.men)

# men who reported 1,2,3 partner
sum(table(DT.reldata.men$Uid) == 1)
sum(table(DT.reldata.men$Uid) == 2)
sum(table(DT.reldata.men$Uid) == 3)

# remove high leverage/influential points (major = 3*IQR)
H = 3*IQR(DT.reldata.men$Age.difference)
U = quantile(DT.reldata.men$Age.difference, probs = 0.75) + H
L = quantile(DT.reldata.men$Age.difference, probs = 0.25) - H

DT.reldata.men <- filter(DT.reldata.men, Age.difference >= L & Age.difference <= U) 
  DT.reldata.men <- filter(DT.reldata.men, No.partners > 0)

(table(DT.reldata.men$Condom.frequency)/5094)*100

ggplot(data = DT.reldata.men, aes(Age.difference)) +
  geom_histogram(bins = 30, na.rm = T) +
  xlab("Age difference") +
  ylab("Relationships") +
  theme(axis.text.x = element_text(size=14), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=14)) +
  theme(text=element_text( size=14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5.5))

ggsave("Agediffhist.png", width = 6.25, height = 5.25,dpi = 600)

# frequncies of the condom use levels

# to contrast age difference across the 3 condom use levels we use the following boxplot. 
# The three condom use levels have a similar age difference
plot(Age.difference ~ Condom.frequency,
     data = DT.reldata.men)

# mean of age differences in the 3 categories
tapply(DT.reldata.men$Age.difference, DT.reldata.men$Condom.frequency, mean)

# condom use frequncies levels
table(DT.reldata.men$Condom.frequency)

ggplot(data = DT.reldata.men) +
  geom_bar(aes(Condom.frequency))

ggsave("Condomusefreq.png", width = 5.25, height = 4.35,dpi = 600)
# ** Step 1, ordinary partner level model ------------------------------------

# A cumulative logit model that includes the effect of age difference on condom use

condom.M0 <- clm(Condom.frequency ~ Age.difference,
                 data = DT.reldata.men)

summary(condom.M0)

exp(coef(condom.M0)[3])
# confidence intervals
confint(condom.M0, type = "Wald")
confint(condom.M0, type = "profile")

condom.Porl <- MASS::polr(Condom.frequency ~ ns(Age.difference,4),
                    data = DT.reldata.men)
summary(condom.Porl)
head(predict(condom.Porl, type = "p"),n=20)

# ** Step 2, participant level model -----------------------------------------
# cumulative logit random intercept model

condom.M1 <- clmm(Condom.frequency ~ Age.difference + (1|Uid),
                  data = DT.reldata.men,
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(condom.M1)

exp(coef(condom.M1)["Age.difference"])

# likelihood ratio test to determine whether the random effect is necessary
anova(condom.M0,condom.M1)

# significant difference. random intercept model is more appropriate

condom.M1$ranef

# ICC
icc = as.numeric(VarCorr(condom.M1)[1])/(as.numeric(VarCorr(condom.M1)[1]) + pi^2/3)
icc
# 69.3% of the unexplained variation is at the participant level

plot(Effect("Age.difference", condom.M1))

tidycond.0 <- data.frame(Effect("Age.difference", condom.M1,
                                xlevels = list(Age.difference = 50),
                                latent = T))  

ggplot(tidycond.0, aes(Age.difference, fit)) +
  geom_line(color = "dodgerblue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "dodgerblue", alpha = 0.25)

plot(Effect("Age.difference", condom.M1,
            xlevels = list(Age.difference = 50),
            latent = T))


# Extracting the effects generated using the effects function

tidycond.1 <- Effect("Age.difference", condom.M1, 
                     xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

cond.1a <- tidycond.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  #geom_area()+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

cond.1a
ggsave("Condomuse.png", width = 5.25, height = 4.35,dpi = 600)

ggplot(tidycond.1, aes(x = Age.difference, y=prob))+
  geom_line(size = 1, color = "dodgerblue")+
  facet_grid(cond~.)

ggsave("Condomusefacet.png", width = 5.25, height = 4.35,dpi = 600)

ggplot(tidycond.1, aes(x = Age.difference, y=prob, col = cond)) + geom_line(size = 1)
ggsave("Condomusetrial.png", width = 5.25, height = 4.35,dpi = 600)

# Predicted effects on ordinal condom frequency
# OrdPred is the function that transforms the logits (created by VarPred) to scores

#debugonce(OrdPred)
#debugonce(VarPred)
#debugonce(OrdTrans)

tidycond.1b <- OrdPred(condom.M1,"Age.difference",DT.reldata.men)

xx = VarPred(condom.M1,"Age.difference",DT.reldata.men)
xx1 = VarPred(condom.M1,"Age.difference",DT.reldata.men)

cond.pred <- tidycond.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

cond.pred
ggsave("Condompred.png", width = 5.25, height = 4.25,dpi = 600)

# ** Step 3, regression spline model--------------------------------------

# cumulative logit random intercept model

# degree of freedom = 5 from estimated EDF in GAM model
condom.M2 <- clmm(Condom.frequency ~ ns(Age.difference,df = 5) + (1|Uid),
                   data = DT.reldata.men,
                   #link = "logit", dont specify because effects dont work when specify
                   nAGQ = 7,
                   Hess = T) #if you need to call summary

summary(condom.M2)

plot(Effect("Age.difference", condom.M2, xlevels = list(Age.difference = 50)))
# Extracting the effects generated using the effects function

Effects.condom.M2 <- Effect("Age.difference", condom.M2, 
                     xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

Effects.condom.M2.plot <- Effects.condom.M2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  #geom_area()+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

Effects.condom.M2.plot + theme(legend.position = "none")
ggsave("Condomuse2.png", width = 5.25, height = 5.25,dpi = 600)

ggplot(Effects.condom.M2, aes(x = Age.difference, y=prob))+
  geom_line(size = 1, color = "dodgerblue")+
  geom_ribbon(aes(ymin = L, ymax = U),
              alpha = 0.25,
              fill = "dodgerblue") +
  facet_grid(cond ~.)

ggsave("CondomusefacetM2.png", width = 5.25, height = 4.35,dpi = 600)

Effects.condom.M2.line.plot <- ggplot(Effects.condom.M2, aes(x = Age.difference, y=prob, col = cond)) + 
                                      geom_line(size = 1.25) + 
                                      ylim(0.1,0.7) +
                                      xlab("Age difference") +
                                      ylab("Condom use (probability)") +
                                      theme(axis.text.x = element_text(size=19),
                                            axis.text.y = element_text(size=19),
                                            text=element_text(size=19),
                                            legend.title = element_blank(),
                                            legend.position = "bottom") +
                                      scale_color_manual(name = "Condom use", 
                                                        values = c(mycols))

leg <- get_legend(Effects.condom.M2.line.plot)
ggplotify::as.ggplot(leg)
ggsave("Condomuse3leg.png", width = 4.25, height = 0.25,dpi = 600)


Effects.condom.M2.line.plot + theme(legend.position = "none")
ggsave("CondomuseM2.png", width = 5.25, height = 4.25,dpi = 600)


# investigating age gap 0-15years
DT.reldata.men <- filter(DT.reldata.men, Age.difference >= 0 & Age.difference <= 15)
# rerun the model once again


# Predicted effects on condom frequency
Predictions.condom.M2 <- OrdPred(condom.M2,"Age.difference",DT.reldata.men)

#debugonce(VarPred)
#VarPred(condom.M2,"Age.difference",DT.reldata.men)

Predictions.condom.M2.plot <- Predictions.condom.M2 %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 

Predictions.condom.M2.plot
ggsave("Condompred2.png", width = 5.25, height = 4.25,dpi = 600)

# ** Step 4, adjusted regression spline model--------------------------------------

library(xtable)

DT.reldata.men.bi.multi.variate <- filter(DT.reldata.men, Age.difference > -6 & Age.difference < 16 ) %>% 
  mutate(Age.diff.category = cut(Age.difference, breaks = c(-6,6,16), labels = c("-5to5", "6to15"))) %>% 
  mutate(No.partners.category = car::recode(No.partners, "c('4','5','6','7','8','9','10','12','15','20') = '4+'"))

table(DT.reldata.men.bi.multi.variate$Condom.frequency, DT.reldata.men.bi.multi.variate$Age.diff.category)
table(DT.reldata.men.bi.multi.variate$Condom.frequency, DT.reldata.men.bi.multi.variate$No.partners.category)
xtable::xtable(table(DT.reldata.men.bi.multi.variate$Condom.frequency,DT.reldata.men.bi.multi.variate$No.partners.category))

ftable(table(DT.reldata.men.bi.multi.variate$Condom.frequency, DT.reldata.men.bi.multi.variate$No.partners.category,DT.reldata.men.bi.multi.variate$Age.diff.category), col.vars =2 )

condom.M3 <- clmm(Condom.frequency ~ ns(Age.difference,df = 4) + ns(Participant.age, df = 3) + No.partners + (1|Uid),
                  #random =  Uid,
                  data = DT.reldata.men,
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(condom.M3)

# (i) Age difference effect
Effects.condom.M3a <- Effect("Age.difference", condom.M3, 
                      xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value)

Effects.condom.M3a.plot <- Effects.condom.M3a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols), guide = guide_legend(reverse = T))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 


Effects.condom.M3a.plot + theme(legend.position = "none")
ggsave("Condomuse3a.png", width = 5.25, height = 4.25,dpi = 600)


ggplot(Effects.condom.M3a, aes(x = Age.difference, y=prob))+
  geom_line(size = 1, color = "dodgerblue")+
  geom_ribbon(aes(ymin = L, ymax = U),
              alpha = 0.25,
              fill = "dodgerblue") +
  facet_grid(cond ~.)

ggsave("CondomusefacetM3a.png", width = 5.25, height = 4.35,dpi = 600)

Effects.condom.M3a.line.plot <- ggplot(Effects.condom.M3a, aes(x = Age.difference, y=prob, col = cond)) + 
                                        geom_line(size = 1.25) + 
                                        xlab("Age difference") +
                                        ylab("Condom use (probability)") +
                                        theme(axis.text.x = element_text(size=19),
                                              axis.text.y = element_text(size=19),
                                              text=element_text(size=19),
                                              legend.title = element_blank(),
                                              legend.position = "bottom") +
                                        scale_color_manual(name = "Condom use", 
                                                           values = c(mycols))

Effects.condom.M3a.line.plot + theme(legend.position = "none")
ggsave("CondomuselineM3a.png", width = 5.25, height = 4.25,dpi = 600)



# Predicted effects and plot
Predictions.condom.M3a <- OrdPred(condom.M3,"Age.difference",DT.reldata.men)

Predictions.condom.M3a.plot <- Predictions.condom.M3a %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 

Predictions.condom.M3a.plot 
ggsave("Condompred3a.png", width = 5.25, height = 4.25,dpi = 600)

# (ii) Age of participant effect
Effects.condom.M3b <- Effect("Participant.age", condom.M3, 
                      xlevels = list(Participant.age = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Participant.age) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

Effects.condom.M3b.plot <- Effects.condom.M3b %>%
  ggplot(aes(x = Participant.age, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Participant Age") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 10)) 

Effects.condom.M3b.plot + theme(legend.position = "none")
ggsave("Condomuse3b.png", width = 5.25, height = 4.25,dpi = 600)



ggplot(Effects.condom.M3b, aes(x = Participant.age, y=prob))+
  geom_line(size = 1, color = "dodgerblue")+
  geom_ribbon(aes(ymin = L, ymax = U),
              alpha = 0.25,
              fill = "dodgerblue") +
  facet_grid(cond ~.)

ggsave("CondomusefacetM3b.png", width = 5.25, height = 5.35,dpi = 600)

Effects.condom.M3b.line.plot <- ggplot(Effects.condom.M3b, aes(x = Participant.age, y=prob, col = cond)) +
                                        geom_line(size = 1.25) + 
                                        xlab("Participant age") +
                                        ylab("Condom use (probability)") +
                                        theme(axis.text.x = element_text(size=19),
                                              axis.text.y = element_text(size=19),
                                              text=element_text(size=19),
                                              legend.title = element_blank(),
                                              legend.position = "bottom") +
                                        scale_color_manual(name = "Condom use", 
                                                           values = c(mycols)) +
                                        xlim(10,50)
  

Effects.condom.M3b.line.plot + theme(legend.position = "none")
ggsave("CondomuselineM3b.png", width = 5.25, height = 4.35,dpi = 600)


# Predicted effects and plot
Predictions.condom.M3b <- OrdPred(condom.M3,"Participant.age",DT.reldata.men)

Predictions.condom.M3b.plot <- Predictions.condom.M3b %>%
  ggplot(aes(x = Participant.age, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Participant age") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  xlim(10,50)
  #+scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

Predictions.condom.M3b.plot
ggsave("Condompred3b.png", width = 5.25, height = 4.25,dpi = 600)


##### number of partners

# (iii) 
Effects.condom.M3c <- Effect("No.partners", condom.M3, 
                             xlevels = list(No.partners = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - No.partners) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

Effects.condom.M3c.plot <- Effects.condom.M3c %>%
  ggplot(aes(x = No.partners, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Number of partners") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 10)) 

Effects.condom.M3c.plot + theme(legend.position = "none")
ggsave("Condomuse3c.png", width = 5.25, height = 4.25,dpi = 600)



Effects.condom.M3c.line.plot <- ggplot(Effects.condom.M3c, aes(x = No.partners, y=prob, col = cond))+
  geom_line(size = 1.25) + 
  xlab("Number of partners") +
  ylab("Condom use (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Condom use", 
                     values = c(mycols)) +
  xlim(0,20)

Effects.condom.M3c.line.plot + theme(legend.position = "none")
ggsave("CondomuselineM3c.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects and plot
Predictions.condom.M3c <- OrdPred(condom.M3,"No.partners",DT.reldata.men)

Predictions.condom.M3c.plot <- Predictions.condom.M3c %>%
  ggplot(aes(x = No.partners, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Number of partners") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  xlim(0,20)

Predictions.condom.M3c.plot
ggsave("Condompred3c.png", width = 5.25, height = 4.25,dpi = 600)

# # Partner type
# Effects.condom.M4a <- Effect("Partner.type", condom.M3) %>%
#   data.frame() %>%
#   select(-matches("logit."))
# Effects.condom.M4a

# ** Participant age on condom use Univariate ----------------------------------------------

# cumulative logit random intercept model

# degree of freedom = 3 from estimated EDF in GAM model
condom.M4 <- clmm(Condom.frequency ~ ns(Participant.age,df = 3) + (1|Uid),
                  data = DT.reldata.men,
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(condom.M4)

plot(Effect("Participant.age", condom.M4, xlevels = list(Participant.age = 50)))
# Extracting the effects generated using the effects function

Effects.condom.M4 <- Effect("Participant.age", condom.M4, 
                            xlevels = list(Participant.age = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Participant.age) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 

Effects.condom.M4.plot <- Effects.condom.M4 %>%
  ggplot(aes(x = Participant.age, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  #geom_area()+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Condom use", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) 

Effects.condom.M4.plot 
ggsave("Condomuse4.png", width = 5.25, height = 5.25,dpi = 600)

ggplot(Effects.condom.M4, aes(x = Participant.age, y=prob))+
  geom_line(size = 1, color = "dodgerblue")+
  geom_ribbon(aes(ymin = L, ymax = U),
              alpha = 0.25,
              fill = "dodgerblue") +
  facet_grid(cond ~.)

ggsave("CondomusefacetM4.png", width = 5.25, height = 4.35,dpi = 600)

ggplot(Effects.condom.M4, aes(x = Participant.age, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  xlab("Participant age") +
  ylab("Condom use (probability)") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        text=element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Condom use", 
                     values = c(mycols)) +
  xlim(10,50)

ggsave("CondomusetrialM4.png", width = 5.25, height = 4.35,dpi = 600)


# Sex Frequency Analysis -----------------------------------------------------
# ** Subset and Exploratory data analysis -----------------------------------
# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("husband/wife", "regular partner", "casual partner")

DT.sexdata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            EnrollmentDate,
            Start.rel.date,
            Timedifference = interval(Start.rel.date,EnrollmentDate)  %/% months(1),
            No.partners,
            Participant.age = Participant.age + 12,
            Age.difference,
            Sex.frequency = ordered(Sex.frequency, levels = sexlevels),
            Partner.type = factor(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Sex.frequency,No.partners)

summary(DT.sexdata.men)

# men who reported 1,2,3 partner
sum(table(DT.sexdata.men$Uid) == 1)
sum(table(DT.sexdata.men$Uid) == 2)
sum(table(DT.sexdata.men$Uid) == 3)

# remove high leverage/influential points (major = 3*IQR)
H = 3*IQR(DT.sexdata.men$Age.difference)
U = quantile(DT.sexdata.men$Age.difference, probs = 0.75) + H
L = quantile(DT.sexdata.men$Age.difference, probs = 0.25) - H

DT.sexdata.men <- filter(DT.sexdata.men, Age.difference >= L & Age.difference <= U)
DT.sexdata.men <- filter(DT.sexdata.men, No.partners >0)

# Remove all the relationships that were formed less than six months before survey date
DT.sexdata.men <- filter(DT.sexdata.men, Timedifference >= 6)

summary(DT.sexdata.men)
(table(DT.sexdata.men$Sex.frequency)/4017)*100
# the distribution of age difference by sex frequency
# to contrast age difference across the 3 condom use levels we use the following boxplot. 
# The three condom use levels have a similar age difference

plot(Age.difference ~ Sex.frequency,
     data = DT.sexdata.men)

# sex frequncies levels
ggplot(data = DT.sexdata.men) +
  geom_bar(aes(Sex.frequency))

ggsave("sexfreqdist.png", width = 5.25, height = 5.25,dpi = 600)

# mean of age differences in the 3 categories
tapply(DT.sexdata.men$Age.difference, DT.sexdata.men$Sex.frequency, mean)

DT.sexdata.men.bi.multi.variate <- filter(DT.sexdata.men, Age.difference > -6 & Age.difference < 16 ) %>% 
  mutate(Age.diff.category = cut(Age.difference, breaks = c(-6,6,16), labels = c("-5to5", "6to15"))) %>% 
  mutate(No.partners.category = car::recode(No.partners, "c('4','5','6','7','8','9','10','12','15','20') = '4+'"))

table(DT.sexdata.men.bi.multi.variate$Sex.frequency, DT.sexdata.men.bi.multi.variate$Age.diff.category)

table(DT.sexdata.men.bi.multi.variate$Sex.frequency,DT.sexdata.men.bi.multi.variate$No.partners.category)

ftable(table(DT.sexdata.men.bi.multi.variate$Sex.frequency, DT.sexdata.men.bi.multi.variate$No.partners.category,DT.sexdata.men.bi.multi.variate$Age.diff.category), row.vars = 1)


# ** Step 1, ordinary partner level model ----------------------------------------
# A cumulative logit model that includes the effect of age difference on condom use

sex.M0 <- clm(Sex.frequency ~ Age.difference,
                 data = DT.sexdata.men)

summary(sex.M0)


# ** Step 2, participant level model ---------------------------------------------
# cumulative logit random intercept model

sex.M1 <- clmm(Sex.frequency ~ Age.difference + (1|Uid),
                  data = DT.sexdata.men,
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(sex.M1)

exp(coef(sex.M1)["Age.difference"])

# likelihood ratio test to determine whether the random effect is necessary
anova(sex.M0,sex.M1)

# significant difference. random intercept model is more appropriate

sex.M1$ranef

# ICC
icc = as.numeric(VarCorr(sex.M1)[1])/(as.numeric(VarCorr(sex.M1)[1]) + pi^2/3)
icc
# 34.6% of the unexplained variation is at the participant level

plot(Effect("Age.difference", sex.M1))

tidysex.0 <- data.frame(Effect("Age.difference", sex.M1,
                                xlevels = list(Age.difference = 50),
                                latent = T))  

ggplot(tidysex.0, aes(Age.difference, fit)) +
  geom_line(color = "dodgerblue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "dodgerblue", alpha = 0.25)

png("sexfreq.png")

# Extracting the effects generated using the effects function

tidysex.1 <- Effect("Age.difference", sex.M1, 
                     xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

sex.1a <- tidysex.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

sex.1a
ggsave("Sexfrequncy.png", width = 5.25, height = 4.35,dpi = 600)

Effects.sex.M0.line.plot <- ggplot(tidysex.1, aes(x = Age.difference, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Age difference") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        text=element_text(size=15),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols))

Effects.sex.M0.line.plot + theme(legend.position = "none")
ggsave("SexfreqM0.png", width = 5.25, height = 4.25,dpi = 600)


# Predicted effects on ordinal sex frequency
# OrdPred is the function that transforms the logits (created by VarPred) to scores

debugonce(OrdPred)
debugonce(VarPred)
debugonce(OrdTrans)

tidysex.1b <- OrdPred(sex.M1,"Age.difference",DT.sexdata.men)

#VarPred(sex .M1,"Age.difference",DT.sexdata.men)

sex.pred <- tidysex.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Sex frequency score") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

sex.pred
ggsave("Sexpred.png", width = 5.25, height = 4.35,dpi = 600)

# ** Step 3, regression spline model --------------------------

sex.M2 <- clmm(Sex.frequency ~ ns(Age.difference,df = 4) + (1|Uid),
                  data = DT.sexdata.men,
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(sex.M2)

plot(Effect("Age.difference", sex.M2))
# Extracting the effects generated using the effects function

Effects.sex.M2 <- Effect("Age.difference", sex.M2, 
                     xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

Effects.sex.M2.plot <- Effects.sex.M2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  #geom_bar(stat = "identity")+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

Effects.sex.M2.plot + theme(legend.position = "none")
ggsave("Sexfrequency2.png", width = 5.25, height = 4.35,dpi = 600)


Effects.sex.M2.line.plot <- ggplot(Effects.sex.M2, aes(x = Age.difference, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Age difference") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols))

leg <- get_legend(Effects.sex.M2.line.plot)
ggplotify::as.ggplot(leg)
ggsave("Sexfreq3leg.png", width = 6.35, height = 0.25,dpi = 600)

Effects.sex.M2.line.plot + theme(legend.position = "none")
ggsave("SexfreqM2.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects on sex frequency
Predictions.sex.M2 <- OrdPred(sex.M2, "Age.difference",DT.sexdata.men)

#VarPred(sex.M2,"Age.difference",DT.sexdata.men)

Predictions.sex.M2.plot <- Predictions.sex.M2 %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Sex Frequency Score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 

Predictions.sex.M2.plot
ggsave("Sexpred2.png", width = 5.25, height = 4.35,dpi = 600)

predictions <- predict.clmm(sex.M2,newdata = DT.sexdata.men,dof = 6)
table(predictions$pred.cat)

# ** Step 4, adjusted regression spline model--------------------------------------

sex.M3 <- clmm(Sex.frequency ~ ns(Age.difference,df = 2) + ns(Participant.age, df = 3)+ No.partners  + (1|Uid) ,
               data = DT.sexdata.men,
               nAGQ = 7,
               Hess = T) #if you need to call summary

summary(sex.M3)

# (i) Age difference effect

Effects.sex.M3a <- Effect("Age.difference", sex.M3, 
                         xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

Effects.sex.M3a.plot <- Effects.sex.M3a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  #geom_bar(stat = "identity")+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

# legsex <- get_legend(Effects.sex.M3a.plot)
# ggplotify::as.ggplot(legsex)
# ggsave("Sexfreq3leg.png", width = 1.35, height = 1.25,dpi = 600)

Effects.sex.M3a.plot + theme(legend.position = "none")
ggsave("Sexfrequency3a.png", width = 5.25, height = 4.35,dpi = 600)

Effects.sex.M3.line.plot <- ggplot(Effects.sex.M3a, aes(x = Age.difference, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Age difference") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols))

Effects.sex.M3.line.plot + theme(legend.position = "none")
ggsave("SexfreqM3.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects on sex frequency

Predictions.sex.M3a <- OrdPred(sex.M3, "Age.difference",DT.sexdata.men)

Predictions.sex.M3a.plot <- Predictions.sex.M3a %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Sex Frequency Score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 

Predictions.sex.M3a.plot
ggsave("Sexpred3a.png", width = 5.25, height = 4.35,dpi = 600)

# (ii) Participant age effect

Effects.sex.M3b <- Effect("Participant.age", sex.M3, 
                          xlevels = list(Participant.age = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Participant.age) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

Effects.sex.M3b.plot <- Effects.sex.M3b %>%
  ggplot(aes(x = Participant.age, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Participant age") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 


Effects.sex.M3b.plot + theme(legend.position = "none")
#ggsave("Sexfrequency3b.png", width = 5.25, height = 4.35,dpi = 600)

Effects.sex.M3b.line.plot <- ggplot(Effects.sex.M3b, aes(x = Participant.age, y=prob, col = cond)) +
  geom_line(size = 1.25) + 
  xlab("Participant age") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols)) +
  xlim(10,50)


Effects.sex.M3b.line.plot + theme(legend.position = "none")
ggsave("SexfreqlineM3b.png", width = 5.25, height = 4.35,dpi = 600)


# Participant age effect on sex frequency
Predictions.sex.M3b <- OrdPred(sex.M3, "Participant.age",DT.sexdata.men)

Predictions.sex.M3b.plot <- Predictions.sex.M3b %>%
  ggplot(aes(x = Participant.age, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Participant age") +
  ylab("Sex Frequency Score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  xlim(10,50)
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

Predictions.sex.M3b.plot
ggsave("Sexpred3b.png", width = 5.25, height = 4.35,dpi = 600)

# (iii) Number of partners effect

Effects.sex.M3c <- Effect("No.partners", sex.M3, 
                          xlevels = list(No.partners = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - No.partners) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

Effects.sex.M3c.plot <- Effects.sex.M3c %>%
  ggplot(aes(x = No.partners, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Number of partners") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) 


Effects.sex.M3c.plot + theme(legend.position = "none")
ggsave("Sexfrequency3c.png", width = 5.25, height = 4.35,dpi = 600)

Effects.sex.M3c.line.plot <- ggplot(Effects.sex.M3c, aes(x = No.partners, y=prob, col = cond)) +
  geom_line(size = 1.25) + 
  xlab("Number of partners") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols)) +
  xlim(0,20)


Effects.sex.M3c.line.plot + theme(legend.position = "none")
ggsave("SexfreqlineM3c.png", width = 5.25, height = 4.35,dpi = 600)

Predictions.sex.M3c <- OrdPred(sex.M3, "No.partners",DT.sexdata.men)

Predictions.sex.M3c.plot <- Predictions.sex.M3c %>%
  ggplot(aes(x = No.partners, y = fit)) +
  geom_line(size = 1.5, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Number of partners") +
  ylab("Sex Frequency Score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  xlim(0,20)

Predictions.sex.M3c.plot

ggsave("Sexpred3c.png", width = 5.25, height = 4.35,dpi = 600)


# Effects.sex.M.Partner <- Effect("Partner.type", sex.M3) %>% 
#   data.frame()
# Effects.sex.M.Partner
# ** Participant age on Sex frequency Univariate ----------------------------------------------
sex.M4 <- clmm(Sex.frequency ~ ns(Participant.age,df = 3) + (1|Uid),
               data = DT.sexdata.men,
               nAGQ = 7,
               Hess = T) #if you need to call summary

summary(sex.M4)



Effects.sex.M4 <- Effect("Participant.age", sex.M4, 
                          xlevels = list(Participant.age = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Participant.age) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("X", "", cond)) %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("1","between.2.5","between.6.10","more.than.10"))%>%
           plyr::mapvalues(from = c("1","between.2.5","between.6.10","more.than.10"),
                           to = sexlevels)) %>% 
  spread(fit, value) 

Effects.sex.M4.plot <- Effects.sex.M4 %>%
  ggplot(aes(x = Participant.age, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Participant age") +
  ylab("Probability") +
  scale_fill_manual(name = "Sex frequency", 
                    values = c(mycols1))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme(text=element_text( size=12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 


Effects.sex.M4.plot + theme(legend.position = "none")
ggsave("Sexfrequency4.png", width = 5.25, height = 4.35,dpi = 600)

Effects.sex.M4.line.plot <- ggplot(Effects.sex.M4, aes(x = Participant.age, y=prob, col = cond)) +
  geom_line(size = 1.25) + 
  xlab("Participant age") +
  ylab("Sex frequency (probability)") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        text=element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Sex frequency", 
                     values = c(mycols)) +
  xlim(10,50)


Effects.sex.M4.line.plot + theme(legend.position = "none")
ggsave("SexfreqlineM4.png", width = 5.25, height = 4.35,dpi = 600)


# Predicted effects on sex frequency
Predictions.sex.M4b <- OrdPred(sex.M4, "Participant.age",DT.sexdata.men)

Predictions.sex.M4b.plot <- Predictions.sex.M4b %>%
  ggplot(aes(x = Participant.age, y = fit)) +
  geom_line(size = 1.5, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Participant age") +
  ylab("Sex Frequency Score") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) +
  xlim(15,50)

Predictions.sex.M4b.plot
ggsave("Sexpred4b.png", width = 5.25, height = 4.35,dpi = 600)

# Partner Type Analysis -----------------------------------------------------
# ** Subset and Exploratory data analysis -----------------------------------

# ordering levels
freqlevels = c("never","sometimes","always")
partlevels = c("casual partner","regular partner","husband/wife")


DT.partnerdata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            No.partners,
            Participant.age = Participant.age + 12,
            Age.difference,
            Partner.type = ordered(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Partner.type,No.partners)

summary(DT.partnerdata.men)

# men who reported 1,2,3 partner
sum(table(DT.partnerdata.men$Uid) == 1)
sum(table(DT.partnerdata.men$Uid) == 2)
sum(table(DT.partnerdata.men$Uid) == 3)

# remove high leverage/influential points (major = 3*IQR)
H = 3*IQR(DT.partnerdata.men$Age.difference)
U = quantile(DT.partnerdata.men$Age.difference, probs = 0.75) + H
L = quantile(DT.partnerdata.men$Age.difference, probs = 0.25) - H

DT.partnerdata.men <- filter(DT.partnerdata.men, Age.difference >= L & Age.difference <= U)
DT.partnerdata.men <- filter(DT.partnerdata.men, No.partners > 0)

(table(DT.partnerdata.men$Partner.type)/5132)*100

# to contrast age difference across the 3 partner type levels we use the following boxplot. 
plot(Age.difference ~ Partner.type,
     data = DT.partnerdata.men)

# partner frequncies levels
ggplot(data = DT.partnerdata.men) +
  geom_bar(aes(Partner.type))

DT.partnerdata.men.bi.multi.variate <- filter(DT.partnerdata.men, Age.difference > -6 & Age.difference < 16 ) %>% 
  mutate(Age.diff.category = cut(Age.difference, breaks = c(-6,6,16), labels = c("-5to5", "6to15"))) %>% 
  mutate(No.partners.category = car::recode(No.partners, "c('4','5','6','7','8','9','10','12','15','20') = '4+'"))

table(DT.partnerdata.men.bi.multi.variate$Partner.type, DT.partnerdata.men.bi.multi.variate$Age.diff.category)

table(DT.partnerdata.men.bi.multi.variate$Partner.type,DT.partnerdata.men.bi.multi.variate$No.partners.category)

ftable(table(DT.partnerdata.men.bi.multi.variate$Partner.type, DT.partnerdata.men.bi.multi.variate$No.partners.category,DT.partnerdata.men.bi.multi.variate$Age.diff.category), row.vars = 1)

# ** Step 1, ordinary partner level model ----------------------------------------
# A cumulative logit model that includes the effect of age difference on condom use

partner.M0 <- clm(Partner.type ~ Age.difference,
              data = DT.partnerdata.men)

summary(partner.M0)


# ** Step 2, participant level model ---------------------------------------------
# cumulative logit random intercept model

partner.M1 <- clmm(Partner.type ~ Age.difference + (1|Uid),
               data = DT.partnerdata.men,
               #link = "logit", dont specify because effects dont work when specify
               nAGQ = 7,
               Hess = T) #if you need to call summary

summary(partner.M1)

exp(coef(partner.M1)["Age.difference"])

# likelihood ratio test to determine whether the random effect is necessary
anova(partner.M0,partner.M1)

# significant difference. random intercept model is more appropriate

partner.M1$ranef

as.numeric(VarCorr(partner.M1)[1])
# ICC
icc = as.numeric(VarCorr(partner.M1)[1])/(as.numeric(VarCorr(partner.M1)[1]) + pi^2/3)
icc
# 49.3% of the unexplained variation is at the participant level

plot(Effect("Age.difference", partner.M1))

tidypart.0 <- data.frame(Effect("Age.difference", partner.M1,
                               xlevels = list(Age.difference = 50),
                               latent = T))  

ggplot(tidypart.0, aes(Age.difference, fit)) +
  geom_line(color = "dodgerblue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "dodgerblue", alpha = 0.25)

# Extracting the effects generated using the effects function

tidypart.1 <- Effect("Age.difference", partner.M1, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("casual.partner","regular.partner","husband.wife"))%>%
           plyr::mapvalues(from = c("casual.partner","regular.partner","husband.wife"),
                           to = c("casual","regular","spouse"))) %>% 
  spread(fit, value) 

partner.1a <- tidypart.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Partner type", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

partner.1a
ggsave("Partnertype.png", width = 5.25, height = 4.35,dpi = 600)

# Predicted effects on partner type
tidypart.1b <- OrdPred(partner.M1,"Age.difference",DT.partnerdata.men)

#VarPred(partner.M1,"Age.difference",DT.partnerdata.men)

partner.pred <- tidypart.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

partner.pred

ggsave("Partnerpred.png", width = 5.25, height = 4.35,dpi = 600)
# ** Step 3, regression spline model --------------------------

# cumulative logit random intercept model

start_time <- Sys.time()

partner.M2 <- clmm(Partner.type ~ ns(Age.difference,df = 5) + (1|Uid),
               #random =  Uid,
               data = DT.partnerdata.men,
               #link = "logit", dont specify because effects dont work when specify
               nAGQ = 7,
               Hess = T) #if you need to call summary

summary(partner.M2)

end_time <- Sys.time()
end_time - start_time

plot(Effect("Age.difference", partner.M2))
# Extracting the effects generated using the effects function

tidypart.2 <- Effect("Age.difference", partner.M2, 
                     xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("casual.partner","regular.partner","husband.wife"))%>%
           plyr::mapvalues(from = c("casual.partner","regular.partner","husband.wife"),
                           to = c("casual","regular","spouse"))) %>% 
  spread(fit, value) 

part.2a <- tidypart.2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  #geom_area(position = position_stack(reverse = T)) +
  geom_bar(stat = "identity", position = position_stack(reverse = T))+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Partner type", 
                    values = c(mycols), guide = guide_legend(reverse = F))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

legpart <- get_legend(part.2a)
ggplotify::as.ggplot(legpart)
ggsave("Partnertype3leg.png", width = 4.35, height = 0.25,dpi = 600)

part.2a + theme(legend.position = "none")
ggsave("Partnertype2.png", width = 5.25, height = 4.35,dpi = 600)

Effects.partner.M2.line.plot <- ggplot(tidypart.2, aes(x = Age.difference, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Age difference") +
  ylab("Partner type (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Partner type", 
                     values = c(mycols))

legpart <- get_legend(Effects.partner.M2.line.plot)
ggplotify::as.ggplot(legpart)
ggsave("Partnertype2.png", width = 4.25, height = 0.25,dpi = 600)


Effects.partner.M2.line.plot + theme(legend.position = "none")
ggsave("PartnertypeM2.png", width = 5.25, height = 4.25,dpi = 600)



# Predicted effects on partner type
tidypart.2b <- OrdPred(partner.M2, "Age.difference",DT.partnerdata.men)

#VarPred(partner.M2,"Age.difference",DT.partnerdata.men)

part.pred2 <- tidypart.2b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 

part.pred2
ggsave("Partnerpred2.png", width = 5.25, height = 4.35,dpi = 600)


# ** Step 4, adjusted regression spline model--------------------------------------

partner.M3 <- clmm(Partner.type ~ ns(Age.difference,df = 3) + ns(Participant.age, df = 4) + No.partners + (1|Uid),
                  #random =  Uid,
                  data = DT.partnerdata.men,
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(partner.M3)

# (i) Age difference effect
Effects.partner.M3a <- Effect("Age.difference", partner.M3, 
                              xlevels = list(Age.difference = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Age.difference) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("casual.partner","regular.partner","husband.wife"))%>%
           plyr::mapvalues(from = c("casual.partner","regular.partner","husband.wife"),
                           to = c("casual","regular","spouse"))) %>% 
  spread(fit, value) 

Effects.partner.M3a.plot <- Effects.partner.M3a %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_manual(name = "Partner type", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) 

Effects.partner.M3a.plot + theme(legend.position = "none")
ggsave("Partnertype3a.png", width = 5.25, height = 4.25,dpi = 600)

Effects.partner.M3a.line.plot <- ggplot(Effects.partner.M3a, aes(x = Age.difference, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Age difference") +
  ylab("Partner type (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Partner type", 
                     values = c(mycols))

Effects.partner.M3a.line.plot + theme(legend.position = "none")
ggsave("PartnertypeM3a.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects and plot

Predictions.partner.M3a <- OrdPred(partner.M3,"Age.difference",DT.partnerdata.men)

Predictions.partner.M3a.plot <- Predictions.partner.M3a %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Age difference") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) 


Predictions.partner.M3a.plot 
ggsave("Partnerpred3a.png", width = 5.25, height = 4.25,dpi = 600)


# (ii) Age of participant effect
Effects.partner.M3b <- Effect("Participant.age", partner.M3, 
                              xlevels = list(Participant.age = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - Participant.age) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("casual.partner","regular.partner","husband.wife"))%>%
           plyr::mapvalues(from = c("casual.partner","regular.partner","husband.wife"),
                           to = c("casual","regular","spouse"))) %>% 
  spread(fit, value) 

Effects.partner.M3b.plot <- Effects.partner.M3b %>%
  ggplot(aes(x = Participant.age, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Participant Age") +
  ylab("Probability") +
  scale_fill_manual(name = "Partner type", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 10)) 

Effects.partner.M3b.plot + theme(legend.position = "none")
ggsave("Partnertype3b.png", width = 5.25, height = 4.25,dpi = 600)

Effects.partner.M3b.line.plot <- ggplot(Effects.partner.M3b, aes(x = Participant.age, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Participant age") +
  ylab("Partner type (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Partner type", 
                     values = c(mycols))+
  xlim(10,50)

Effects.partner.M3b.line.plot + theme(legend.position = "none")
ggsave("PartnertypeM3b.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects and plot
Predictions.partner.M3b <- OrdPred(partner.M3,"Participant.age",DT.partnerdata.men)

Predictions.partner.M3b.plot <- Predictions.partner.M3b %>%
  ggplot(aes(x = Participant.age, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Participant age") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=19), 
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlim(10,50)

Predictions.partner.M3b.plot
ggsave("Partnerpred3b.png", width = 5.25, height = 4.25,dpi = 600)


# (iii) Number of partners effect
Effects.partner.M3c <- Effect("No.partners", partner.M3, 
                             xlevels = list(No.partners = 50)) %>%
  data.frame() %>%
  select(-matches("logit.")) %>%
  gather(var, value, - No.partners) %>%
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = c("casual.partner","regular.partner","husband.wife"))%>%
           plyr::mapvalues(from = c("casual.partner","regular.partner","husband.wife"),
                           to = c("casual","regular","spouse"))) %>% 
  spread(fit, value) 

Effects.partner.M3c.plot <- Effects.partner.M3c %>%
  ggplot(aes(x = No.partners, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  xlab("Number of partners") +
  ylab("Partner type (probability)") +
  scale_fill_manual(name = "Partner type", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) +
  theme(text=element_text( size=15)) +
  scale_x_continuous( breaks = scales::pretty_breaks(n = 10)) 

Effects.partner.M3c.plot + theme(legend.position = "none")
ggsave("Partnertype3c.png", width = 5.25, height = 4.25,dpi = 600)

Effects.partner.M3c.line.plot <- ggplot(Effects.partner.M3c, aes(x = No.partners, y=prob, col = cond)) + 
  geom_line(size = 1.25) + 
  #ylim(0.1,0.7) +
  xlab("Number of partners") +
  ylab("Partner type (probability)") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19),
        text=element_text(size=19),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(name = "Partner type", 
                     values = c(mycols))

Effects.partner.M3c.line.plot + theme(legend.position = "none")
ggsave("PartnertypeM3c.png", width = 5.25, height = 4.25,dpi = 600)

# Predicted effects and plot
Predictions.partner.M3c <- OrdPred(partner.M3,"No.partners",DT.partnerdata.men)

Predictions.partner.M3c.plot <- Predictions.partner.M3c %>%
  ggplot(aes(x = No.partners, y = fit)) +
  geom_line(size = 1.25, color = "dodgerblue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "dodgerblue") +
  xlab("Number of partners") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19)) +
  theme(text=element_text( size=19)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

Predictions.partner.M3c.plot
ggsave("Partnerpred3c.png", width = 5.25, height = 4.25,dpi = 600)

# ** Participant age on Partner type Univariate ----------------------------------------------

# Relationship duration analysis ------------------------------------------

# ** Subset and Exploratory data analysis-------------------------------------

DT.coxdata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            EnrollmentDate,
            Start.rel.date,
            End.rel.date,
            Timedifference = interval(Start.rel.date,EnrollmentDate)  %/% months(1),
            No.partners,
            Partner.type = factor(Partner.type, levels = partlevels),
            Participant.age = Participant.age + 12,
            Age.difference = round(Age.difference,6),
            Relationship.dur,
            Rel.ongoing,
            Rel.dissolved = ifelse(Rel.ongoing == T , 0, 1)) %>% 
  drop_na(Age.difference,Relationship.dur,No.partners)

summary(DT.coxdata.men)

DT.coxdata.men <- filter(DT.coxdata.men, Relationship.dur >= 0) # remove some relationships with negative relationship duration

# men who reported 1,2,3 partner
sum(table(DT.coxdata.men$Uid) == 1)
sum(table(DT.coxdata.men$Uid) == 2)
sum(table(DT.coxdata.men$Uid) == 3)

# remove high leverage/influential points (major = 3*IQR)
H = 3*IQR(DT.coxdata.men$Age.difference)
U = quantile(DT.coxdata.men$Age.difference, probs = 0.75) + H
L = quantile(DT.coxdata.men$Age.difference, probs = 0.25) - H

DT.coxdata.men <- filter(DT.coxdata.men, Age.difference >= L & Age.difference <= U)


# We found out that some relationships the end date was beyond the enrollment date which should not be the case and so we remove these relationships

DT.coxdata.men <- filter(DT.coxdata.men, zoo::as.yearmon(End.rel.date,"%b%y") <= zoo::as.yearmon(EnrollmentDate))

# Create a survival object

DT.coxdata.men$SurvObj <- with(DT.coxdata.men, Surv(Relationship.dur, Rel.dissolved))


# Distribution of survival times
ggplot(data = DT.coxdata.men, aes(Relationship.dur)) +
  geom_histogram(bins = 30, na.rm = T)


surv <- Surv(DT.coxdata.men$Relationship.dur, DT.coxdata.men$Rel.dissolved)
ggsurvevents(surv)

# Kaplan-Meier estimator
# log-log confidence interval is preferred

KM.estimator.1 <- survfit(SurvObj ~ 1, data = DT.coxdata.men, conf.type = "log-log")
KM.estimator.1
summary(KM.estimator.1)
ggsurvplot(KM.estimator.1, size = 1.25, palette = "dodgerblue", ggtheme = theme_bw(), xlab = "Time (months)",
           legend = "none", font.y = 14, font.x = 14, font.tickslab = 14)

ggsave("Kaplanmeier.png", width = 5.25, height = 4.35,dpi = 600)

KM.estimator.2 <- survfit(SurvObj ~ Age.difference, data = DT.coxdata.men, conf.type = "log-log")
KM.estimator.2 

KM.estimator.3 <- survfit(SurvObj ~ Partner.type, data = DT.coxdata.men, conf.type = "log-log")
KM.estimator.3
summary(KM.estimator.3)
plot(KM.estimator.3)
ggsurvplot(KM.estimator.3, size = 1.25, palette = c("dodgerblue","orangered","#1B9E77"),
           ggtheme = theme_bw(), pval = T, xlab = "Time (months)",
           legend.labs = c("Casual", "Regular", "Spouse"), 
           legend = "bottom", conf.int = T, legend.title = "Partner type",
           font.y = 14, font.x = 14, font.tickslab = 14)


ggsave("Kaplanmeier3.png", width = 5.25, height = 4.35,dpi = 600)


plot(KM.estimator.3, fun = function(s) -log(-log(s)))
# how the estimated survival depends on partner type
# are there differences in survival among casual, regular, spousal partnerships

# ### Relationship duration old way
# DT.coxdata.men <- DT.coxdata.men %>% mutate(Rel.dur = as.numeric(difftime(DateCleaning(End.rel.date), Start.rel.date, units = "weeks")))

# creating the survival object
# Rel.dissoved is an event indicator, equal to 1 if the relationship ended before survey day and 0 for those that were still ongoing and hence censored
# fit Cox PH model of time to relationship dissolution on time constant covariate
Reldurmod.M1 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference,
                      data = DT.coxdata.men)

summary(Reldurmod.M1)

# test proportional hazard assumption
# schoenfeld residuals

residuals(Reldurmod.M1, type = "schoenfeld")
plot(residuals(Reldurmod.M1, type = "schoenfeld"))

# # scaled schoenfeld residuals
# res.zph <- cox.zph(Reldurmod.M1)
# res.zph
# plot(res.zph) 
# abline(h=0,lty=3)
# # p value greater than 0.05 indicating no violation

# an additional year in age differences increases the monthly hazard of relationship dissolution by a factor of 1.014041 i.e 1.4% ,95% CI for the HR: (0.9992,1.029) hence age difference is not significant (the hazard ratio/estimated relative risk for a yearly increase in age difference is  1.014041).


Reldurmod.M <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference + Partner.type,
                      data = DT.coxdata.men)

summary(Reldurmod.M)
res.zph.2 <- cox.zph(Reldurmod.M)
plot(res.zph.2)

# Partnership type is a confounder with 3 levels we need to stratify
# we have different baseline hazards for each stratum

Reldurmod.strata.M <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference + strata(Partner.type),
                     data = DT.coxdata.men)

summary(Reldurmod.strata.M)

res.zph.3 <- cox.zph(Reldurmod.strata.M)
plot(res.zph.3)
abline(h=0,lty=3)


ggsurvplot(survfit(Reldurmod.strata.M), data = DT.coxdata.men)
# interaction model
Reldurmod.strata.M2 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference * strata(Partner.type),
                            data = DT.coxdata.men)

summary(Reldurmod.strata.M2)

# likelihood ratio test

anova(Reldurmod.strata.M, Reldurmod.strata.M2)
# clustering model; gives you robust variance estimators when you have clusters. It does not change the point estimates though.

Reldurmod.cluster.M1 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ ns(Age.difference, df = 4) + cluster(Uid)
                              + strata(Partner.type), 
                              data = DT.coxdata.men)

summary(Reldurmod.cluster.M1)
AIC(Reldurmod.cluster.M1)

# testing proportional hazards assumption

residuals(Reldurmod.cluster.M1, type = "schoenfeld")
plot(residuals(Reldurmod.cluster.M1, type = "schoenfeld"))

# scaled schoenfeld residuals
res.zph.2 <- cox.zph(Reldurmod.cluster.M1)
res.zph.2
plot(res.zph.2)
abline(h=0,lty=3)

Reldurmod.cluster.M1.interaction <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ ns(Age.difference, df = 4) *strata(Partner.type)  + cluster(Uid), 
                                          data = DT.coxdata.men)

Reldurmod.cluster.M1.interaction
AIC(Reldurmod.cluster.M1.interaction)
# how does change in age difference relative to mean age difference affect the hazard?

tidyreldur.1 <- visreg(Reldurmod.cluster.M1,
                       xvar = "Age.difference",
                       ylab = "Hazard ratio",
                       type = "contrast", # only option for coxph models
                       trans = exp,
                       plot = F) %$%
  data.frame(fit) %>%
  mutate(hr = visregFit,
         lwr = visregLwr,
         upr = visregUpr)

rel.pred.1 <- tidyreldur.1 %>%
  ggplot(aes(x = Age.difference, y = hr)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25, 
              fill = "dodgerblue") +
  geom_line(size = 1.25, 
            color = "dodgerblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Age difference") +
  ylab("Hazard Ratio") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))+
  theme(text=element_text(size=19)) 

rel.pred.1

ggsave("relcox1.png", width = 5.25, height = 4.35,dpi = 600)
# The hazard of ending a rationship was higher for participants who were 4.5 years older or more than their partners relative to the participants who had mean age difference.



# Examinig the distribution of survival times
# compute the predicted survivor function for cox ph model

percentiles <- round(quantile(DT.coxdata.men$Age.difference,prob = c(.025, .25, .5, .75, .975), type = 3),6)

#percentiles <- round(quantile(DT.coxdata.men$Age.difference,prob = c(.5,0.75), type = 3),6)
percentiles


new.df <- with(DT.coxdata.men,
               data.frame(Age.difference = percentiles))

fit <- survfit(Reldurmod.cluster.M1, newdata = new.df, data = DT.coxdata.men) 
plot(fit, ylim =c(0,1), xlab = "Months", ylab = "Survival probability", main = "Survival Curves")

Predicted.survivor <- survfit(Reldurmod.cluster.M1, newdata = new.df, data = DT.coxdata.men) %>%
  tidy() %>% 
  select(time, starts_with("estimate"), strata) %>% 
  gather(var, value, -time, -strata)


reldur.1 <- Predicted.survivor %>% 
  ggplot(aes(x = time, 
             y = value)) +
  geom_line(aes(color = strata, linetype = var), size = 1.25) + xlim(0,400)+
  scale_color_manual(name = "Partner type", values = c("dodgerblue","orangered","#1B9E77"),
                     labels = c("Casual","Spouse","Regular")) +
  scale_linetype_manual(name = "Age difference",
                        values = c("solid","dashed","twodash","longdash", "dotted"),
                        labels = c("4yrs younger", 
                                    "1yr older", 
                                    "4yrs older",
                                    "7yrs older",
                                    "17yrs older"))+
  xlab("Relationship duration (months)") +
  ylab("Probability of survival") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))+
  theme(text=element_text(size=19))
reldur.1

cox.leg.M1 <- get_legend(reldur.1)
ggplotify::as.ggplot(cox.leg.M1)
ggsave("coxlegM1.png", width = 1.39, height = 4.35,dpi = 600)


reldur.1 + theme(legend.position = "none")
ggsave("survivalcurvesM1.png", width = 5.25, height = 4.35,dpi = 600)


# Alternatively
reldur.1 <- Predicted.survivor %>% 
  ggplot(aes(x = time, 
             y = value)) +
  geom_line(aes(linetype = strata, color = var), size = 1.25)+
  scale_linetype_manual(name = "Partner type", values = c("solid","twodash","dotted"),
                        labels = c("Casual","Spouse","Regular"))+
  scale_color_manual(name = "Age difference",
                     values = mycols,
                     labels = c("4yrs younger", 
                                "1yr older", 
                                "4yrs older",
                                "7yrs older",
                                "17yrs older"))
reldur.1
# to use ggcoxadjustedcurves, you need to create a new data frame


# survival curves from a cox ph model

# Roxy's approach of selecting to dispalay survival curves of 5 age differences

Exp.survival.M1 <- survexp(~ Age.difference, 
                        data = DT.coxdata.men, 
                        ratetable = Reldurmod.cluster.M1) %>%
  tidy() %>%
  select(-matches("n.risk")) %>%
  gather(var, value, -time) %>%
  mutate(var = gsub("surv.Age.difference\\.", "", var),
         Age.difference = gsub("^\\D", "-", var) %>%
           as.numeric()) %>%
  select(-var) %>%
  filter(Age.difference %in% percentiles) %>%
  mutate(Age.difference = factor(Age.difference, 
                                 levels = percentiles,
                                 labels = c("4yrs younger", 
                                            "1yr older", 
                                            "4yrs older",
                                            "7yrs older",
                                            "17yrs older")))

reldur.1 <- Exp.survival.M1 %>%
  ggplot(aes(x = time, 
             y = value)) +
  geom_line(aes(color = Age.difference), size = 1) +
  geom_hline(yintercept = 0.5, colour = "black", linetype = 2) + 
  xlab("Relationship duration (months)") +
  ylab("Probability of survival") +
  scale_color_manual(name = "Age difference", 
                    values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))+
  theme(text=element_text(size=15))

cox.leg.M1 <- get_legend(reldur.1)
ggplotify::as.ggplot(cox.leg.M1)
ggsave("coxlegM1.png", width = 1.39, height = 1.5,dpi = 600)


reldur.1 + theme(legend.position = "none")
ggsave("survivalcurvesM1.png", width = 5.25, height = 4.35,dpi = 600)

# Examinig the distribution of survival times (adjusted survival curve-adjusted for age difference)
plot(survfit(Reldurmod.cluster.M1), ylim =c(0,1), xlab = "Months", ylab = "Survival probability", main = "Survival Curve")

Reldurmod.cluster.M2 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ ns(Age.difference,4) + ns(Participant.age, df=3) + No.partners +  cluster(Uid) + strata(Partner.type), 
                      data = DT.coxdata.men)

summary(Reldurmod.cluster.M2)


tidyreldur.2 <- visreg(Reldurmod.cluster.M2,
                       xvar = "Age.difference",
                       ylab = "Hazard ratio",
                       type = "contrast", # only option for coxph models
                       trans = exp,
                       plot = F) %$%
  data.frame(fit) %>%
  mutate(hr = visregFit,
         lwr = visregLwr,
         upr = visregUpr)

rel.pred.2 <- tidyreldur.2 %>%
  ggplot(aes(x = Age.difference, y = hr)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25, 
              fill = "dodgerblue") +
  geom_line(size = 1.25, 
            color = "dodgerblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Age difference") +
  ylab("Hazard Ratio") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))+
  theme(text=element_text(size=19)) 

rel.pred.2

ggsave("relcox2.png", width = 5.25, height = 4.35,dpi = 600)



new.df.2 <- with(DT.coxdata.men,
               data.frame(Age.difference = percentiles,
                          Participant.age = rep(mean(Participant.age,5)),
                          No.partners = rep(mean(No.partners,5))))


Predicted.survivor.2 <- survfit(Reldurmod.cluster.M2, newdata = new.df.2, data = DT.coxdata.men) %>%
  tidy() %>% 
  select(time, starts_with("estimate"), strata) %>% 
  gather(var, value, -time, -strata)


reldur.2 <- Predicted.survivor.2 %>% 
  ggplot(aes(x = time, 
             y = value)) +
  geom_line(aes(color = strata, linetype = var), size = 1.25) + xlim(0,400)+
  scale_color_manual(name = "Partner type", values = c("dodgerblue","orangered","#1B9E77"),
                     labels = c("Casual","Spouse","Regular")) +
  scale_linetype_manual(name = "Age difference",
                        values = c("solid","dashed","twodash","longdash", "dotted"),
                        labels = c("4yrs younger", 
                                   "1yr older", 
                                   "4yrs older",
                                   "7yrs older",
                                   "17yrs older"))+
  xlab("Relationship duration (months)") +
  ylab("Probability of survival") +
  theme(axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))+
  theme(text=element_text(size=19))
reldur.2

reldur.2 + theme(legend.position = "none")
ggsave("survivalcurvesM2.png", width = 5.25, height = 4.35,dpi = 600)




fit2 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.diff.cat + Participant.age + No.partners + cluster(Uid), 
             data = DT.coxdata.men)
fit2

new.df2 <- with(DT.coxdata.men, data.frame(Age.diff.cat = c("younger","non-AD","intra-GAD", "inter-GAD"), Participant.age = rep(mean(Participant.age),4), No.partners = rep(mean(No.partners),4)))

xx2 <- ggsurvplot(survfit(fit2, newdata = new.df2),data = new.df, pval = F, legend = "none", legend.title = "Age Gaps", legend.labs = c("younger", "non-AD", "intra-GAD", "inter-GAD"), ggtheme = theme_bw(), censor = F, conf.int = F, palette = mycols, font.y = 15, font.x=15, font.tickslab = 15)
xx2

ggsave("survivalcat2.png", width = 5.25, height = 4.35,dpi = 600)

# approach 2
Exp.survival.M3 <- survexp(~ Age.difference, 
                           data = DT.coxdata.men, 
                           ratetable = Reldurmod.cluster.M2) %>%
  tidy() %>%
  select(-matches("n.risk")) %>%
  gather(var, value, -time) %>%
  mutate(var = gsub("surv.Age.difference\\.", "", var),
         Age.difference = gsub("^\\D", "-", var) %>%
           as.numeric()) %>%
  select(-var) %>%
  filter(Age.difference %in% percentiles) %>%
  mutate(Age.difference = factor(Age.difference, 
                                 levels = percentiles,
                                 labels = c("4yrs younger", 
                                            "1yr older", 
                                            "4yrs older",
                                            "7yrs older",
                                            "17yrs older")))

reldur.2 <- Exp.survival.M3 %>%
  ggplot(aes(x = time, 
             y = value)) +
  geom_line(aes(color = Age.difference), size = 1) +
  geom_hline(yintercept = 0.5, colour = "black", linetype = 2) + 
  xlab("Relationship duration (months)") +
  ylab("Probability of survival") +
  scale_color_manual(name = "Age difference", 
                     values = c(mycols))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))+
  theme(text=element_text(size=15))

reldur.2 + theme(legend.position = "none")
ggsave("survivalcurvesM2.png", width = 5.25, height = 4.35,dpi = 600)



# For relationships that started 6 months before survey date

# Remove all the relationships that were formed less than six months before survey date
DT.coxdata.men.new <- filter(DT.coxdata.men, Timedifference >= 6)

Reldurmod.cluster.M4 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ ns(Age.difference, df = 4) + cluster(Uid), 
                              data = DT.coxdata.men.new)

summary(Reldurmod.cluster.M4)

tidyreldur.4 <- visreg(Reldurmod.cluster.M4,
                       xvar = "Age.difference",
                       ylab = "Hazard ratio",
                       type = "contrast", # only option for coxph models
                       trans = exp,
                       plot = F) %$%
  data.frame(fit) %>%
  mutate(hr = visregFit,
         lwr = visregLwr,
         upr = visregUpr)

rel.pred.4 <- tidyreldur.4 %>%
  ggplot(aes(x = Age.difference, y = hr)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25, 
              fill = "dodgerblue") +
  geom_line(size = 1, 
            color = "dodgerblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Age difference") +
  ylab("Hazard Ratio") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))+
  theme(text=element_text(size=15)) 

rel.pred.4

ggsave("relcox4.png", width = 5.25, height = 4.35,dpi = 600)

Reldurmod.cluster.M5 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ ns(Age.difference,2) + Participant.age + ns(No.partners,1) +  cluster(Uid), 
                              data = DT.coxdata.men.new)

summary(Reldurmod.cluster.M5)

tidyreldur.5 <- visreg(Reldurmod.cluster.M5,
                       xvar = "Age.difference",
                       ylab = "Hazard ratio",
                       type = "contrast", # only option for coxph models
                       trans = exp,
                       plot = F) %$%
  data.frame(fit) %>%
  mutate(hr = visregFit,
         lwr = visregLwr,
         upr = visregUpr)

rel.pred.5 <- tidyreldur.5 %>%
  ggplot(aes(x = Age.difference, y = hr)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25, 
              fill = "dodgerblue") +
  geom_line(size = 1, 
            color = "dodgerblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Age difference") +
  ylab("Hazard Ratio") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))+
  theme(text=element_text(size=15)) 

rel.pred.5

ggsave("relcox5.png", width = 5.25, height = 4.35,dpi = 600)
# Extras ------------------------------------------------------------------



# ** Implementing GAMMs for comparision: condom use model -----------------------------------

# Response should be integer class labels
DT.reldata.men.gamm <- DT.reldata.men
levels(DT.reldata.men.gamm$Condom.frequency) <- c(1,2,3)
DT.reldata.men.gamm$Condom.frequency <- as.numeric(DT.reldata.men.gamm$Condom.frequency)


# with random effects(basic model) 
# Random intercepts are coded by including a smooth over the grouping variable with the
# smoothing class specified as bs="re".
start_time <- Sys.time()
gam.Condom <- bam(Condom.frequency ~ s(Age.difference, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                  data = DT.reldata.men.gamm,
                  family = ocat(R = 3),
                  method = "fREML", #fREML is much faster and yields similar results like RELM
                  nthreads = 4,
                  discrete = T)

end_time <- Sys.time()
end_time - start_time

gam.Condom
save(gam.Condom, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Condom.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Condom.Rdata")
summary(gam.Condom)
coef(gam.Condom) ## estimated coefficients, edf=5

gam.check(gam.Condom)

plot(gam.Condom)
plot(gam.Condom, residuals = T)

predict.gam(gam.Condom, type = "response")

# adjusting for age and number of partners
start_time <- Sys.time()
gam.Condom.adj <- bam(Condom.frequency ~ s(Age.difference, bs="cr", k = 10) + s(Participant.age, bs="cr", k = 10)+ s(No.partners, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                      data = DT.reldata.men.gamm,
                      family = ocat(R = 3),
                      method = "fREML", #fREML is much faster and yields similar results like RELM
                      nthreads = 4,
                      discrete = T)

end_time <- Sys.time()
end_time - start_time
gam.Condom.adj
save(gam.Condom.adj, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Condom.adj.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Condom.adj.Rdata")

# ** Implementing GAMMs for comparision: Sex frequency model -----------------------------------

# Response should be integer class labels
DT.sexdata.men.gamm <- DT.sexdata.men
levels(DT.sexdata.men.gamm$Sex.frequency) <- c(1,2,3,4)
DT.sexdata.men.gamm$Sex.frequency <- as.numeric(DT.sexdata.men.gamm$Sex.frequency)


# with random effects(basic model) 
# Random intercepts are coded by including a smooth over the grouping variable with the
# smoothing class specified as bs="re".
start_time <- Sys.time()
gam.Sex <- bam(Sex.frequency ~ s(Age.difference, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
               data = DT.sexdata.men.gamm,
               family = ocat(R = 4),
               method = "fREML", #fREML is much faster and yields similar results like RELM
               nthreads = 2,
               discrete = T)

end_time <- Sys.time()
end_time - start_time

gam.Sex
save(gam.Sex, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Sex.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Sex.Rdata")

# adjusting for number of partners
start_time <- Sys.time()
gam.Sex.adj <- bam(Sex.frequency ~ s(Age.difference, bs="cr", k = 10) + s(Participant.age, bs="cr", k = 10) + s(No.partners, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                   data = DT.sexdata.men.gamm,
                   family = ocat(R = 4),
                   method = "fREML", #fREML is much faster and yields similar results like RELM
                   nthreads = 2,
                   discrete = T)

end_time <- Sys.time()
end_time - start_time
gam.Sex.adj
save(gam.Sex.adj, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Sex.adj.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Sex.adj.Rdata")

# ** Implementing GAMMs for comparision: partner type model -----------------------------------
# Generalized additive mixed models

# Response should be integer class labels
DT.partnerdata.men.gamm <- DT.partnerdata.men 
levels(DT.partnerdata.men.gamm$Partner.type) <- c(1,2,3)
DT.partnerdata.men.gamm$Partner.type <- as.numeric(DT.partnerdata.men.gamm$Partner.type)


# with random effects(basic model) 
# Random intercepts are coded by including a smooth over the grouping variable with the
# smoothing class specified as bs="re".
start_time <- Sys.time()
gam.Partner <- bam(Partner.type ~ s(Age.difference, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                   data = DT.partnerdata.men.gamm,
                   family = ocat(R = 3),
                   method = "fREML", #fREML is much faster and yields similar results like RELM
                   nthreads = 4,
                   discrete = T)

end_time <- Sys.time()
end_time - start_time

gam.Partner
save(gam.Partner, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Partner.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Partner.Rdata")

# adjusting for number of partners
start_time <- Sys.time()
gam.Partner.adj <- bam(Partner.type ~ s(Age.difference, bs="cr", k = 10) + s(Participant.age, bs="cr", k = 10) + s(No.partners, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                       data = DT.partnerdata.men.gamm,
                       family = ocat(R = 3),
                       method = "fREML", #fREML is much faster and yields similar results like RELM
                       nthreads = 4,
                       discrete = T)

end_time <- Sys.time()
end_time - start_time
gam.Partner.adj
save(gam.Partner.adj, file ="/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Partner.adj.Rdata")
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/gam.Partner.adj.Rdata")

# ** Implementing GAMMs for comparision: Relationship duration model -----------------------------------

gam.cox <- gam(Relationship.dur ~ s(Age.difference, bs="cr", k = 10) + s(Uid, bs="re"), 
               family = cox.ph(),
               data = DT.coxdata.men,
               weights = Rel.dissolved,
               nthreads = 4)
summary(gam.cox)

gam.cox.adj <- gam(Relationship.dur ~ s(Age.difference, bs="cr", k = 10) + s(Participant.age, bs="cr", k = 10) + s(No.partners, bs="cr", k = 10),
               family = cox.ph(),
               data = DT.coxdata.men,
               weights = Rel.dissolved,
               nthreads = 4)
summary(gam.cox.adj)


# cox mixed effects model
library(coxme)
Reldurmod.coxme.M1 <- coxme(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference + (1|Uid), 
                            data = DT.coxdata.men)

summary(Reldurmod.coxme.M1)

# frailty model
Reldurmod.frailty.M1 <- coxph(Surv(Relationship.dur, Rel.dissolved) ~ Age.difference + frailty(Uid), 
                              data = DT.coxdata.men)

summary(Reldurmod.frailty.M1)

# computing the expected survival
data(pbc)
pbc

pbcfit <- coxph(Surv(time, status==2) ~ age + log(bili) + log(protime) + log (albumin) + edema, pbc)
pbcfit

# Create a data set corresponding to the hypothetical subject
temp <- data.frame(age=53, edema= 0, bili=2, protime=12 , albumin=2)

# Obtain and plot the expected curve of the hypothetical person

sfit <- survfit(pbcfit, newdata=temp)
plot(sfit, xlab="Years",ylab="Expected Survival")

xfit <- coxph(Surv(time, status==2)~age + log(bili) + strata(edema), data=pbc)
xfit
temp <- data.frame(age=c(53, 60), bili=c(2, 3))
curves <- survfit(xfit, newdata=temp)
print(curves)
plot(curves[3,])

library(survminer) # good cox plots
plot(survfit(Reldurmod.M1))
ggsurvplot(survfit(Reldurmod.M1), data = DT.coxdata.men, ggtheme = theme_bw(), palette="dodgerblue" )

new.agedf <- data.frame(Age.difference = c(-10,0,20))
plot(survfit(Reldurmod.M1,newdata = new.agedf))
ggsurvplot(survfit(Reldurmod.M1,newdata = new.agedf),data = new.agedf, ggtheme = theme_bw(), palette="dodgerblue" )