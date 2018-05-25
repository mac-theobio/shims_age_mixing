
# Associations between age difference and relationship characteristics analysis


# load libraries

#library(data.table)
library(tidyverse)
library(ordinal)    #for cumulative link mixed models
library(splines)    #or splines in models
library(survival)   #for cox ph model
library(effects)    #to do effects plots
library(cowplot)    #plot_grid
library(dotwhisker) #make dot whisker plots
library(broom)      #convert objects into tidy data frame: tidy()
library(visreg)     #getting "contrast" hazard ratios
# library(egg)        #devtools::install_github("baptiste/egg") #for plot management
# library(strcode)    #devtools::install_github("lorenzwalthert/strcode") #for code structuring with sub/headings

## load data and functions

#load("~/Documents/shims_age_mixing/DT.Agemix.men.5.Rdata") # 5% random sample
load("/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.men.Rdata") # full dataset
theme_set(theme_bw()) # set global plot theme 
source("Functions_for_SHIMS_study.R")


# Condom Use Analysis -----------------------------------------------------

# ** Subset and Exploratory data analysis-------------------------------------


# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("casual partner","regular partner","husband/wife")


DT.reldata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            No.partners,
            Age.difference,
            Condom.frequency = ordered(Condom.frequency, levels = freqlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Condom.frequency)

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

# frequncies of the condom use levels

# to contrast age difference across the 3 condom use levels we use the following boxplot. 
# The three condom use levels have a similar age difference
plot(Age.difference ~ Condom.frequency,
     data = DT.reldata.men)

# sex frequncies levels
ggplot(data = DT.reldata.men) +
  geom_bar(aes(Condom.frequency))

ggsave("Condomusefreq.png", width = 6.25, height = 5.25,dpi = 600)
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
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(condom.M1)

exp(coef(condom.M1)["Age.difference"])
# condom.M1 <- clmm2(Condom.frequency ~ Age.difference ,
#                    random = Uid,
#                    data = DT.reldata.men,
#                    #link = "logit",
#                    nAGQ = 7,
#                    Hess = T) #if you need to call summary
# 
# summary(condom.M1)

# likelihood ratio test to determine whether the random effect is necessary
anova(condom.M0,condom.M1)

# significant difference. random intercept model is more appropriate

condom.M1$ranef

# ICC
icc = as.numeric(VarCorr(condom.M1)[1])/(as.numeric(VarCorr(condom.M1)[1]) + pi^2/3)
icc
# 69.3% of the unexplained variation is at the participant level
# 
# X <- Effect("Age.difference", condom.M1)
# XX <- data.frame(Effect("Age.difference", condom.M1))

plot(Effect("Age.difference", condom.M1))
# estimated probabilities of never using condom, using condom sometimes and alwyas using a condom
# for the proportional odds model with 95% pointwise confidence envelopes around the fitted probabilites
# based on standard errors computed using the delta method.

# # using ggplot
# tidycond <- Effect("Age.difference", condom.M1, 
#                      xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
#   data.frame()
# 
# ggplot(tidycond, aes(Age.difference, prob.never)) +
#   geom_line(color = "#009E73", size = 1.2) +
#   geom_ribbon(aes(ymin = L.prob.never, ymax = U.prob.never), fill = "#009E73", alpha = 0.25)
# # same can be done for sometimes and always

# we can plot the fitted value on the scale of the latent continous condom use score. We have one line
# for all the 3 categories of condom use.
# the estimated threshold are shown by dotted lines, dividing the condom frequency into 3 categories.

tidycond.0 <- data.frame(Effect("Age.difference", condom.M1,
                                xlevels = list(Age.difference = 50),
                                latent = T))  

ggplot(tidycond.0, aes(Age.difference, fit)) +
  geom_line(color = "#009E73", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#009E73", alpha = 0.25)

# plot(Effect("Age.difference", condom.M1,
#             xlevels = list(Age.difference = 50),
#             latent = T))
# 
# dev.off()

# the dotted line s-a indcates the boundary between "sometimes" and "always" while 
# n-s indicates the boundary between "never" and "sometimes"
# most of the people with age difference between -10 and 18 years would be expected to answer 
# "sometimes" when it comes to condom use although the confidence intervals are
# large on the extreme age differences indicating great uncertainity


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
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

cond.1a
ggsave("Condomuse.png", width = 6.25, height = 5.25,dpi = 600)

# The estimated probability of never using a condom increases as the age difference between the man
# and female partner increases.
# The estimated probability of always using a condom decreases as the age difference between the man
# and female partner increases.
# The estimated probability of using condom sometimes remains relatively constant across different age differences


# Predicted effects on condom frequency
tidycond.1b <- OrdPred(condom.M1,"Age.difference",DT.reldata.men)

#VarPred(condom.M1,"Age.difference",DT.reldata.men)

cond.pred <- tidycond.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

cond.pred
ggsave("Condompred.png", width = 6.25, height = 5.25,dpi = 600)

# ** Step 3, regression spline model--------------------------------------

# cumulative logit random intercept model

start_time <- Sys.time()

condom.M2 <- clmm(Condom.frequency ~ ns(Age.difference,df = 2) + (1|Uid),
                   #random =  Uid,
                   data = DT.reldata.men,
                   #link = "logit", dont specify because effects dont work when specify
                   nAGQ = 7,
                   Hess = T) #if you need to call summary

summary(condom.M2)

end_time <- Sys.time()
end_time - start_time

plot(Effect("Age.difference", condom.M2))
# Extracting the effects generated using the effects function

tidycond.2 <- Effect("Age.difference", condom.M2, 
                     xlevels = list(Age.difference = 50)) %>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(-matches("logit.")) %>% #removes logits
  gather(var, value, - Age.difference) %>% # long format
  separate(var, c("fit", "cond"), extra = "merge") %>%
  mutate(cond = gsub("prob.", "", cond) %>%
           ordered(levels = freqlevels))%>%
  spread(fit, value) 


cond.2a <- tidycond.2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = T)) +
  #geom_area()+
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Condom use", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

cond.2a
ggsave("Condomuse2.png", width = 6.25, height = 5.25,dpi = 600)

# Predicted effects on condom frequency
tidycond.2b <- OrdPred(condom.M2,"Age.difference",DT.reldata.men)

#VarPred(condom.M2,"Age.difference",DT.reldata.men)

cond.pred2 <- tidycond.2b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

cond.pred2
ggsave("Condompred2.png", width = 6.25, height = 5.25,dpi = 600)

predictions <- predict.clmm(condom.M2,newdata = DT.reldata.men,dof = 4)

table(predictions$pred.cat)

# predictions using effect package
tidycond.3 <- Effect("Age.difference", condom.M2, 
                     xlevels = list(Age.difference = DT.reldata.men$Age.difference))%>% #default 5 values evaluated but now we want 50
  data.frame() %>%
  select(starts_with("prob")) %>% #removes logits
  mutate(pred.cat = colnames(.)[apply(., 1, which.max)]) %>% 
  mutate(pred.cat = gsub("prob.", "", pred.cat) %>%
           ordered(levels = freqlevels))

table(tidycond.3$pred.cat)

# ** Cross validation --------------------------------------

cv.clmm <- function(Data, X, Y, K = 10, seed = 1234, dof){
  # A function that computes cross validation error for a clmm2 model with 4 df in the spline term
  # create K folds using createFolds() function from caret
  # first step is to create train and test data
  # fit the model to the training set
  # use the fitted model and test data to make predictions
  # compare the predicted categories with the true categories to give you the test error(cv)
  # repeat the process K times
  # output a tibble with cv error for each df
  
  cross.validation.err <- c()
  degrees.freedom <- c()
  set.seed(seed)
  
  for (j in 1:length(dof)) {
    folds <- caret::createFolds(Data[[Y]], k = K, list = TRUE)
    
    test.error <- c() # create an empty vector
    
    for (i in seq_len(K)) {
      DT.train.i <- Data[-folds[[i]],]
      DT.test.i <- Data[folds[[i]],]
      clmm.fit.i <- clmm(Condom.frequency ~ ns(Age.difference,df = dof[j]) + (1|Uid),
                         data = DT.train.i,
                         nAGQ = 7,
                         Hess = T)
      
      predictions.i <- predict.clmm(clmm.fit.i,newdata = DT.test.i,dof = dof[j])
      
      test.error.i <- sum(DT.test.i[,Y] != predictions.i$pred.cat)/nrow(DT.test.i)
      test.error[i] <- test.error.i
    }
    cross.validation.err[j] <- mean(test.error)
    degrees.freedom[j] <- j
  }
  CV.dataframe <- dplyr::tibble(degrees.freedom,cross.validation.err)
  return(CV.dataframe)
}

# example
degreesoffreedom <- c(1:20)

start_time <- Sys.time()
mycv <- cv.clmm(Data = DT.reldata.men, X = "Age.difference", Y = "Condom.frequency", K = 10,dof = 1, seed = 1)

save(mycv, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/cv_condomuse.Rdata")

plot(mycv, type = "l")
end_time <- Sys.time()
end_time - start_time

# Based on the cross validation output we set degrees of freedom = 2 in condom.M2

# Sex Frequency Analysis -----------------------------------------------------
# ** Subset and Exploratory data analysis -----------------------------------

# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")


DT.sexdata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            No.partners,
            Age.difference,
            Sex.frequency = ordered(Sex.frequency, levels = sexlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Sex.frequency)

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

# the distribution of age difference by sex frequency
# to contrast age difference across the 3 condom use levels we use the following boxplot. 
# The three condom use levels have a similar age difference
plot(Age.difference ~ Sex.frequency,
     data = DT.sexdata.men)

# sex frequncies levels
ggplot(data = DT.sexdata.men) +
  geom_bar(aes(Sex.frequency))

ggsave("sexfreqdist.png", width = 6.25, height = 5.25,dpi = 600)



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
  geom_line(color = "#009E73", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#009E73", alpha = 0.25)

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
  scale_fill_brewer(name = "Sex frequency", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

sex.1a
ggsave("Sexfrequncy.png", width = 6.25, height = 5.25,dpi = 600)

# Predicted effects on sex  frequency
tidysex.1b <- OrdPred(sex.M1,"Age.difference",DT.sexdata.men)

#VarPred(sex .M1,"Age.difference",DT.sexdata.men)

sex.pred <- tidysex.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Sex frequency score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

sex.pred
ggsave("Sexpred.png", width = 6.25, height = 5.25,dpi = 600)

# ** Step 3, regression spline model --------------------------

# cumulative logit random intercept model

start_time <- Sys.time()

sex.M2 <- clmm(Sex.frequency ~ ns(Age.difference,df = 6) + (1|Uid),
                  #random =  Uid,
                  data = DT.sexdata.men,
                  #link = "logit", dont specify because effects dont work when specify
                  nAGQ = 7,
                  Hess = T) #if you need to call summary

summary(sex.M2)

end_time <- Sys.time()
end_time - start_time

plot(Effect("Age.difference", sex.M2))
# Extracting the effects generated using the effects function

tidysex.2 <- Effect("Age.difference", sex.M2, 
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

sex.2a <- tidysex.2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Sex frequency", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

sex.2a
ggsave("Sexfrequency2.png", width = 6.25, height = 5.25,dpi = 600)

# Predicted effects on sex frequency
tidysex.2b <- OrdPred(sex.M2, "Age.difference",DT.sexdata.men)

#VarPred(sex.M2,"Age.difference",DT.sexdata.men)

sex.pred2 <- tidysex.2b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Condom use score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

sex.pred2
ggsave("Sexpred2.png", width = 6.25, height = 5.25,dpi = 600)

predictions <- predict.clmm(sex.M2,newdata = DT.sexdata.men,dof = 6)

table(predictions$pred.cat)

cv.clmm <- function(Data, X, Y, K = 10, seed = 1234, dof){
  # A function that computes cross validation error for a clmm2 model with 4 df in the spline term
  # create K folds using createFolds() function from caret
  # first step is to create train and test data
  # fit the model to the training set
  # use the fitted model and test data to make predictions
  # compare the predicted categories with the true categories to give you the test error(cv)
  # repeat the process K times
  # output a tibble with cv error for each df

  #Data = Data
  cross.validation.err <- c()
  degrees.freedom <- c()
  set.seed(seed)
  
  for (j in 1:length(dof)) {
    folds <- caret::createFolds(Data[[Y]], k = K, list = TRUE)
    
    test.error <- c() # create an empty vector
    
    for (i in seq_len(K)) {
      DT.train.i <- Data[-folds[[i]],]
      DT.test.i <- Data[folds[[i]],]
      clmm.fit.i <- clmm(Sex.frequency ~ ns(Age.difference,df = dof[j]) + (1|Uid),
                         data = DT.train.i,
                         nAGQ = 7,
                         Hess = T)
      
      predictions.i <- predict.clmm(clmm.fit.i,newdata = DT.test.i,dof = dof[j])
      
      test.error.i <- sum(DT.test.i[,Y] != predictions.i$pred.cat)/nrow(DT.test.i)
      test.error[i] <- test.error.i
    }
    cross.validation.err[j] <- mean(test.error)
    degrees.freedom[j] <- j
  }
  CV.dataframe <- dplyr::tibble(degrees.freedom,cross.validation.err)
  return(CV.dataframe)
}

# debug(cv.clmm)
mycv.sex <- cv.clmm(Data = DT.sexdata.men, X = "Age.difference", Y = "Sex.frequency", K = 10,dof = degreesoffreedom, seed = 1)

save(mycv.sex, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/cv_sexfreq.Rdata")
plot(mycv.sex, type = "l")

# Based on the cross validation output we set degrees of freedom = 6 in sex.M2

# Partner Type Analysis -----------------------------------------------------
# ** Subset and Exploratory data analysis -----------------------------------

# ordering levels
freqlevels = c("never","sometimes","always")
partlevels = c("casual partner","regular partner","husband/wife")


DT.partnerdata.men <- DT.Agemix.men %>% 
  transmute(Uid = as.factor(Uid),
            No.partners,
            Age.difference,
            Partner.type = ordered(Partner.type, levels = partlevels),
            Money.gifts = ordered(Money.gifts, levels = freqlevels)) %>% 
  drop_na(Age.difference,Partner.type)

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

# to contrast age difference across the 3 partner type levels we use the following boxplot. 
plot(Age.difference ~ Partner.type,
     data = DT.partnerdata.men)

# sex frequncies levels
ggplot(data = DT.partnerdata.men) +
  geom_bar(aes(Partner.type))


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
  geom_line(color = "#009E73", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#009E73", alpha = 0.25)

# Extracting the effects generated using the effects function

tidypart.1 <- Effect("Age.difference", partner.M1, 
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

partner.1a <- tidypart.1 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

partner.1a
ggsave("Partnertype.png", width = 6.25, height = 5.25,dpi = 600)

# Predicted effects on partner type
tidypart.1b <- OrdPred(partner.M1,"Age.difference",DT.partnerdata.men)

#VarPred(partner.M1,"Age.difference",DT.partnerdata.men)

partner.pred <- tidypart.1b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

partner.pred

ggsave("Partnerpred.png", width = 6.25, height = 5.25,dpi = 600)
# ** Step 3, regression spline model --------------------------

# cumulative logit random intercept model

start_time <- Sys.time()

partner.M2 <- clmm(Partner.type ~ ns(Age.difference,df = 3) + (1|Uid),
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
           ordered(levels = c("husband.wife","regular.partner","casual.partner"))%>%
           plyr::mapvalues(from = c("husband.wife","regular.partner","casual.partner"),
                           to = partlevels)) %>% 
  spread(fit, value) 

part.2a <- tidypart.2 %>%
  ggplot(aes(x = Age.difference, y = prob, fill = cond)) +
  geom_area(position = position_stack(reverse = F)) +
  xlab("Age difference") +
  ylab("Probability") +
  scale_fill_brewer(name = "Partner type", 
                    palette = "Dark2")+
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

part.2a
ggsave("Partnertype2.png", width = 6.25, height = 5.25,dpi = 600)

# Predicted effects on sex frequency
tidypart.2b <- OrdPred(partner.M2, "Age.difference",DT.partnerdata.men)

#VarPred(partner.M2,"Age.difference",DT.partnerdata.men)

part.pred2 <- tidypart.2b %>%
  ggplot(aes(x = Age.difference, y = fit)) +
  geom_line(size = 1, color = "#009E73") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.25,
              fill = "#009E73") +
  xlab("Age difference") +
  ylab("Partner type score") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) 

part.pred2
ggsave("Partnerpred2.png", width = 6.25, height = 5.25,dpi = 600)

predictions <- predict.clmm(partner.M2,newdata = DT.partnerdata.men,dof = 4)

table(predictions$pred.cat)

cv.clmm <- function(Data, X, Y, K = 10, seed = 1234, dof){
  # A function that computes cross validation error for a clmm2 model with 4 df in the spline term
  # create K folds using createFolds() function from caret
  # first step is to create train and test data
  # fit the model to the training set
  # use the fitted model and test data to make predictions
  # compare the predicted categories with the true categories to give you the test error(cv)
  # repeat the process K times
  # output a tibble with cv error for each df
  
  #Data = Data
  cross.validation.err <- c()
  degrees.freedom <- c()
  set.seed(seed)
  
  for (j in 1:length(dof)) {
    folds <- caret::createFolds(Data[[Y]], k = K, list = TRUE)
    
    test.error <- c() # create an empty vector
    
    for (i in seq_len(K)) {
      DT.train.i <- Data[-folds[[i]],]
      DT.test.i <- Data[folds[[i]],]
      clmm.fit.i <- clmm(Partner.type ~ ns(Age.difference,df = dof[j]) + (1|Uid),
                         data = DT.train.i,
                         nAGQ = 7,
                         Hess = T)
      
      predictions.i <- predict.clmm(clmm.fit.i,newdata = DT.test.i,dof = dof[j])
      
      test.error.i <- sum(DT.test.i[,Y] != predictions.i$pred.cat)/nrow(DT.test.i)
      test.error[i] <- test.error.i
    }
    cross.validation.err[j] <- mean(test.error)
    degrees.freedom[j] <- j
  }
  CV.dataframe <- dplyr::tibble(degrees.freedom,cross.validation.err)
  return(CV.dataframe)
}

# debug(cv.clmm)
mycv.partner <- cv.clmm(Data = DT.partnerdata.men, X = "Age.difference", Y = "Partner.type", K = 10,dof = degreesoffreedom, seed = 1)

save(mycv.partner, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/cv_partner.Rdata")
plot(mycv.partner, type = "l")

# Based on the cross validation output we set degrees of freedom = 3 in partner.M2


# Implementing GAMMs for comparision partner type model -----------------------------------
# Generalized additive mixed models
library(mgcv)

# Response should be integer class labels
DT.partnerdata.men.gamm <- DT.partnerdata.men 
levels(DT.partnerdata.men.gamm$Partner.type) <- c(1,2,3)
DT.partnerdata.men.gamm$Partner.type <- as.numeric(DT.partnerdata.men.gamm$Partner.type)


# with random effects(basic model) 
# Random intercepts are coded by including a smooth over the grouping variable with the
# smoothing class specified as bs="re".

gam.Partner <- bam(Partner.type ~ s(Age.difference, bs="cr", k = 10) + s(Uid, bs="re"), # penalized cubic regression splines
                   data = DT.partnerdata.men.gamm,
                   family = ocat(R = 3),
                   method = "fRELM") #fREML is much faster and yields similar results like RELM
                   

gam.Partner
summary(gam.Partner)
coef(gam.Partner) ## estimated coefficients

gam.check(gam.Partner)

plot(gam.Partner, residuals = T)

predict.gam(gam.Partner, type = "response")



