# ==============
# Load libraries
# ==============

library(tidyverse)
library(readxl)
library(sas7bdat)
library(data.table)
# =======================
# Source global functions
# =======================

source("Functions_for_SHIMS_study.R")

# ================
# Data importation
# ================
T1.Baseline <- read.sas7bdat("/Users/emanuel/Dropbox/SHIMS Baseline data/baseline.sas7bdat")

# ordering levels
freqlevels = c("never" = 3, "sometimes" = 2, "always" = 1,"REF" = 4)
sexlevels = c("1" = 1, "between 2-5" = 2, "between 6-10" = 3,"more than 10" = 4,"REF" = 5)
partlevels = c( "casual partner" = 3, "regular partner" = 2, "husband/wife" = 1, "REF" = 4)
gender = c("male" = 1, "female" = 2)
partnersex = c("male" = 1, "female" = 2, "DK" = 3, "REF" = 4)

# ===============
# Subset the data
# ===============

# Subset the data as you change the variable names and data types

T1.agemix <- T1.Baseline %>% transmute(Uid = uid,
                                       Region = region,
                                       Gender = factor(REQsex, levels = gender, labels = names(gender)),
                                       Age = age_REQ,
                                       EnrollmentDate = as.Date(REQ_erdt, "%d%b%y"),
                                       Start.rel.date.p1 = RQp1rbmy,
                                       End.rel.date.p1 = RQp1remy,
                                       Partner.age.p1 = as.numeric(RQp1ftyy),
                                       Partner.gender.p1 = factor(RQp1ps, levels = partnersex, labels = names(partnersex)),
                                       Start.rel.date.p2 = RQp2rbmy,
                                       End.rel.date.p2 = RQp2remy,
                                       Partner.age.p2 = as.numeric(RQp2ftyy),
                                       Partner.gender.p2 = factor(RQp2ps, levels = partnersex, labels = names(partnersex)),
                                       Start.rel.date.p3 = RQp3rbmy,
                                       End.rel.date.p3 = RQp3remy,
                                       Partner.age.p3 = as.numeric(RQp3ftyy),
                                       Partner.gender.p3 = factor(RQp3ps, levels = partnersex, labels = names(partnersex)),
                                       No.partners = as.numeric(RQtnp6m),
                                       Condom.freq.p1 = ordered(RQp1hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Condom.freq.p2 = ordered(RQp2hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Condom.freq.p3 = ordered(RQp3hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Sex.freq.p1 = ordered(RQp1hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Sex.freq.p2 = ordered(RQp2hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Sex.freq.p3 = ordered(RQp3hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Money.gifts.p1 = ordered(RQp1grm,levels = freqlevels, labels = names(freqlevels)),
                                       Money.gifts.p2 = ordered(RQp2grm,levels = freqlevels, labels = names(freqlevels)),
                                       Money.gifts.p3 = ordered(RQp3grm,levels = freqlevels, labels = names(freqlevels)),
                                       Partner.type.p1 = ordered(RQp1psr,levels = partlevels, labels = names(partlevels)),
                                       Partner.type.p2 = ordered(RQp2psr,levels = partlevels, labels = names(partlevels)),
                                       Partner.type.p3 = ordered(RQp3psr,levels = partlevels, labels = names(partlevels))
)


# looking at the data, NaN appears to be used when there is no information...eg if someone did not have
# a sexual relationship then data for partners will appear as NaN-----this implies that this is not missing data
# in most cases

# ====================
# Create new variables
# ====================

# create a variable for age of participant when he/she had sexual rel started with 
# partner 1,2,3

T1.agemix$Age.res.p1 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p1))

T1.agemix$Age.res.p2 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p2))

T1.agemix$Age.res.p3 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p3))


# create a variable for relationship duration [weeks]

T1.agemix$Rel.dur.p1 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p1), 
                                 DateCleaning(T1.agemix$Start.rel.date.p1),
                                 units = "weeks"))

T1.agemix$Rel.dur.p2 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p2), 
                                 DateCleaning(T1.agemix$Start.rel.date.p2),
                                 units = "weeks"))

T1.agemix$Rel.dur.p3 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p3), 
                                 DateCleaning(T1.agemix$Start.rel.date.p3),
                                 units = "weeks"))


# An indicator was created to denote whether or not the relationship was 
# ongoing at the time of the interview or ended. Those relationships that were 
# ongoing had right-censored relationship durations

T1.agemix$Rel.ended.p1 <- as.factor(ifelse(format(T1.agemix$EnrollmentDate, "%Y-%m") == 
                                 format(DateCleaning(T1.agemix$End.rel.date.p1), "%Y-%m"),
                               0,1))

T1.agemix$Rel.ended.p2 <- as.factor(ifelse(format(T1.agemix$EnrollmentDate, "%Y-%m") == 
                                 format(DateCleaning(T1.agemix$End.rel.date.p2), "%Y-%m"),
                               0,1))

T1.agemix$Rel.ended.p3 <- as.factor(ifelse(format(T1.agemix$EnrollmentDate, "%Y-%m") == 
                                 format(DateCleaning(T1.agemix$End.rel.date.p3), "%Y-%m"),
                               0,1))


# computing age differences defined as the male partner's age minus the female 
# partner's age

T1.agemix$Age.diff.p1 <-  ifelse(T1.agemix$Gender == "Male", 
                                 T1.agemix$Age.res.p1-T1.agemix$Partner.age.p1,
                                 T1.agemix$Partner.age.p1 - T1.agemix$Age.res.p1)

T1.agemix$Age.diff.p2 <- ifelse(T1.agemix$Gender == "Male", 
                                T1.agemix$Age.res.p2-T1.agemix$Partner.age.p2,
                                T1.agemix$Partner.age.p2 - T1.agemix$Age.res.p2)

T1.agemix$Age.diff.p3 <- ifelse(T1.agemix$Gender == "Male", 
                                T1.agemix$Age.res.p3-T1.agemix$Partner.age.p3,
                                T1.agemix$Partner.age.p3 - T1.agemix$Age.res.p3)


# ===========
# Subset data
# ===========

T1.agemixing <- T1.agemix %>% transmute(Uid,
                                        Gender,
                                        No.partners,
                                        Age.res.p1,
                                        Age.res.p2,
                                        Age.res.p3,
                                        Partner.age.p1,
                                        Partner.age.p2,
                                        Partner.age.p3,
                                        Age.diff.p1,
                                        Age.diff.p2,
                                        Age.diff.p3,
                                        Partner.gender.p1,
                                        Partner.gender.p2,
                                        Partner.gender.p3,
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
                                        Rel.ended.p1,
                                        Rel.ended.p2,
                                        Rel.ended.p3,
                                        Money.gifts.p1,
                                        Money.gifts.p2,
                                        Money.gifts.p3)

# convert to long format

setDT(T1.agemixing) #convert to a data.table for easy manipulation

DT.Agemix <- T1.agemixing %>% melt( measure = patterns("^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                       "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                       "^Rel.ended", "^Money.gifts"),
                                    value.name = c("Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                   "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                   "Rel.ended", "Money.gifts"),
                                    variable.name = "Partner") 

                                      
# tidy dataset
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- DT.Agemix %>% 
  filter(Gender == "male" & Participant.age >= 15) %>% 
  mutate(Participant.age = Participant.age - 15) %>% 
  filter(Gender != Partner.gender) %>% # remove same sex #only 44 men reported same sex partners
  select(-Gender) %>% # drop redundant gender variable
  drop_na()

summary(DT.Agemix.men)                                      
# ============================
# Save the data as an R object
# ============================

#T1.agemix <- subset(T1.agemix, Gender == "Male")
# Save the dataframe
save(DT.Agemix.men, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.men.Rdata")

