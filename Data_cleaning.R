# ==============
# Load libraries
# ==============

library(tidyverse)
library(readxl)
library(sas7bdat)
library(data.table)
library(zoo)        #dates
library(lubridate)
# =======================
# Source global functions
# =======================

source("Functions_for_SHIMS_study.R")

# ================
# Data importation
# ================
T1.Baseline <- read.sas7bdat("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/baseline.sas7bdat")

# ordering levels
freqlevels = c("never" = 3, "sometimes" = 2, "always" = 1,"REF" = 4)
sexlevels = c("1" = 1, "between 2-5" = 2, "between 6-10" = 3,"more than 10" = 4,"REF" = 5)
partlevels = c( "casual partner" = 3, "regular partner" = 2, "husband/wife" = 1, "REF" = 4)
gender = c("male" = 1, "female" = 2)
partnersex = c("male" = 1, "female" = 2, "DK" = 3, "REF" = 4)

# Subset Data

T1.agemix <- T1.Baseline %>% transmute(Uid = uid,
                                       EnrollmentDate = dmy(REQ_erdt),
                                       Region = region,
                                       Gender = factor(REQsex, levels = gender, labels = names(gender)),
                                       Age = age_REQ,
                                       No.partners = as.numeric(RQtnp6m),
                                       Start.rel.date.p1 = RQp1rbmy,
                                       End.rel.date.p1 = RQp1remy,
                                       Partner.age.p1 = as.numeric(RQp1ftyy),
                                       Partner.gender.p1 = factor(RQp1ps, levels = partnersex, labels = names(partnersex)),
                                       Condom.freq.p1 = ordered(RQp1hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Sex.freq.p1 = ordered(RQp1hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Partner.type.p1 = ordered(RQp1psr,levels = partlevels, labels = names(partlevels)),
                                       Money.gifts.p1 = ordered(RQp1grm,levels = freqlevels, labels = names(freqlevels)),
                                       NO.2nd.partner = RQno2p,
                                       Start.rel.date.p2 = RQp2rbmy,
                                       End.rel.date.p2 = RQp2remy,
                                       Partner.age.p2 = as.numeric(RQp2ftyy),
                                       Partner.gender.p2 = factor(RQp2ps, levels = partnersex, labels = names(partnersex)),
                                       Condom.freq.p2 = ordered(RQp2hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Sex.freq.p2 = ordered(RQp2hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Partner.type.p2 = ordered(RQp2psr,levels = partlevels, labels = names(partlevels)),
                                       Money.gifts.p2 = ordered(RQp2grm,levels = freqlevels, labels = names(freqlevels)),
                                       NO.3rd.partner = RQno3p,
                                       Start.rel.date.p3 = RQp3rbmy,
                                       End.rel.date.p3 = RQp3remy,
                                       Partner.age.p3 = as.numeric(RQp3ftyy),
                                       Partner.gender.p3 = factor(RQp3ps, levels = partnersex, labels = names(partnersex)),
                                       Condom.freq.p3 = ordered(RQp3hocu,levels = freqlevels, labels = names(freqlevels)),
                                       Sex.freq.p3 = ordered(RQp3hm6s,levels = sexlevels, labels = names(sexlevels)),
                                       Money.gifts.p3 = ordered(RQp3grm,levels = freqlevels, labels = names(freqlevels)),
                                       Partner.type.p3 = ordered(RQp3psr,levels = partlevels, labels = names(partlevels))
)


# looking at the data, NaN appears to be used when there is no information...eg if someone did not have
# a sexual relationship then data for partners will appear as NaN-----this implies that this is not missing data
# in most cases

# ==============================
# Remove female participant data
# ==============================

T1.agemix <- filter(T1.agemix, Gender == "male")
T1.agemix[,3:32][T1.agemix[,3:32] == "REF"] <- NA
# ================
# Data exploration
# ================
summary(T1.agemix)
table(T1.agemix$No.partners) # total number of partners reported
mean(T1.agemix$No.partners, na.rm = T)

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


# Remove if age at formation is bigger than current age

T1.agemix <- filter(T1.agemix, Age.res.p1 <= Age | is.na(Age.res.p1)) %>% 
  filter(Age.res.p2 <= Age | is.na(Age.res.p2)) %>% 
  filter(Age.res.p3 <= Age | is.na(Age.res.p3)) 

# create a variable for relationship duration [weeks]

# T1.agemix$Rel.dur.p1 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p1), 
#                                  DateCleaning(T1.agemix$Start.rel.date.p1),
#                                  units = "weeks"))
# 
# T1.agemix$Rel.dur.p2 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p2), 
#                                  DateCleaning(T1.agemix$Start.rel.date.p2),
#                                  units = "weeks"))
# 
# T1.agemix$Rel.dur.p3 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p3), 
#                                  DateCleaning(T1.agemix$Start.rel.date.p3),
#                                  units = "weeks"))

# Relationship duration in months

T1.agemix$Rel.dur.p1 <- (as.yearmon(T1.agemix$End.rel.date.p1,"%b%y") - as.yearmon(T1.agemix$Start.rel.date.p1,"%b%y"))*12

T1.agemix$Rel.dur.p2 <- (as.yearmon(T1.agemix$End.rel.date.p2,"%b%y") - as.yearmon(T1.agemix$Start.rel.date.p2,"%b%y"))*12

T1.agemix$Rel.dur.p3 <- (as.yearmon(T1.agemix$End.rel.date.p3,"%b%y") - as.yearmon(T1.agemix$Start.rel.date.p3,"%b%y"))*12

# An indicator was created to denote whether or not the relationship was 
# ongoing at the time of the interview or ended. Those relationships that were 
# ongoing had right-censored relationship durations

T1.agemix$Rel.ongoing.p1 <- as.yearmon(T1.agemix$EnrollmentDate) == as.yearmon(T1.agemix$End.rel.date.p1, "%b%y")

T1.agemix$Rel.ongoing.p2 <- as.yearmon(T1.agemix$EnrollmentDate) == as.yearmon(T1.agemix$End.rel.date.p2, "%b%y")

T1.agemix$Rel.ongoing.p3 <- as.yearmon(T1.agemix$EnrollmentDate) == as.yearmon(T1.agemix$End.rel.date.p3, "%b%y")

# computing age differences defined as the male partner's age minus the female 
# partner's age

T1.agemix$Age.diff.p1 <-  ifelse(T1.agemix$Gender == "male", 
                                 T1.agemix$Age.res.p1-T1.agemix$Partner.age.p1,
                                 T1.agemix$Partner.age.p1 - T1.agemix$Age.res.p1)

T1.agemix$Age.diff.p2 <- ifelse(T1.agemix$Gender == "male", 
                                T1.agemix$Age.res.p2-T1.agemix$Partner.age.p2,
                                T1.agemix$Partner.age.p2 - T1.agemix$Age.res.p2)

T1.agemix$Age.diff.p3 <- ifelse(T1.agemix$Gender == "male", 
                                T1.agemix$Age.res.p3-T1.agemix$Partner.age.p3,
                                T1.agemix$Partner.age.p3 - T1.agemix$Age.res.p3)


# ===========
# Subset data
# ===========
# We remove the variables that we no longer require

T1.agemixing <- T1.agemix %>% transmute(Uid,
                                        EnrollmentDate,
                                        No.partners,
                                        Start.rel.date.p1 = DateCleaning(T1.agemix$Start.rel.date.p1),
                                        End.rel.date.p1,
                                        Age.res.p1,
                                        Partner.age.p1,
                                        Age.diff.p1,
                                        Partner.gender.p1,
                                        Condom.freq.p1,
                                        Sex.freq.p1,
                                        Partner.type.p1,
                                        Rel.dur.p1,
                                        Rel.ongoing.p1,
                                        Money.gifts.p1,
                                        NO.2nd.partner,
                                        Start.rel.date.p2 = DateCleaning(T1.agemix$Start.rel.date.p2),
                                        End.rel.date.p2,
                                        Age.res.p2,
                                        Partner.age.p2,
                                        Age.diff.p2,
                                        Partner.gender.p2,
                                        Condom.freq.p2,
                                        Sex.freq.p2,
                                        Partner.type.p2,
                                        Rel.dur.p2,
                                        Rel.ongoing.p2,
                                        Money.gifts.p2,
                                        NO.3rd.partner,
                                        Start.rel.date.p3 = DateCleaning(T1.agemix$Start.rel.date.p3),
                                        End.rel.date.p3,
                                        Age.res.p3,
                                        Partner.age.p3,
                                        Age.diff.p3,
                                        Partner.gender.p3,
                                        Condom.freq.p3,
                                        Sex.freq.p3,
                                        Partner.type.p3,
                                        Rel.dur.p3,
                                        Rel.ongoing.p3,
                                        Money.gifts.p3)

table(T1.agemixing$No.partners)
table(T1.agemixing$NO.2nd.partner)
table(T1.agemixing$NO.3rd.partner)

# ======================================================
# Remove 987 participants who reported 0 sexual partners
# ======================================================
# NB: I manually checked and all did not report anything regarding a relationship/partner
summary(T1.agemixing)

T1.agemixing.1 <- filter(T1.agemixing, is.nan(No.partners) | No.partners != 0)
summary(T1.agemixing.1)
# ======================
# convert to long format
# ======================

setDT(T1.agemixing.1) #convert to a data.table for easy manipulation

DT.Agemix <- T1.agemixing.1 %>% melt( measure = patterns("^Start.rel.date","^End.rel.date","^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                       "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                       "^Rel.ongoing", "^Money.gifts"),
                                    value.name = c("Start.rel.date","End.rel.date","Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                   "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                   "Rel.ongoing", "Money.gifts"),
                                    variable.name = "Partner") 


DT.Agemix.1part <- filter(DT.Agemix, No.partners == 1 & Partner == 1)
n_distinct(DT.Agemix.1part$Uid)
DT.Agemix.2part <- filter(DT.Agemix, No.partners == 2 & Partner != 3)
n_distinct(DT.Agemix.2part$Uid)
DT.Agemix.3part <- filter(DT.Agemix, No.partners >= 3)
n_distinct(DT.Agemix.3part$Uid)


# ======================
# convert to long format
# ======================
setDT(T1.agemixing) #convert to a data.table for easy manipulation

DT.Agemix <- T1.agemixing %>% melt( measure = patterns("^Start.rel.date","^End.rel.date","^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                         "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                         "^Rel.ongoing", "^Money.gifts"),
                                      value.name = c("Start.rel.date","End.rel.date","Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                     "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                     "Rel.ongoing", "Money.gifts"),
                                      variable.name = "Partner") 


# filter all rows where we lack a single measurement

DT.Agemix.new <- DT.Agemix[rowSums(is.na(DT.Agemix)) < 10, ]
summary(DT.Agemix.new)
# men who reported more than 1,2,3 partner
sum(table(DT.Agemix.new$Uid) == 1)
sum(table(DT.Agemix.new$Uid) == 2)
sum(table(DT.Agemix.new$Uid) == 3)

# tidy dataset
# Remove all data from respondents who were less than 12 years old when they formed a relationship
# Subtract 12 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 12 years old.

DT.Agemix.men <- DT.Agemix.new %>% 
  filter(Participant.age >= 12) %>% #removes na's in participant age
  mutate(Participant.age = Participant.age - 12) %>% 
  filter(Partner.gender == "female") # remove same sex relationships

# Not removing relationships where age of participant at rel formation was <15  
DT.Agemix.men.2 <- DT.Agemix.new %>% 
  mutate(Participant.age = Participant.age - 12) %>% 
  filter(Partner.gender == "female") # remove same sex relationships

summary(DT.Agemix.men)
# ============================
# Save the data as an R object
# ============================
save(DT.Agemix.men.2, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.2.Rdata")
save(DT.Agemix.men, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata")

