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

# # =========
# # 5% subset
# # =========
# T1.Baseline <- read_excel("SAMPLE_T1_2017-05-02_00-59-36.xlsx")
# 
# names(T1.Baseline) <- make.names(names(T1.Baseline))
# 
# freqlevels = c("never" , "sometimes", "always" )
# sexlevels = c("1", "between 2-5", "between 6-10","more than 10")
# partlevels = c( "casual partner", "regular partner", "husband/wife")
# gender = c("male" = "Male", "female" = "Female")
# partnersex = c("male", "female")
# 
# T1.agemix <- T1.Baseline %>% transmute(Uid,
#                                        Region,
#                                        Gender = factor(REQsex, levels = gender, labels = names(gender)),
#                                        Age = Age.REQ,
#                                        No.partners = as.numeric(RQtnp6m),
#                                        EnrollmentDate = as.POSIXct(REQ.Erdt),
#                                        Start.rel.date.p1 = RQp1rbmy,
#                                        End.rel.date.p1 = RQp1remy,
#                                        Partner.age.p1 = as.numeric(RQp1ftyy),
#                                        Partner.gender.p1 = factor(RQp1ps, levels = partnersex),
#                                        Condom.freq.p1 = ordered(RQp1hocu,levels = freqlevels),
#                                        Sex.freq.p1 = ordered(RQp1hm6s,levels = sexlevels),
#                                        Partner.type.p1 = ordered(RQp1psr,levels = partlevels),
#                                        Money.gifts.p1 = ordered(RQp1grm,levels = freqlevels),
#                                        NO.2nd.partner = RQno2p,
#                                        Start.rel.date.p2 = RQp2rbmy,
#                                        End.rel.date.p2 = RQp2remy,
#                                        Partner.age.p2 = as.numeric(RQp2ftyy),
#                                        Partner.gender.p2 = factor(RQp2ps, levels = partnersex),
#                                        Condom.freq.p2 = ordered(RQp2hocu,levels = freqlevels),
#                                        Sex.freq.p2 = ordered(RQp2hm6s,levels = sexlevels),
#                                        Partner.type.p2 = ordered(RQp2psr,levels = partlevels),
#                                        Money.gifts.p2 = ordered(RQp2grm,levels = freqlevels),
#                                        NO.3rd.partner = RQno3p,
#                                        Start.rel.date.p3 = RQp3rbmy,
#                                        End.rel.date.p3 = RQp3remy,
#                                        Partner.age.p3 = as.numeric(RQp3ftyy),
#                                        Partner.gender.p3 = factor(RQp3ps, levels = partnersex),
#                                        Condom.freq.p3 = ordered(RQp3hocu,levels = freqlevels),
#                                        Sex.freq.p3 = ordered(RQp3hm6s, levels = sexlevels),
#                                        Money.gifts.p3 = ordered(RQp3grm, levels = freqlevels),
#                                        Partner.type.p3 = ordered(RQp3psr, levels = partlevels)
# )
# 

# ===============
# Subset the data
# ===============

# Remove unwanted variables
# Rename the variables
# Set the data type for some of the variables

T1.agemix <- T1.Baseline %>% transmute(Uid = uid,
                                       Region = region,
                                       Gender = factor(REQsex, levels = gender, labels = names(gender)),
                                       Age = age_REQ,
                                       No.partners = as.numeric(RQtnp6m),
                                       EnrollmentDate = as.Date(REQ_erdt, "%d%b%y"),
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
                                        No.partners,
                                        Age.res.p1,
                                        Partner.age.p1,
                                        Age.diff.p1,
                                        Partner.gender.p1,
                                        Condom.freq.p1,
                                        Sex.freq.p1,
                                        Partner.type.p1,
                                        Rel.dur.p1,
                                        Rel.ended.p1,
                                        Money.gifts.p1,
                                        NO.2nd.partner,
                                        Age.res.p2,
                                        Partner.age.p2,
                                        Age.diff.p2,
                                        Partner.gender.p2,
                                        Condom.freq.p2,
                                        Sex.freq.p2,
                                        Partner.type.p2,
                                        Rel.dur.p2,
                                        Rel.ended.p2,
                                        Money.gifts.p2,
                                        NO.3rd.partner,
                                        Age.res.p3,
                                        Partner.age.p3,
                                        Age.diff.p3,
                                        Partner.gender.p3,
                                        Condom.freq.p3,
                                        Sex.freq.p3,
                                        Partner.type.p3,
                                        Rel.dur.p3,
                                        Rel.ended.p3,
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
T1.agemixing.1[T1.agemixing.1 == "REF"] <- NA
summary(T1.agemixing.1)
# ======================
# convert to long format
# ======================

setDT(T1.agemixing.1) #convert to a data.table for easy manipulation

DT.Agemix <- T1.agemixing.1 %>% melt( measure = patterns("^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                       "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                       "^Rel.ended", "^Money.gifts"),
                                    value.name = c("Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                   "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                   "Rel.ended", "Money.gifts"),
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

DT.Agemix <- T1.agemixing %>% melt( measure = patterns("^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                         "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                         "^Rel.ended", "^Money.gifts"),
                                      value.name = c("Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                     "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                     "Rel.ended", "Money.gifts"),
                                      variable.name = "Partner") 


# filter all rows where we lack a single measurement

DT.Agemix.new <- DT.Agemix[rowSums(is.na(DT.Agemix)) < 10, ]

# men who reported more than 1,2,3 partner
sum(table(DT.Agemix.new$Uid) == 1)
sum(table(DT.Agemix.new$Uid) == 2)
sum(table(DT.Agemix.new$Uid) == 3)

# tidy dataset
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

#DT.Agemix[DT.Agemix == "REF"] <- NA


DT.Agemix.men <- DT.Agemix.new %>% 
  filter(Participant.age >= 15) %>% #removes na's in participant age
  mutate(Participant.age = Participant.age - 15) %>% 
  filter(Partner.gender == "female") # remove same sex relationships
  
sum(table(DT.Agemix.men$Uid) == 1)
sum(table(DT.Agemix.men$Uid) == 2)
sum(table(DT.Agemix.men$Uid) == 3)
summary(DT.Agemix.men)    

# ============================
# Save the data as an R object
# ============================
save(T1.Baseline, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/T1.Baseline.Rdata")
save(DT.Agemix.new, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.new.Rdata")
save(DT.Agemix.men, file = "/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.men.Rdata")

