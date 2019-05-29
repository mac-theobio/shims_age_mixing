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
edulevel = c("primary" = 1, "secondary" = 2, "higher" = 3, "did not attend" = 4, "DK" = 5, "REF" = 6)
# Subset Data

T1.agemix <- T1.Baseline %>% transmute(Uid = uid,
                                       HHid = hhid,
                                       SHIMS.EA = SHIMS_EA,
                                       EnrollmentDate = dmy(REQ_erdt),
                                       Region = region,
                                       Gender = factor(REQsex, levels = gender, labels = names(gender)),
                                       Age = age_REQ,
                                       Age.sex.debut = RQftage,
                                       Education.level = factor(REQhlsa, levels = edulevel, labels = names(edulevel)),
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

T1.agemix.men <- filter(T1.agemix, Gender == "male")
T1.agemix.men[,6:36][T1.agemix.men[,6:36] == "REF"] <- NA
# ================
# Data exploration
# ================
summary(T1.agemix.men)
table(T1.agemix.men$No.partners) # total number of partners reported
mean(T1.agemix.men$No.partners, na.rm = T)

# ====================
# Create new variables
# ====================

# create a variable for age of participant when he/she had sexual rel started with 
# partner 1,2,3

T1.agemix.men$Age.res.p1 <- AgeResAtRelOnset(currentage = T1.agemix.men$Age,
                                         currentdate = T1.agemix.men$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix.men$Start.rel.date.p1))

T1.agemix.men$Age.res.p2 <- AgeResAtRelOnset(currentage = T1.agemix.men$Age,
                                         currentdate = T1.agemix.men$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix.men$Start.rel.date.p2))

T1.agemix.men$Age.res.p3 <- AgeResAtRelOnset(currentage = T1.agemix.men$Age,
                                         currentdate = T1.agemix.men$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix.men$Start.rel.date.p3))



# Relationship duration in years (*12 converts to months)

T1.agemix.men$Rel.dur.p1 <- (as.yearmon(T1.agemix.men$End.rel.date.p1,"%b%y") - as.yearmon(T1.agemix.men$Start.rel.date.p1,"%b%y"))

T1.agemix.men$Rel.dur.p2 <- (as.yearmon(T1.agemix.men$End.rel.date.p2,"%b%y") - as.yearmon(T1.agemix.men$Start.rel.date.p2,"%b%y"))

T1.agemix.men$Rel.dur.p3 <- (as.yearmon(T1.agemix.men$End.rel.date.p3,"%b%y") - as.yearmon(T1.agemix.men$Start.rel.date.p3,"%b%y"))

# An indicator was created to denote whether or not the relationship was 
# ongoing at the time of the interview or ended. Those relationships that were 
# ongoing had right-censored relationship durations

T1.agemix.men$Rel.ongoing.p1 <- as.yearmon(T1.agemix.men$EnrollmentDate) == as.yearmon(T1.agemix.men$End.rel.date.p1, "%b%y")

T1.agemix.men$Rel.ongoing.p2 <- as.yearmon(T1.agemix.men$EnrollmentDate) == as.yearmon(T1.agemix.men$End.rel.date.p2, "%b%y")

T1.agemix.men$Rel.ongoing.p3 <- as.yearmon(T1.agemix.men$EnrollmentDate) == as.yearmon(T1.agemix.men$End.rel.date.p3, "%b%y")

# computing age differences defined as the male partner's age minus the female 
# partner's age

T1.agemix.men$Age.diff.p1 <-  ifelse(T1.agemix.men$Gender == "male", 
                                 T1.agemix.men$Age.res.p1-T1.agemix.men$Partner.age.p1,
                                 T1.agemix.men$Partner.age.p1 - T1.agemix.men$Age.res.p1)

T1.agemix.men$Age.diff.p2 <- ifelse(T1.agemix.men$Gender == "male", 
                                T1.agemix.men$Age.res.p2-T1.agemix.men$Partner.age.p2,
                                T1.agemix.men$Partner.age.p2 - T1.agemix.men$Age.res.p2)

T1.agemix.men$Age.diff.p3 <- ifelse(T1.agemix.men$Gender == "male", 
                                T1.agemix.men$Age.res.p3-T1.agemix.men$Partner.age.p3,
                                T1.agemix.men$Partner.age.p3 - T1.agemix.men$Age.res.p3)

# ===========
# Subset data
# ===========
# We remove the variables that we no longer require

T1.agemixing <- T1.agemix.men %>% transmute(Uid,
                                            HHid,
                                            SHIMS.EA,
                                            Current.age = Age,
                                            Age.sex.debut,
                                            Education.level,
                                            EnrollmentDate,
                                            No.partners,
                                            Start.rel.date.p1 = DateCleaning(T1.agemix.men$Start.rel.date.p1),
                                            End.rel.date.p1 = DateCleaning(T1.agemix.men$End.rel.date.p1),
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
                                            Start.rel.date.p2 = DateCleaning(T1.agemix.men$Start.rel.date.p2),
                                            End.rel.date.p2 = DateCleaning(T1.agemix.men$End.rel.date.p2),
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
                                            Start.rel.date.p3 = DateCleaning(T1.agemix.men$Start.rel.date.p3),
                                            End.rel.date.p3 = DateCleaning(T1.agemix.men$End.rel.date.p3),
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
sum(table(DT.Agemix.new$Uid) == 1)+
sum(table(DT.Agemix.new$Uid) == 2)+
sum(table(DT.Agemix.new$Uid) == 3)

# tidy dataset
# Remove if age at formation is bigger than current age
# Remove all data from respondents who were less than 12 years old when they formed a relationship
# Subtract 12 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 12 years old.


DT.Agemix.men <- DT.Agemix.new %>% 
  filter(!is.na(Participant.age) & !is.na(Partner.gender)) %>% #removes na's in participant age
  filter(Participant.age <= Current.age |is.na(Participant.age)) %>%#Remove if age at rel formation is > current age
  filter(Participant.age >= 12) %>%         
  mutate(Participant.age = Participant.age - 12) %>% 
  filter(Partner.gender == "female") # remove same sex relationships


sum(table(DT.Agemix.men$Uid) == 1)+
sum(table(DT.Agemix.men$Uid) == 2)+
sum(table(DT.Agemix.men$Uid) == 3)

# # Keeping relationships where age of participant at rel formation was <12  
# DT.Agemix.men.2 <- DT.Agemix.new %>% 
#   mutate(Participant.age = Participant.age - 12) %>% 
#   filter(Partner.gender == "female") # remove same sex relationships

summary(DT.Agemix.men)
# ============================
# Save the data as an R object
# ============================
#save(DT.Agemix.men.2, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.2.Rdata")
save(DT.Agemix.men, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata")


# ===============================================
# Men who did not have at least one relationship
# ==============================================

DT.Agemix.new <- DT.Agemix[rowSums(is.na(DT.Agemix)) < 10, ]

summary(DT.Agemix.new)
DT.Agemix.men.excluded <- DT.Agemix[rowSums(is.na(DT.Agemix)) >= 10, ] %>% 
  group_by(Uid) %>% filter(n()==3)  # if you had no relationships, Uid appears 3 times in the dataset

DT.Agemix.men.excluded.final <- DT.Agemix.men.excluded[!duplicated(DT.Agemix.men.excluded$Uid),]

#save(DT.Agemix.men.excluded.final, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.excluded.final.Rdata")


# ====================
# Female reported data
# ====================
T1.agemix.women <- filter(T1.agemix, Gender == "female")
T1.agemix.women[,3:32][T1.agemix.women[,3:32] == "REF"] <- NA

# ====================
# Create new variables
# ====================

# create a variable for age of participant when he/she had sexual rel started with 
# partner 1,2,3

T1.agemix.women$Age.res.p1 <- AgeResAtRelOnset(currentage = T1.agemix.women$Age,
                                             currentdate = T1.agemix.women$EnrollmentDate,
                                             daterel = DateCleaning(T1.agemix.women$Start.rel.date.p1))

T1.agemix.women$Age.res.p2 <- AgeResAtRelOnset(currentage = T1.agemix.women$Age,
                                             currentdate = T1.agemix.women$EnrollmentDate,
                                             daterel = DateCleaning(T1.agemix.women$Start.rel.date.p2))

T1.agemix.women$Age.res.p3 <- AgeResAtRelOnset(currentage = T1.agemix.women$Age,
                                             currentdate = T1.agemix.women$EnrollmentDate,
                                             daterel = DateCleaning(T1.agemix.women$Start.rel.date.p3))


# Remove if age at formation is bigger than current age

T1.agemix.women <- filter(T1.agemix.women, Age.res.p1 <= Age | is.na(Age.res.p1)) %>% 
  filter(Age.res.p2 <= Age | is.na(Age.res.p2)) %>% 
  filter(Age.res.p3 <= Age | is.na(Age.res.p3)) 

# Relationship duration in years (*12 converts to months)

T1.agemix.women$Rel.dur.p1 <- (as.yearmon(T1.agemix.women$End.rel.date.p1,"%b%y") - as.yearmon(T1.agemix.women$Start.rel.date.p1,"%b%y"))

T1.agemix.women$Rel.dur.p2 <- (as.yearmon(T1.agemix.women$End.rel.date.p2,"%b%y") - as.yearmon(T1.agemix.women$Start.rel.date.p2,"%b%y"))

T1.agemix.women$Rel.dur.p3 <- (as.yearmon(T1.agemix.women$End.rel.date.p3,"%b%y") - as.yearmon(T1.agemix.women$Start.rel.date.p3,"%b%y"))

# An indicator was created to denote whether or not the relationship was 
# ongoing at the time of the interview or ended. Those relationships that were 
# ongoing had right-censored relationship durations

T1.agemix.women$Rel.ongoing.p1 <- as.yearmon(T1.agemix.women$EnrollmentDate) == as.yearmon(T1.agemix.women$End.rel.date.p1, "%b%y")

T1.agemix.women$Rel.ongoing.p2 <- as.yearmon(T1.agemix.women$EnrollmentDate) == as.yearmon(T1.agemix.women$End.rel.date.p2, "%b%y")

T1.agemix.women$Rel.ongoing.p3 <- as.yearmon(T1.agemix.women$EnrollmentDate) == as.yearmon(T1.agemix.women$End.rel.date.p3, "%b%y")

# computing age differences defined as the male partner's age minus the female 
# partner's age

T1.agemix.women$Age.diff.p1 <-  T1.agemix.women$Partner.age.p1 - T1.agemix.women$Age.res.p1

T1.agemix.women$Age.diff.p2 <-  T1.agemix.women$Partner.age.p2 - T1.agemix.women$Age.res.p2

T1.agemix.women$Age.diff.p3 <- T1.agemix.women$Partner.age.p3 - T1.agemix.women$Age.res.p3


# ===========
# Subset data
# ===========
# We remove the variables that we no longer require

T1.agemixing.women <- T1.agemix.women %>% transmute(Uid,
                                            EnrollmentDate,
                                            No.partners,
                                            Start.rel.date.p1 = DateCleaning(T1.agemix.women$Start.rel.date.p1),
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
                                            Start.rel.date.p2 = DateCleaning(T1.agemix.women$Start.rel.date.p2),
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
                                            Start.rel.date.p3 = DateCleaning(T1.agemix.women$Start.rel.date.p3),
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

table(T1.agemixing.women$No.partners)
table(T1.agemixing.women$NO.2nd.partner)
table(T1.agemixing.women$NO.3rd.partner)

# ======================
# convert to long format
# ======================
setDT(T1.agemixing.women) #convert to a data.table for easy manipulation

DT.Agemix.women <- T1.agemixing.women %>% melt( measure = patterns("^Start.rel.date","^End.rel.date","^Age.res", "^Partner.age", "^Age.diff","^Partner.gender",
                                                       "^Condom.freq", "^Sex.freq","^Partner.type", "^Rel.dur",
                                                       "^Rel.ongoing", "^Money.gifts"),
                                    value.name = c("Start.rel.date","End.rel.date","Participant.age", "Partner.age", "Age.difference","Partner.gender",
                                                   "Condom.frequency", "Sex.frequency","Partner.type", "Relationship.dur",
                                                   "Rel.ongoing", "Money.gifts"),
                                    variable.name = "Partner") 


# filter all rows where we lack a single measurement

DT.Agemix.new.women <- DT.Agemix.women[rowSums(is.na(DT.Agemix.women)) < 10, ]
summary(DT.Agemix.new)
# women who reported more than 1,2,3 partner
sum(table(DT.Agemix.new.women$Uid) == 1)+
  sum(table(DT.Agemix.new.women$Uid) == 2)+
  sum(table(DT.Agemix.new.women$Uid) == 3)

# tidy dataset
# Remove all data from respondents who were less than 12 years old when they formed a relationship
# Subtract 12 from all respondent ages, so that a respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 12 years old.

DT.Agemix.women.2 <- DT.Agemix.new.women %>% 
  filter(!is.na(Participant.age) & !is.na(Partner.gender)) %>% #removes na's in participant age
  filter(Participant.age >= 12) %>%         
  mutate(Participant.age = Participant.age - 12) %>% 
  filter(Partner.gender == "male") # remove same sex relationships

sum(table(DT.Agemix.women.2$Uid) == 1)+
  sum(table(DT.Agemix.women.2$Uid) == 2)+
  sum(table(DT.Agemix.women.2$Uid) == 3)

# # Keeping relationships where age of participant at rel formation was <12  
# DT.Agemix.men.2 <- DT.Agemix.new %>% 
#   mutate(Participant.age = Participant.age - 12) %>% 
#   filter(Partner.gender == "female") # remove same sex relationships

# create a categorical var for number of partners
DT.Agemix.women.2 <- mutate(DT.Agemix.women.2, No.partners.category = car::recode(No.partners, "c('2','3','4','5','6','9','11','20','21','31') = '2+'"))

DT.Agemix.women.2[ is.na(DT.Agemix.women.2)] <- NA

summary(DT.Agemix.women.2)
#save(DT.Agemix.women.2, file = "/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.women.2.Rdata")

DT.Agemix.women.2 <- drop_na(DT.Agemix.women.2, Age.difference,No.partners, No.partners.category) %>% 
  filter(No.partners > 0)

table(DT.Agemix.women.2$No.partners)

ggplot(data = DT.Agemix.women.2, aes(Age.difference)) +
  geom_histogram(bins = 30, na.rm = T, col = "gray66", fill = "gray68") +
  xlab("Partner age difference") +
  ylab("Relationships") +
  theme(axis.text.x = element_text(size=14), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=14)) +
  theme(text=element_text( size=14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5.5)) +
  facet_wrap(~ No.partners.category)

ggplot(data = DT.Agemix.women.2, aes(Age.difference, No.partners)) + 
  geom_jitter(size=3, width = 0.25, height = 0.25, alpha = 0.5, show.legend = T) 

ggplot(data = DT.Agemix.women.2, aes(No.partners)) +
  geom_histogram(bins = 30, na.rm = T, col = "gray66", fill = "gray68") +
  xlab("Number of partners") +
  ylab("Relationships") 

