library(readxl)
library(dplyr)

setwd("/Users/Emanuel/Desktop/shims_age_mixing-master")
source("Functions_for_SHIMS_study.R")
#####################################################################################
# Read the data
Sample.Baseline <- read_excel("SAMPLE_T1_2017-05-02_00-59-36.xlsx")

# Eliminate spaces from variable names (old style)
names(Sample.Baseline) <- make.names(names(Sample.Baseline))

# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("husband/wife","regular partner","casual partner")

# Subset the data as you change the variable names and data types
T1.agemix <- Sample.Baseline %>% transmute(Uid = Uid,
                                           Region,
                                           Gender = as.factor(REQsex),
                                           Age = Age.REQ,
                                           EnrollmentDate = as.POSIXct(REQ.Erdt),
                                           Start.rel.date.p1 = RQp1rbmy,
                                           End.rel.date.p1 = RQp1remy,
                                           Partner.age.p1 = as.numeric(RQp1ftyy),
                                           Start.rel.date.p2 = RQp2rbmy,
                                           End.rel.date.p2 = RQp2remy,
                                           Partner.age.p2 = as.numeric(RQp2ftyy),
                                           Start.rel.date.p3 = RQp3rbmy,
                                           End.rel.date.p3 = RQp3remy,
                                           Partner.age.p3 = as.numeric(RQp3ftyy),
                                           No.partners = as.numeric(RQtnp6m),
                                           Condom.freq.p1 = ordered(RQp1hocu,levels = freqlevels),
                                           Condom.freq.p2 = ordered(RQp2hocu,levels = freqlevels),
                                           Condom.freq.p3 = ordered(RQp3hocu,levels = freqlevels),
                                           Sex.freq.p1 = ordered(RQp1hm6s,levels = sexlevels),
                                           Sex.freq.p2 = ordered(RQp2hm6s,levels = sexlevels),
                                           Sex.freq.p3 = ordered(RQp3hm6s,levels = sexlevels),
                                           Money.gifts.p1 = ordered(RQp1grm,levels = freqlevels),
                                           Money.gifts.p2 = ordered(RQp2grm,levels = freqlevels),
                                           Money.gifts.p3 = ordered(RQp3grm,levels = freqlevels),
                                           Partner.type.p1 = ordered(RQp1psr,levels = partlevels),
                                           Partner.type.p2 = ordered(RQp2psr,levels = partlevels),
                                           Partner.type.p3 = ordered(RQp3psr,levels = partlevels)
                                           )

######################################################################################
# create a variable for age of participant when he/she had sexual rel started with 
# partner 1

T1.agemix$Age.res.p1 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p1))

T1.agemix$Age.res.p2 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p2))

T1.agemix$Age.res.p3 <- AgeResAtRelOnset(currentage = T1.agemix$Age,
                                         currentdate = T1.agemix$EnrollmentDate,
                                         daterel = DateCleaning(T1.agemix$Start.rel.date.p3))

######################################################################################
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

##################################################################################
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

######################################################################################
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

########################################################################################
#T1.agemix <- subset(T1.agemix, Gender == "Male")
# Save the dataframe
save(T1.agemix, file = "T1.agemix.Rdata")

