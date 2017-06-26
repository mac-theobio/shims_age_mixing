library(readxl)
library(dplyr)

setwd("/home/emanuel/Documents/SHIMS/shims_age_mixing")
#####################################################################################
# User defined functions

DateCleaning <- function(daterel){
  paste(substr(daterel,1,3),substr(daterel,4,5),sep = "/") %>%
    paste("01",sep = "/") %>%
    as.Date("%b/%y/%d")
}

AgeResAtRelOnset <- function(currentage, currentdate, daterelstart){
  as.numeric(currentage) - as.numeric(difftime(currentdate,
                                               daterelstart,
                                               units = "weeks")
                                      )/52.25
}

#####################################################################################
# Read the data
Sample.Baseline <- read_excel("SAMPLE_T1_2017-05-02_00-59-36.xlsx")

# Eliminate spaces from variable names (old style)
names(Sample.Baseline) <- make.names(names(Sample.Baseline))

# Subset the data as you change the variable names and data types
T1.agemix <- Sample.Baseline %>% transmute(Uid = Uid,
                                           Gender = as.factor(REQsex),
                                           Age = Age.REQ,
                                           EnrollmentDate = as.Date(REQ.Erdt),
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
                                           Condom.freq.p1 = as.ordered(RQp1hocu),
                                           Condom.freq.p2 = as.ordered(RQp2hocu),
                                           Condom.freq.p3 = as.ordered(RQp3hocu),
                                           Sex.freq.p1 = as.ordered(RQp1hm6s),
                                           Sex.freq.p2 = as.ordered(RQp2hm6s),
                                           Sex.freq.p3 = as.ordered(RQp3hm6s),
                                           Money.gifts.p1 = as.ordered(RQp1grm),
                                           Money.gifts.p2 = as.ordered(RQp2grm),
                                           Money.gifts.p3 = as.ordered(RQp3grm),
                                           Partner.type.p1 = as.ordered(RQp1psr),
                                           Partner.type.p2 = as.ordered(RQp2psr),
                                           Partner.type.p3 = as.ordered(RQp3psr))

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
# create a variable for relationship duration [days]

T1.agemix$Rel.dur.p1 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p1),
                                            DateCleaning(T1.agemix$Start.rel.date.p1),
                                            units = "days"))
T1.agemix$Rel.dur.p2 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p2),
                                            DateCleaning(T1.agemix$Start.rel.date.p2),
                                            units = "days"))
T1.agemix$Rel.dur.p3 <- as.numeric(difftime(DateCleaning(T1.agemix$End.rel.date.p3),
                                            DateCleaning(T1.agemix$Start.rel.date.p3),
                                            units = "days"))
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

# Save the dataframe
save(T1.agemix, file = "T1.agemix.Rdata")
