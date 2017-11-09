# ==============
# Load libraries
# ==============

library(tidyverse)
library(readxl)

source("Functions_for_SHIMS_study.R")
# ================
# Woring directory
# ================

setwd("C:/Users/Emanuel/Desktop/shims_age_mixing")

# ================
# Data importation
# ================

sample_baseline <- read_excel(path = "SAMPLE_T1_2017-05-02_00-59-36.xlsx")

class(sample_baseline)
glimpse(sample_baseline)

sample_baseline

# Eliminate spaces from variable names (old style)
names(sample_baseline) <- make.names(names(sample_baseline))
names(sample_baseline) <- gsub("[.]", "_", names(sample_baseline))

# ===============
# Subset the data
# ===============

# ordering levels
freqlevels = c("never","sometimes","always")
sexlevels = c("1","between 2-5","between 6-10","more than 10")
partlevels = c("husband/wife","regular partner","casual partner")

# Subset the data as you change the variable names and data types

T1_agemix <- sample_baseline %>% transmute(Uid = Uid,
                                           Region,
                                           Gender = as.factor(REQsex),
                                           Age = Age_REQ,
                                           EnrollmentDate = as.POSIXct(REQ_Erdt),
                                           Start_rel_date_p1 = RQp1rbmy,
                                           End_rel_date_p1 = RQp1remy,
                                           Partner_age_p1 = as.numeric(RQp1ftyy),
                                           Start_rel_date_p2 = RQp2rbmy,
                                           End_rel_date_p2 = RQp2remy,
                                           Partner_age_p2 = as.numeric(RQp2ftyy),
                                           Start_rel_date_p3 = RQp3rbmy,
                                           End_rel_date_p3 = RQp3remy,
                                           Partner_age_p3 = as.numeric(RQp3ftyy),
                                           No_partners = as.numeric(RQtnp6m),
                                           Condom_freq_p1 = ordered(RQp1hocu,levels = freqlevels),
                                           Condom_freq_p2 = ordered(RQp2hocu,levels = freqlevels),
                                           Condom_freq_p3 = ordered(RQp3hocu,levels = freqlevels),
                                           Sex_freq_p1 = ordered(RQp1hm6s,levels = sexlevels),
                                           Sex_freq_p2 = ordered(RQp2hm6s,levels = sexlevels),
                                           Sex_freq_p3 = ordered(RQp3hm6s,levels = sexlevels),
                                           Money_gifts_p1 = ordered(RQp1grm,levels = freqlevels),
                                           Money_gifts_p2 = ordered(RQp2grm,levels = freqlevels),
                                           Money_gifts_p3 = ordered(RQp3grm,levels = freqlevels),
                                           Partner_type_p1 = ordered(RQp1psr,levels = partlevels),
                                           Partner_type_p2 = ordered(RQp2psr,levels = partlevels),
                                           Partner_type_p3 = ordered(RQp3psr,levels = partlevels))

# =============
# Date cleaning
# =============


# =========================================
# Create and add new columns to the dataset
# =========================================
# create a variable for age of participant when he/she had sexual rel started with 
# partner 1

T1_agemix %>% mutate(Age_res_p1 = AgeResAtRelOnset(currentage = Age,
                                                   currentdate = EnrollmentDate,
                                                   daterelstart = Start_rel_date_p1),
                     Age_res_p2 = AgeResAtRelOnset(currentage = Age,
                                                   currentdate = EnrollmentDate,
                                                   daterelstart = Start_rel_date_p2),
                     Age_res_p3 = AgeResAtRelOnset(currentage = Age,
                                                   currentdate = EnrollmentDate,
                                                   daterelstart = Start_rel_date_p3))