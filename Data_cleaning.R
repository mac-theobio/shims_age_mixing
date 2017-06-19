library(readxl)
library(dplyr)

setwd("/home/emanuel/Desktop/SHIMS/shims_age_mixing")
##################################################################################################
# User defined functions

Rel_onset_date <- function(daterelstart){
  paste(substr(daterelstart,1,3),substr(daterelstart,4,5),sep = "/") %>%
    paste("01",sep = "/") %>%
    as.Date("%b/%y/%d")
}

Age_res_at_rel_onset <- function(currentage, currentdate, daterelstart){
  as.numeric(currentage) - as.numeric(difftime(currentdate,daterelstart,units = "weeks"))/52.25
}

##################################################################################################
# Read the data
Sample_Baseline <- read_excel("~/Desktop/SHIMS/shims_age_mixing/SAMPLE_T1_2017-05-02_00-59-36.xlsx")

# Eliminate spaces from variable names (old style)
names(Sample_Baseline) <- make.names(names(Sample_Baseline))
names(Sample_Baseline) <- gsub("[.]", "_", names(Sample_Baseline))

# Subset the data as you change the names and data types
T1_agemix <- transmute(Sample_Baseline,
                       Uid = Uid,
                       REQsex = as.factor(REQsex),
                       Age_REQ = Age_REQ,
                       REQ_Erdt = as.Date(REQ_Erdt),
                       RQp1rbmy = RQp1rbmy,
                       RQp1ftyy = as.numeric(RQp1ftyy),
                       RQp2rbmy = RQp2rbmy,
                       RQp2ftyy = as.numeric(RQp2ftyy),
                       RQp3rbmy = RQp3rbmy,
                       RQp3ftyy = as.numeric(RQp3ftyy),
                       RQtnp6m = as.numeric(RQtnp6m),
                       RQp1hocu = as.factor(RQp1hocu),
                       RQp2hocu = as.factor(RQp2hocu),
                       RQp3hocu = as.factor(RQp3hocu),
                       RQp1hm6s = as.factor(RQp1hm6s),
                       RQp2hm6s = as.factor(RQp2hm6s),
                       RQp3hm6s = as.factor(RQp3hm6s),
                       RQp1grm = as.factor(RQp1grm),
                       RQp2grm = as.factor(RQp2grm),
                       RQp3grm = as.factor(RQp3grm),
                       RQp1psr = as.factor(RQp1psr),
                       RQp2psr = as.factor(RQp2psr),
                       RQp3psr = as.factor(RQp3psr)
                       )
##################################################################################################
# create a variable for age of participant when he/she had sexual rel started with partner 1

T1_agemix$Age_res_p1 <- Age_res_at_rel_onset(currentage = T1_agemix$Age_REQ,
                                             currentdate = T1_agemix$REQ_Erdt,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp1rbmy))

T1_agemix$Age_res_p2 <- Age_res_at_rel_onset(currentage = T1_agemix$Age_REQ,
                                             currentdate = T1_agemix$REQ_Erdt,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp2rbmy))

T1_agemix$Age_res_p3 <- Age_res_at_rel_onset(currentage = T1_agemix$Age_REQ,
                                             currentdate = T1_agemix$REQ_Erdt,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp3rbmy))

##################################################################################################
# create a variable for relationship duration

save(T1_agemix, file = "T1_agemix.Rdata")
