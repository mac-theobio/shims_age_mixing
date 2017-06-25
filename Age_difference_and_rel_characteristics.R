library(data.table)
library(ggplot2)
library(dplyr)
library(nlme)

setwd("/home/emanuel/Documents/SHIMS/shims_age_mixing")
load("T1.agemix.Rdata")
######################################################################################
# We want to tidy the data frame by subsetting and converting it to long format
# each row represents a relationship

T1.reldata <- T1.agemix %>% transmute(Uid,
                                      Gender,
                                      Age.diff.p1,
                                      Age.diff.p2,
                                      Age.diff.p3,
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
                                      Money.gifts.p1,
                                      Money.gifts.p2,
                                      Money.gifts.p3)

setDT(T1.reldata) #convert to a data.table for easy manipulation

DT.reldata = T1.reldata %>% melt( measure = patterns("^Age.diff", "^Condom.freq", 
                                                     "^Sex.freq","^Partner.type",
                                                     "^Rel.dur", "^Money.gifts"),
                                  value.name = c("Age.difference", "Condom.frequency",
                                                 "Sex.frequency","Partner.type", 
                                                 "Relationship.dur", "Money.gifts"),
                                  variable.name = "Partner")

#####################################################################################
# Removing respondents who did not report relationship 1,2,3

DT.reldata.clean <- DT.reldata[!(is.na(DT.reldata$Age.difference) &
                                   is.na(DT.reldata$Sex.frequency) &
                                   is.na(DT.reldata$Partner.type) &
                                   is.na(DT.reldata$Relationship.dur) &
                                   is.na(DT.reldata$Money.gifts))]

#####################################################################################
# Fitting the models

