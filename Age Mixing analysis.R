library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(nlme)
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
# Read the data and subset it

Sample_Baseline <- read_excel("~/Desktop/SHIMS/shims_age_mixing/SAMPLE_T1_2017-05-02_00-59-36.xlsx")

T1_agemix <- Sample_Baseline[,c("Uid","REQsex","Age REQ","REQ Erdt","RQp1rbmy","RQp1ftyy",
                                "RQp2rbmy","RQp2ftyy","RQp3rbmy","RQp3ftyy")]

##################################################################################################
# create a variable for age of participant when he/she had sexual rel started with partner 1

T1_agemix$Age_res_p1 <- Age_res_at_rel_onset(currentage = T1_agemix$`Age REQ`,
                                             currentdate = T1_agemix$`REQ Erdt`,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp1rbmy))

T1_agemix$Age_res_p2 <- Age_res_at_rel_onset(currentage = T1_agemix$`Age REQ`,
                                             currentdate = T1_agemix$`REQ Erdt`,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp2rbmy))

T1_agemix$Age_res_p3 <- Age_res_at_rel_onset(currentage = T1_agemix$`Age REQ`,
                                             currentdate = T1_agemix$`REQ Erdt`,
                                             daterelstart = Rel_onset_date(T1_agemix$RQp3rbmy))
##################################################################################################
# We want to tidy the data frame

T1_agemixing <- T1_agemix[,c("Uid", "REQsex","Age_res_p1","Age_res_p2","Age_res_p3",
                             "RQp1ftyy","RQp2ftyy","RQp3ftyy")]

setDT(T1_agemixing) #convert to a data.table for easy manipulation

DT.Agemix = melt(T1_agemixing, measure = patterns("^Age_res","^RQp"),
             value.name = c("Participant_age","Partner_age"),variable.name = "Partner")

##################################################################################################
# Scatterplot of partner age vs participant age
theme_set(theme_bw())

print(ggplot(na.exclude(DT.Agemix), aes(x=Participant_age, y=as.numeric(Partner_age)))
      + geom_point()
      + facet_wrap(~REQsex)
      + coord_equal()
      + xlab("Participant Age")
      + ylab("Partner Age")
      + ggtitle("Partner vs Participant age at onset of sexual relationship")
      )

#################################################################################################
# Fitting a linear mixed model
# Remove all data from respondents younger than 15 years old
# Subtract 15 from all respondent ages, so that “respondent.age.at.relationship.formation” is
# coded 0 for a man who started a relationship at age 15 years old.

DT.Agemix.men <- na.exclude(DT.Agemix[which(REQsex == "Male" & Participant_age >= 15),])
DT.Agemix.men$Participant_age <- DT.Agemix.men$Participant_age - 15
DT.Agemix.men$Partner_age <- as.numeric(DT.Agemix.men$Partner_age)

model <- lme(Partner_age~Participant_age,
             data = DT.Agemix.men,
             method = "REML",
             weights = varPower(value = 0.5, form = ~Participant_age + 1),
             random = ~1|Uid)