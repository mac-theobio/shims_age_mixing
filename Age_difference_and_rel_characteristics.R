setwd("/home/emanuel/Desktop/SHIMS/shims_age_mixing")

# load the cleaned data stored in a dataframe
load("T1_agemix.Rdata")

############################################################################################
# computing age differences defined as the male partner's age minus the female partner's age

T1_agemix$Age_diff_p1 <- ifelse(T1_agemix$REQsex == "male", 
                                   T1_agemix$Age_res_p1-T1_agemix$RQp1ftyy,
                                   T1_agemix$RQp1ftyy - T1_agemix$Age_res_p1)
  
T1_agemix$Age_diff_p2 <- ifelse(T1_agemix$REQsex == "male", 
                                   T1_agemix$Age_res_p2-T1_agemix$RQp2ftyy,
                                   T1_agemix$RQp2ftyy - T1_agemix$Age_res_p2)

T1_agemix$Age_diff_p3 <- ifelse(T1_agemix$REQsex == "male", 
                                   T1_agemix$Age_res_p3-T1_agemix$RQp3ftyy,
                                   T1_agemix$RQp3ftyy - T1_agemix$Age_res_p3)
