# Computing the proportion of women and men who reported 0,1,>=2 relationships at baseline
# and at T2

library(readxl)
# Read the data 
SAMPLE_PREVALENCE_2017_03_18_05_08_09 <- 
  read_excel("~/Downloads/SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx")

# coerce the Sexprt Num variable to type numeric
SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` <- 
  as.numeric(as.character(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num`))

# Propotion of female at baseline(T1) who reported 0 partners

f0 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` == 0 &
          SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ")

# Propotion of female at baseline(T1) who reported 1 partners

f1 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` == 1 &
        SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ")

# Propotion of female at baseline(T1) who reported 2 or more partners
f2 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` >= 2 &
        SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Female  ")

# Propotion of male at baseline(T1) who reported 0 partners

m0 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` == 0 &
        SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ")

# Propotion of male at baseline(T1) who reported 1 partners

m1 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` == 1 &
        SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ")

# Propotion of male at baseline(T1) who reported 2 or more partners

m2 <- sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$`Sexprt Num` >= 2 &
        SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ", na.rm = T) /
      sum(SAMPLE_PREVALENCE_2017_03_18_05_08_09$Gender == "Male    ")

# we create a matrix to summarize this data for the proportion of male and female
# who reported 0,1 or 2 or more partners

sexpartners <- matrix(c(f0,f1,f2,m0,m1,m2),ncol=3,byrow=TRUE)
colnames(sexpartners) <- c("0 partners","1 partner",">=2 partners")
rownames(sexpartners) <- c("Female", "male")

# Read the data

SAMPLE_INCIDENCE_2017_03_18_05_08_18 <- 
  read_excel("~/Downloads/SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx")

# coerce the Sexprt Num T2 variable to type numeric

SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` <- 
  as.numeric(as.character(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2`))

# Propotion of female at T2 who reported 0 partners

f02 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` == 0 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE)

# Propotion of female at T2 who reported 1 partners

f12 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` == 1 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE)

# Propotion of female at T2 who reported 2 or more partners

f22 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` >= 2 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Female  ", na.rm = TRUE)

# Propotion of male at T2 who reported 0 partners

m02 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` == 0 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE)

# Propotion of male at T2 who reported 1 partners

m12 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` == 1 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE)

# Propotion of male at T2 who reported 2 partners

m22 <- sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$`Sexprt Num T2` >= 2 &
             SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE) /
  sum(SAMPLE_INCIDENCE_2017_03_18_05_08_18$Gender == "Male    ", na.rm = TRUE)

# we create a matrix to summarize this data for the proportion of male and female
# who reported 0,1 or 2 or more partners

sexpartnersT2 <- matrix(c(f02,f12,f22,m02,m12,m22),ncol=3,byrow=TRUE)
colnames(sexpartnersT2) <- c("0 partners","1 partner",">=2 partners")
rownames(sexpartnersT2) <- c("Female", "male")
