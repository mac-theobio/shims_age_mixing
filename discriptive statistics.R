# General Discriptive Statistics ------------------------------------------

# load libraries

library(tidyverse)
library(psych) # computing icc

## load data and functions

load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.Rdata") # full dataset
# load("/Users/emanuel/Google Drive/SHIMS/SHIMS Baseline data/DT.Agemix.men.excluded.final.Rdata") # full dataset
theme_set(theme_bw()) # set global plot theme 

# Baseline Characteristics of male respondents aged 18-49 ---------------------------------
# with at least one sex partner in the past 6 months

summary(DT.Agemix.men)
mean(DT.Agemix.men$Current.age); sd(DT.Agemix.men$Current.age)
median(DT.Agemix.men$Current.age); IQR(DT.Agemix.men$Current.age)
median(DT.Agemix.men$Age.sex.debut, na.rm = T); IQR(DT.Agemix.men$Age.sex.debut, na.rm = T)
(table(DT.Agemix.men$Education.level)/5788)*100

# without a sexvpartner in the past 6 months
# summary(DT.Agemix.men.excluded.final)
# mean(DT.Agemix.men.excluded.final$Current.age); sd(DT.Agemix.men.excluded.final$Current.age)
# median(DT.Agemix.men.excluded.final$Current.age); IQR(DT.Agemix.men.excluded.final$Current.age)
# median(DT.Agemix.men.excluded.final$Age.sex.debut, na.rm = T); IQR(DT.Agemix.men.excluded.final$Age.sex.debut, na.rm = T)
# (table(DT.Agemix.men.excluded.final$Education.level)/2173)*100

# the distribution of the age of the male participants

ggplot(data = DT.Agemix.men, aes(Participant.age + 12)) +
  geom_histogram(bins = 30) +
  xlab("Participant age at relationship formation") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

ggsave("participanthist.png", width = 6.25, height = 5.25,dpi = 600)

# measures of central tendency and dispersion
summary(DT.Agemix.men$Participant.age+12)
IQR(DT.Agemix.men$Participant.age+12)

# the distribution of the age of the female participants

ggplot(data = DT.Agemix.men, aes(Partner.age)) +
  geom_histogram(bins = 30, na.rm = T) +
  xlab("Partner age at relationship formation") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# we see some people reported very large partner ages (even close to 100). These are definitely outliers
# A boxplot below depicts this

ggplot(data = DT.Agemix.men, aes(x=factor(0),y=Partner.age)) +
  geom_boxplot(na.rm = T) +
  xlab(NULL) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip()

# measures of central tendency and dispersion
summary(DT.Agemix.men$Partner.age)
IQR(DT.Agemix.men$Partner.age)


# # look at the distribution of ages of people who did not have at least one relationship
load("/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.men.excluded.Rdata") # full dataset

# the distribution of the age of the male participants

ggplot(data = DT.Agemix.men.excluded.final, aes(Current.age)) +
  geom_histogram(bins = 30, na.rm = T, col = "gray66", fill = "gray68") +
  xlab("Age") +
  ylab("Relationships") +
  theme(axis.text.x = element_text(size=14), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=14)) +
  theme(text=element_text( size=14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5.5))

mean(DT.Agemix.men.excluded.final$Current.age) #23.52508

summary(DT.Agemix.men)

# number of relationships for different variables
count(DT.Agemix.men, Condom.frequency)
count(DT.Agemix.men, Sex.frequency)
count(DT.Agemix.men, Partner.type)
count(DT.Agemix.men, Education.level)

# number of participants for different variables
count(count(DT.Agemix.men, Uid), n) # with 1, 2, 3 partners
count(count(DT.Agemix.men, Uid, Condom.frequency), Condom.frequency)
count(count(DT.Agemix.men, Uid, Sex.frequency), Sex.frequency)
count(count(DT.Agemix.men, Uid, Partner.type), Partner.type)
count(count(DT.Agemix.men, Uid, Education.level), Education.level)

# subset all relationships where partner was casual
# an alternative to line 40
One.ormore.casual <- DT.Agemix.men %>% filter(Partner.type == "casual partner") %>% group_by(Uid) %>% 
  summarise(n_distinct(Uid)) #unique Uid

# subset all relationships where age difference is >= 5 years
PAG.5ormore <- DT.Agemix.men %>% filter(abs(Age.difference) >= 5) %>% group_by(Uid) %>% 
  summarise(n_distinct(Uid)) #unique Uid

# subset all relationships where condom use is always
Consistent.condomuse <- DT.Agemix.men %>% filter(Condom.frequency == "always") %>% count(Uid)
Rels.each.participant <- count(DT.Agemix.men, Uid)
XXX <- merge(Consistent.condomuse,Rels.each.participant, by= "Uid") %>%  # merge 2 tables
  filter(n.x == n.y) # filter

# age differences and relationships 

x <- filter(DT.Agemix.men, Age.difference < 0);length(unique(x$Uid))
x_1year <- filter(x, Age.difference < -1);length(unique(x_1year$Uid))
x_2year <- filter(x, Age.difference <= -2);length(unique(x_2year$Uid))

xx <- filter(DT.Agemix.men, Age.difference >= 0)

xx1 <- filter(DT.Agemix.men, Age.difference < -5)
xx2 <- filter(DT.Agemix.men, Age.difference >= -5 & Age.difference < 0)
xx3 <- filter(DT.Agemix.men, Age.difference >= 0 & Age.difference < 5)
xx4 <- filter(DT.Agemix.men, Age.difference >= 5 & Age.difference < 10)
xx5 <- filter(DT.Agemix.men, Age.difference >= 10)


# total number of men
length(unique(DT.Agemix.men$Uid))

length(unique(filter(DT.Agemix.men, Age.difference < 0)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap <0

length(unique(filter(DT.Agemix.men, Age.difference < -5)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap <0
length(unique(filter(DT.Agemix.men, Age.difference >= -5 & Age.difference < 0)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap <0
length(unique(filter(DT.Agemix.men, Age.difference >= 0 & Age.difference < 5)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap <0
length(unique(filter(DT.Agemix.men, Age.difference >= 5 & Age.difference < 10)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap <0
length(unique(filter(DT.Agemix.men, Age.difference >= 10)$Uid))/ length(unique(DT.Agemix.men$Uid)) # age gap >=10

t.test(DT.Agemix.men$Age.difference) #CI

ggplot(DT.Agemix.men, aes(No.partners)) +
  geom_histogram()

table(DT.Agemix.men$No.partners)


# remove extreme outliers (major = 3*IQR)
H = 3*IQR(DT.Agemix.men$Partner.age, na.rm = T)
U = quantile(DT.Agemix.men$Partner.age, probs = 0.75, na.rm = T) + H
L = quantile(DT.Agemix.men$Partner.age, probs = 0.25, na.rm = T) - H

DT.Agemix.men <- filter(DT.Agemix.men, Partner.age >= L & Partner.age <= U)
summary(DT.Agemix.men$Partner.age)

ggplot(data = DT.Agemix.men, aes(Partner.age)) +
  geom_histogram(bins = 30, na.rm = T) +
  xlab("Partner age at relationship formation") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

ggsave("partnerhist.png", width = 6.25, height = 5.25,dpi = 600)