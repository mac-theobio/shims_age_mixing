# General Discriptive Statistics ------------------------------------------

# load libraries

library(tidyverse)

## load data and functions

load("/Users/emanuel/Dropbox/SHIMS Baseline data/DT.Agemix.men.Rdata") # full dataset
theme_set(theme_bw()) # set global plot theme 


# Baseline Characteristics of male respondents aged 18-49 ---------------------------------

(table(DT.Agemix.men$Condom.frequency) / 5634) * 100
(table(DT.Agemix.men$Sex.frequency) / 5548) * 100
(table(DT.Agemix.men$Partner.type) / 5684)*100

summary(DT.Agemix.men$Relationship.dur)


# the distribution of the age of the male participants

ggplot(data = DT.Agemix.men, aes(Participant.age + 15)) +
  geom_histogram(bins = 30) +
  xlab("Participant age at relationship formation") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  theme(text=element_text( size=11)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

ggsave("participanthist.png", width = 6.25, height = 5.25,dpi = 600)

# measures of central tendency and dispersion
summary(DT.Agemix.men$Participant.age+15)
IQR(DT.Agemix.men$Participant.age)

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

# measures of central tendency and dispersion
summary(DT.Agemix.men$Partner.age)
IQR(DT.Agemix.men$Partner.age)
