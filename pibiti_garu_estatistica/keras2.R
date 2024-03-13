setwd("/home/joao/Documents/BIG/v2/")
library(plyr)
library(keras)

#testing
setwd("/home/joao/Documents/national-health-and-nutrition-examination-survey/")
demographics <- read.csv("demographic.csv")
head(demographics)
summary(demographics$SEQN)
length(demographics$SEQN)
summary(demographics$RIAGENDR)

examination <- read.csv("examination.csv")
summary(examination$SEQN)
summary(examination$BMXWT)
summary(examination$BMXHT)
summary(examination$BPXDI4)
summary(examination$BPXDI4[examination$BPXDI4 > 0 & !is.na(examination$BPXDI4)])

#demographic:
#SEQN
#RIAGENDR

#examination:
#SEQN
#BMXWT

studentdata <- read.csv("../student_data.csv", sep = ";")
head(studentdata)
is.numeric(studentdata$age)
is.numeric(studentdata$entry_year)

studentdata$age <- as.character(studentdata$age)
studentdata$age <- gsub("[^0-9.-]", "", studentdata$age)
studentdata$age <- as.numeric(studentdata$age)
summary(studentdata)
studentdata$age[studentdata$age == 225] <- 22
table(studentdata$entry_year)
studentdata$entry_year <- -(studentdata$entry_year - 2016)
studentdata$entry_year <- as.factor(studentdata$entry_year)
ggplot(data=studentdata, aes(x=entry_year, y=age)) + geom_boxplot()
