install.packages("dlookr")
install.packages("rworldmap")
install.packages("tm")
install.packages("evaluate")
install.packages("wordcloud")
install.packages("ggmosaic")
install.packages("ggpubr")
install.packages("apyramid")
install.packages("pscl")
install.packages("rpart")
install.packages("rsample")
install.packages("recipes")
install.packages("ggeffects")
install.packages("effects")
install.packages('caret')
install.packages('Information')
install.packages('car')
install.packages('pROC')

devtools::install_github("selva86/InformationValue")

library(tm)  
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(dlookr)
library(evaluate)
library(rworldmap)
library(wordcloud)  
library(ggmosaic)
library(ggpubr)
library(apyramid)
library(pscl)
library(rsample)
library(rpart)
library(recipes)
library(ggeffects)
library(effects)
library(caret)
library(InformationValue)
library(pROC)
library(car)

set.seed(64)

### Setting files for analysis
#PS: the path should be change if project is downloaded and doubled slash are required in Windows based systems
#setwd("J:\\IFSP\\ICD6-ciencia-de-dados\\projeto-final-ciencia-de-dados-datasets\\mental-health-in-tech-industry")
#$getwd()

answers <- read.csv("J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\MentalHealthTechIndustrySurvey\\Surveys\\mental-health-answers.csv", na.string="", stringsAsFactors=F)
questions <- read.csv("J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\MentalHealthTechIndustrySurvey\\Surveys\\mental-health-question.csv",  na.string="", stringsAsFactors=F)
survey <- read.csv("J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\MentalHealthTechIndustrySurvey\\Surveys\\mental-health-survey.csv", na.string="", stringsAsFactors=F)

### Checando se existem valores NA - com a execução constatado que não há

sapply(answers, function(x) sum(is.na(x)))
sapply(questions, function(x) sum(is.na(x)))
sapply(survey, function(x) sum(is.na(x)))

### Obtaining head and summary of data

head(answers)
head(questions)
head(survey)

summary(answers)
# Data of interest in answers:
# 4218 of total participants in surveys through years

summary(questions)
# Data of interest in questions:
# Surveys with 118 questions each 

summary(survey)
# Data of interest in survey:
# 2015 has no data
