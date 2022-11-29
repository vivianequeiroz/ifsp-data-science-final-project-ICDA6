install.packages("rworldmap")
install.packages("rstatix")
install.packages("tidyverse")
install.packages("corrr")
install.packages("corrrplot")
install.packages("caret")
install.packages("visreg")
install.packages("performance")
install.packages("car")
install.packages("devtools")
install.packages("ggcorrplot")

library(corrr)
library(rworldmap)
library(tidyverse)
library(tidyr)
library(rstatix)
library(corrplot)
library(caret)
library(visreg)
library(performance)
library(car)
library(devtools)
library(ggcorrplot)

mentalHealthExpenditureSuicideRates <- read.csv("J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\Health Expenditure and Suicide Rates [2000-2019]\\WHO_MHExp_and_Deaths.csv", na.string="", stringsAsFactors=T)

#Overview of dataframe
summary(mentalHealthExpenditureSuicideRates)
head(mentalHealthExpenditureSuicideRates)

#Checking for NAs
apply(mentalHealthExpenditureSuicideRates, 2, function(x) any(is.na(x)))
#Output:
#Country_Name              Year        Population  Deaths_All_Types   Deaths_Suicides     HExp_Pctage_Y 
#FALSE             FALSE             FALSE             FALSE             FALSE             FALSE 
#MHExp_Pctage_2011      Dep_Num_2015      Suicide_p100 
#FALSE             FALSE             FALSE 

#Variables of interest:

#Year: 2014, 2016, 2017, 2018 and 2019
#Deaths_Suicides:Deaths by suicides according to ICD10
#HExp_Pctage_Y - Health Expenditure as % of GDP
#MHExp_Pctage_2011 - Mental Health Expenditure index in 2011
#Dep_Num_2015 - Depression estimate index in 2015
#Suicide_p100 - Number of suicides per 100,000 population

#Removing other collumns
mentalHealthExpenditureSuicideRates$Deaths_All_Types <- NULL

summary(mentalHealthExpenditureSuicideRates)

print(nrow(mentalHealthExpenditureSuicideRates))

#Obtaining years of intereset:
mentalHealthExpenditureSuicideRatesYearsOfInterest <- subset(mentalHealthExpenditureSuicideRates, Year == "2014" | Year == "2016" | Year == "2017" | Year == "2018" | Year == "2019")


mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(mentalHealthExpenditureSuicideRatesYearsOfInterest, joinCode="NAME", nameJoinColumn="Country_Name", verbose = TRUE)

mapCountryData(spdf, nameColumnToPlot="Country_Name", catMethod="categorical", addLegend=FALSE, mapTitle="Países presentes na análise")


#Total per country
mentalHealthExpenditureSuicideRatesCount <- count(mentalHealthExpenditureSuicideRatesYearsOfInterest, Country_Name) 

mentalHealthExpenditureSuicideRatesList <- as.list(mentalHealthExpenditureSuicideRatesCount$n)
mentalHealthExpenditureSuicideRatesList

names(mentalHealthExpenditureSuicideRatesList) <- mentalHealthExpenditureSuicideRatesCount$AnswerText
mentalHealthExpenditureSuicideRatesList

#Frequency per country 
mentalHealthExpenditureSuicideRatesYearsOfInterest %>%
  group_by(Country_Name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))
#with no percentage: mutate(freq = n / sum(n))


head(mentalHealthExpenditureSuicideRatesYearsOfInterest)
#Visualizations
summary(mentalHealthExpenditureSuicideRatesYearsOfInterest$HExp_Pctage_Y)
summary(mentalHealthExpenditureSuicideRatesYearsOfInterest$Dep_Num_2015)
summary(mentalHealthExpenditureSuicideRatesYearsOfInterest$MHExp_Pctage_2011)

#Statistical analysis
mentalHealthExpenditureSuicideRatesYearsOfInterest %>%
  rstatix::get_summary_stats(HExp_Pctage_Y, MHExp_Pctage_2011)


correlationTable <- mentalHealthExpenditureSuicideRates %>% 
  select(Deaths_Suicides, MHExp_Pctage_2011, Suicide_p100) %>%   # keep numeric variables of interest
  correlate()      # create correlation table (using default pearson)

correlationTable    # print


head(mentalHealthExpenditureSuicideRatesYearsOfInterest)
str(mentalHealthExpenditureSuicideRatesYearsOfInterest)

correlations <- cor(mentalHealthExpenditureSuicideRatesYearsOfInterest %>% 
                      select(Deaths_Suicides, MHExp_Pctage_2011, Suicide_p100)[,1:8])
corrplot(correlations, method="circle")


# Variables of interest: Population Deaths_Suicides HExp_Pctage_Y MHExp_Pctage_2011 Dep_Num_2015 Suicide_p100

par(mfrow=c(3,8))
for(i in 3:8) {
  hist(mentalHealthExpenditureSuicideRatesYearsOfInterest[,i], main=names(mentalHealthExpenditureSuicideRatesYearsOfInterest)[i])
}

correlations <- cor(mentalHealthExpenditureSuicideRatesYearsOfInterest[,3:8])
corrplot(correlations, method="circle")

str(mentalHealthExpenditureSuicideRatesYearsOfInterest)

train <- mentalHealthExpenditureSuicideRatesYearsOfInterest %>% 
  dplyr::sample_frac(0.70)

test  <- dplyr::anti_join(mentalHealthExpenditureSuicideRatesYearsOfInterest, train)

str(test)

#Training with 70% of data

trainingModel <- lm(MHExp_Pctage_2011 ~ Dep_Num_2015 + Deaths_Suicides, data = train)
summary(trainingModel)

check_model(trainingModel)
plot(allEffects(trainingModel))

plot(trainingModel)


avPlots(trainingModel)

summary(trainingModel)$r.squared

#Testing with 30% of data

model <- lm(MHExp_Pctage_2011 ~ Dep_Num_2015 + Deaths_Suicides, data = test)
summary(model)

check_model(model)
plot(allEffects(model))

plot(model)


avPlots(model)

summary(model)$r.squared


# pairs(mentalHealthExpenditureSuicideRates, col=mentalHealthExpenditureSuicideRates$Country_Name)
# 
# x <- mentalHealthExpenditureSuicideRates[,3:8]
# y <- mentalHealthExpenditureSuicideRates[,1]
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)