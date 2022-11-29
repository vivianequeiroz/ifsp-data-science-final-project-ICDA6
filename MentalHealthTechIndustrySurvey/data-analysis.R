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

#### Commit 1

## Questions with less responses
answers %>% separate_rows(`QuestionID`,2,sep = ',') %>%
  group_by(`QuestionID`) %>%
  summarise(N=n()) %>% arrange((N)) %>%
  print(n=20)
## Output
# QuestionID           N
# <int> <int>
#   1         92    1260
# 2           93    1260
# 3           94    1260
# 4           95    1260
# 5           96    1260
# 6           97    1260
# 7           98    1260
# 8           99    1260
# 9           100   1260
# 10          101   1260
# 11          102   1260
# 12          103   1260
# 13          104   1433
# 14          105   1433
# 15          106   1433
# 16          107   1433
# 17          108   1433
# 18          109   1433
# 19          110   1433
# 20          111   1433


## Questions with more responses
answers %>% separate_rows(`QuestionID`,2,sep = ',') %>%
  group_by(`QuestionID`) %>%
  summarise(N=n()) %>% arrange(desc(N)) %>%
  print(n=20)

counts = table(answers$QuestionID)
barplot(counts, main="Barplot das perguntas respondidas em todas as pesquisas")

## Years with more participants
hist(answers$SurveyID, main="Histograma da frequência de participantes por ano", xlab = "Anos", ylab="Frequência", col = "brown")

#### Commit 2

## Exploring collumns based on the 12 more answered question from all years 
## Main goal: more consistence when developing the model

## Gender - Question 2

#Filtering by question 2 - What is your gender?
allGenders <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 2)

#Transformando em data frame
#allGendersDataFrame = as.data.frame(allGenders)

#Validating unique values 

unique(allGenders$AnswerText)
#Output: 101 in total, indicates the necessity of standardization 

#Validating mode since its a categorical variable
find_mode <- function (x) {
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

find_mode(allGenders$AnswerText)

#Using find values to substitute in every occurence that does not match Male or Female
standardizedGenders <-  allGenders %>%
  mutate(AnswerText = if_else(AnswerText != "Male" & AnswerText != "Female", 
                              find_mode(allGenders$AnswerText), 
                              AnswerText))
  
#Confirming there are only female and male in dataset 

genders <- unique(standardizedGenders$AnswerText)
genders
#Output: > [1] "Female" "Male" 

#Filtro com expressão regular para separar generos
# allGendersDataFrame$AnswerText <- gsub("(?i)F|(?i)Female", "F", allGendersDataFrame$AnswerText)
# allGendersDataFrame$AnswerText <- gsub("(?i)M|(?i)Male", "M", allGendersDataFrame$AnswerText)
# allGendersDataFrame$AnswerText
# allGendersDataFrame$AnswerText <- gsub("(?i)T|(?i)Trans", "T", allGendersDataFrame$AnswerText)
# allGendersDataFrame$AnswerText <- gsub("(?i)N|(?i)NonBinary", "N", allGendersDataFrame$AnswerText)
# 
# allGendersDataFrame$AnswerText <- gsub(".*/(?:F|f)/.*", "F", allGendersDataFrame$AnswerText, perl = TRUE)
# allGendersDataFrame$AnswerText <- gsub(".*/(?:M|m)/.*", "M", allGendersDataFrame$AnswerText, perl = TRUE)
# allGendersDataFrame$AnswerText <- gsub(".*^((?!\bMale\b|\bFemale\b).)*$.*", "M", allGendersDataFrame$AnswerText, perl = TRUE)
# 
# unique(allGendersDataFrame$AnswerText)
# 
# unique(allGendersDataFrame$AnswerText)

#Dataframe for female values
allFemale <- standardizedGenders %>% 
  group_by(AnswerText) %>% 
  filter(AnswerText == "Female")

hist(allFemale$SurveyID, main="Histograma de respostas do sexo feminino", xlab="Anos")

#Dataframe for male values
allMale <- standardizedGenders %>% 
  group_by(AnswerText) %>% 
  filter(AnswerText == "Male")

hist(allMale$SurveyID, main="Histograma de respostas do sexo masculino", xlab="Anos")

# #Criando data frame para o genero trans
# allTransgender <- allGendersDataFrame %>% 
#   group_by(AnswerText) %>% 
#   filter(stringr::str_detect(AnswerText, 'T'))
# 
# allTransgenderDataFrame <- as.data.frame(allTransgender)
# 

# #Criando data frame para o genero nao binario
# allNonBinary <- allGendersDataFrame %>% 
#   group_by(AnswerText) %>% 
#   filter(stringr::str_detect(AnswerText, 'N'))
# 
# allNonBinaryDataFrame <- as.data.frame(allNonBinary)
# 
# hist(allNonBinaryDataFrame$SurveyID, main="Histograma de respostas do gênero não-binário", xlab="Anos")
# 
# #Criando data frame para os demais generos não mapeados
# allNonMappedGenders <- allGendersDataFrame %>% 
#   filter(!stringr::str_detect(AnswerText, 'T|M|F|N'))
# 
# allNonMappedGendersDataFrame <- as.data.frame(allNonMappedGenders)
# allNonMappedGendersDataFrame
# 
# hist(allNonMappedGendersDataFrame$SurveyID, main="Histograma de respostas do genero não mapeado",xlab="Anos")
# 
# #Total no genero feminino
# table(allFemaleDataFrame$AnswerText)
# 
# #Total no genero masculino
# table(allMaleDataFrame$AnswerText)
# 
# #Total no genero trans
# mutatedAllTransgenderDataFrame <- allTransgenderDataFrame %>% 
#                                   mutate(AnswerText = "T")
# 
# table(mutatedAllTransgenderDataFrame$AnswerText)
# 
# #Total no genero não binário
# mutatedAllNonBinaryDataFrame <- allNonBinaryDataFrame %>% 
#   mutate(AnswerText = "N")
# 
# table(mutatedAllNonBinaryDataFrame$AnswerText)
# 
# #Total nao mapeado
# nonMappedGenderDataFrameSum <- allNonMappedGendersDataFrame %>% 
#   filter(!stringr::str_detect(AnswerText, 'T|M|F|N'))
# 
# nonMappedGenderDataFrameSum
# 
# 
# #Todos os generos do dataset
# allGendersInDataset <- c(table(mutatedAllTransgenderDataFrame$AnswerText), table(mutatedAllNonBinaryDataFrame$AnswerText), table(nonMappedGenderDataFrameSum$AnswerText), table(allFemaleDataFrame$AnswerText),table(allMaleDataFrame$AnswerText)
# )
# nonMappedGenderDataFrameSum
# mutatedAllTransgenderDataFrame
# barplot(allGendersInDataset)


#Validante total amount of male and female each
table(standardizedGenders$AnswerText)
#Output: Female   Male 
#           914   3304 


#Visualizations of proportion of gender per year
  
ggplot(standardizedGenders, aes(x = SurveyID,fill = AnswerText)) +  
  geom_bar(position = "dodge")


ggplot(standardizedGenders, aes(x = SurveyID,fill = AnswerText)) +  
  geom_bar(position = "fill")


#### Commit 3

#### Age - 1

#Filtering by question 1 - What is your age?
allAges <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 1)

#Converting to number
allAges <- type.convert(allAges, as.is = T)
table(allAges$AnswerText)

#Visualizations and summarizations to look for possible outliers 
boxplot(allAges$AnswerText)
min(allAges$AnswerText)
max(allAges$AnswerText)
describe(allAges)

#Showing outliers
ageOutliersValues <- boxplot.stats(allAges$AnswerText)$out 
mtext(paste("Outliers: ", paste(ageOutliersValues, collapse=", ")), cex=0.6)
ageOutliersValues
#Output:  [1] -29  56  60  54 329  55  -1  55  57  58  57  54  62  65  56  57  54  -1   5  56  61  55   8  11  -1  72  56  60  55
#[30]  55  54  56  57  63  55  99  57  55  61  55  54  61  55 323  62  54  55  58   3  54  55  66  59  55  63  55  56  54
#[59]  59  65  63  54  55  54  56  74  57  57  70  63  55  56  56  66  64  54  61  57  54  64  62  60  58  59  67  57  60
#[88]  58  57  56  54  56  55  57  -1  -1  57  55  61  57  61  67  56  54  55  54  57  65  55  61  58  54  64  55  60  55
#[117]   0  54  63  59  59  56  59  55


# Adding NA at outliers because there are categorical answers from this group that will be important for posterior analysis 
allAgesWithNA <- ifelse(!allAges$AnswerText %in% ageOutliersValues, allAges$AnswerText, NA)

#Replacing values in AnswerText collumns
colToReplace <- c("AnswerText")                         
colToReplace

allAgesWithNA <- allAges 
allAgesWithNA[colToReplace] <- sapply(allAgesWithNA[colToReplace], 
                                   function(x) replace(x, x %in% ageOutliersValues, NA))

#Summarizing to validate outliers do not appear anymore
summary(allAgesWithNA)
sapply(allAgesWithNA, function(x) sum(is.na(x)))
#Visualizations without outliers
boxplot(allAgesWithNA$AnswerText)
hist(allAgesWithNA$AnswerText)


#Function to calculate mode without NAs
#UTILS: Can be extract to other commit, its for reuse
calculate_mode_without_na <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

#Calculating median since its a numerical variable
median(allAgesWithNA$AnswerText, na.rm = TRUE)
#Output: 32

# Considering values without outliers, NAs will be substitute to 32 since its the median
allAgesWithNA$AnswerText[is.na(allAgesWithNA$AnswerText)] <- 32
standardizedAges <- as.data.frame(allAgesWithNA)

standardizedAges

#Validating there isn't more NAs
sapply(standardizedAges, function(x) sum(is.na(x)))

standardizedAges
boxplot(standardizedAges$AnswerText, main="Histograma idade dos partipantes de pesquisa", ylab="Idade")
hist(standardizedAges$AnswerText, main="Histograma idade dos partipantes de pesquisa", xlab="Idade", ylab="Frequencia")


ggplot(standardizedGenders, aes(x = SurveyID,fill = AnswerText)) +  
  geom_bar(position = "fill")

# Commit 4


#Renomenado allAges para AllAges a fim de manter o padrão de nome nas colunas
#standardizedAges %>% 
#              rename("Ages" = "allAges")


## Country - 3

#Filtering by question 3 - What country do you live in?
allCountry <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 3)

allCountry

#Checking for NAs
sapply(allCountry, function(x) sum(is.na(x)))

#Create a map-shaped window
#mapDevice('x11')
#join to a coarse resolution map
#spdf <- joinCountryData2Map(allCountry, joinCode="NAME", nameJoinColumn="AnswerText", verbose = TRUE)

#mapCountryData(spdf, nameColumnToPlot="AnswerText", catMethod="fixedWidth", missingCountryCol = 'gray')

# 4214 codes from your data successfully matched countries in the map
# 4 codes from your data failed to match with a country code in the map
# failedCodes failedCountries
# [1,] NA          "Other"        
# [2,] NA          "Other"        
# [3,] NA          "-1"           
# [4,] NA          "-1" 


#Finding mode to substitute those 4 values
calculate_mode_without_na(allCountry$AnswerText)

allCountry$AnswerText

#Country outliers
countryOutliers <- c("Other", "-1", "United States")

allCountry$AnswerText <- sapply(allCountry$AnswerText, 
                                      function(x) replace(x, x %in% countryOutliers, "United States of America"))

table(allCountry$AnswerText)

#Total per country
countryCount <- count(allCountry, AnswerText) 

countryList <- as.list(countryCount$n)
countryList

names(countryList) <- countryCount$AnswerText
countryList

#Answer per country 
allCountry %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))
#with no percentage: mutate(freq = n / sum(n))

#TODO: adjust parameters to improve visualization
# 4218 codes from your data successfully matched countries in the map
# 0 codes from your data failed to match with a country code in the map
# failedCodes failedCountries
# 165 codes from the map weren't represented in your data**

mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(allCountry, joinCode="NAME", nameJoinColumn="AnswerText", verbose = TRUE)

mapCountryData(spdf, nameColumnToPlot="AnswerText", catMethod="categorical", addLegend=FALSE, mapTitle="Países participantes das pesquisas")

# Commit 5


## Family mental history - 6

#Filtering by question 6 - Do you have a family history of mental illness?
familyMentalHealthHistory <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 6)

familyMentalHealthHistory

#Checking for NAs
unique(familyMentalHealthHistory$AnswerText)

#Dataframe for those who HAS a historical mental health issue in family
hasMentalHealthHistoryInFamily <- answers %>% 
  group_by(AnswerText) %>% 
  filter(AnswerText == "Yes")

hasMentalHealthHistoryInFamily
summary(hasMentalHealthHistoryInFamily)

hasNotMentalHealthHistoryInFamily <- answers %>% 
  group_by(AnswerText) %>% 
  filter(AnswerText == "No" | AnswerText == "I don't know")

summary(hasNotMentalHealthHistoryInFamily)


summary(doesNotKnowMentalHealthHistoryInFamily)
#Total AnswerText from all filtered values surpass the total of 4218 UserID
#When joining other dataframes it is important to filter based to UserID!

#Visualization of total by answers
table(familyMentalHealthHistory$AnswerText)


#Validating mode
find_mode(familyMentalHealthHistory$AnswerText)
calculate_mode_without_na(familyMentalHealthHistory$AnswerText)

familyMentalHealthHistory$AnswerText <- gsub("(?i)I don't know|(?i)Yes", "Yes", familyMentalHealthHistory$AnswerText)


#Accounting total per answers in variable
mentalHealthHistoryCount <- count(familyMentalHealthHistory, AnswerText) 

#Transforming to list
mentalHealthHistoryList <- as.list(mentalHealthHistoryCount$n)
mentalHealthHistoryList

#Obtaining collumns names
names(mentalHealthHistoryList) <- mentalHealthHistoryCount$AnswerText
mentalHealthHistoryList
barplot()
#Frequency in family mental history answers
familyMentalHealthHistory %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))
# sem porcentagem: mutate(freq = n / sum(n))

#Answers plot 
barplot(table(familyMentalHealthHistory$AnswerText), main="Histórico familiar de alguma questão relacionada a saúde mental")


#Frequency with exact numbers:
mentalHealthHistoryProportions <- table(familyMentalHealthHistory$AnswerText)
prop.table(mentalHealthHistoryProportions)

#Commit 6


## Had already seek mental health professional - 7

#Filtering by question 7 - Have you ever sought treatment for a mental health disorder from a mental health professional?
hadAlreadySeekMentalHealthAssistance <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 7)

hadAlreadySeekMentalHealthAssistance

#Checking for NAs
sapply(hadAlreadySeekMentalHealthAssistance, function(x) sum(is.na(x)))

unique(hadAlreadySeekMentalHealthAssistance$AnswerText) #true (0) false (1)

#Substitute 1 to Yes and 0 to No
hadAlreadySeekMentalHealthAssistance$AnswerText <- gsub("(?i)0|(?i)Yes", "Yes", hadAlreadySeekMentalHealthAssistance$AnswerText)
hadAlreadySeekMentalHealthAssistance$AnswerText <- gsub("(?i)1|(?i)No", "No", hadAlreadySeekMentalHealthAssistance$AnswerText)

unique(hadAlreadySeekMentalHealthAssistance$AnswerText)
table(hadAlreadySeekMentalHealthAssistance$AnswerText)
#Visualization of data after filtering
barplot(table(hadAlreadySeekMentalHealthAssistance$AnswerText))

#Frequency in seek of mental health professional
hadAlreadySeekMentalHealthAssistance %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))
# with no percentage: mutate(freq = n / sum(n))

hadAlreadySeekMentalHealthAssistance

#Accounting per anwswer
hadAlreadySeekMentalHealthAssistanceCount <- count(hadAlreadySeekMentalHealthAssistance, AnswerText) 

#Transforming in list
hadAlreadySeekMentalHealthAssistanceList <- as.list(hadAlreadySeekMentalHealthAssistanceCount$n)
hadAlreadySeekMentalHealthAssistanceList

#Obtaining collumns names
names(hadAlreadySeekMentalHealthAssistanceList) <- hadAlreadySeekMentalHealthAssistanceCount$AnswerText
hadAlreadySeekMentalHealthAssistanceList

#Commit 7


##Mental Health as part of healthcare coverage at work

#Filtering question 10 - Does your employer provide mental health benefits as part of healthcare coverage?
employerProvideMentalHealthAssistance <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 10)

employerProvideMentalHealthAssistance

unique(employerProvideMentalHealthAssistance$AnswerText)


#Checking for NAs
sapply(employerProvideMentalHealthAssistance, function(x) sum(is.na(x)))


#Output values:
# "Yes"                            "Don't know"                     "No"                            
# "Not eligible for coverage / NA" "-1"                             "I don't know" 

#Aggregating in no, yes and i dont know values
employerProvideMentalHealthAssistance$AnswerText <- gsub("(?i)Not eligible for coverage / NA", "I don't know", employerProvideMentalHealthAssistance$AnswerText)
employerProvideMentalHealthAssistance$AnswerText <- gsub("(?i)-1|(?i)Yes", "Yes", employerProvideMentalHealthAssistance$AnswerText)
employerProvideMentalHealthAssistance$AnswerText <- gsub("(?i)Don't know","I don't know", employerProvideMentalHealthAssistance$AnswerText)
employerProvideMentalHealthAssistance$AnswerText <- gsub("(?i)I I Don't know","I don't know", employerProvideMentalHealthAssistance$AnswerText)
employerProvideMentalHealthAssistance$AnswerText <- gsub("(?i)I don't know","No", employerProvideMentalHealthAssistance$AnswerText)

unique(employerProvideMentalHealthAssistance$AnswerText)
table(employerProvideMentalHealthAssistance$AnswerText)

#Verifying filtered values
unique(employerProvideMentalHealthAssistance$AnswerText)

#Barplot after treatment
barplot(table(employerProvideMentalHealthAssistance$AnswerText), main="Empregador provê algum benefício que inclui saúde mental")
 
#Accounting per answer
employerProvideMentalHealthAssistanceCount <- count(employerProvideMentalHealthAssistance, AnswerText) 

#Transforming in list
employerProvideMentalHealthAssistanceList <- as.list(employerProvideMentalHealthAssistanceCount$n)
employerProvideMentalHealthAssistanceList

#Obtaining collumns name
names(employerProvideMentalHealthAssistanceList) <- employerProvideMentalHealthAssistanceCount$AnswerText
employerProvideMentalHealthAssistanceList


#Frequency in answers
employerProvideMentalHealthAssistance %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))
# with no porcentage: mutate(freq = n / sum(n))

#Commit 8


## Mental health in interview - 12

#Filtering by question 12 - Would you bring up a mental health issue with a potential employer in an interview?
talkAboutMentalHealthAtInterview <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 12)

talkAboutMentalHealthAtInterview

unique(talkAboutMentalHealthAtInterview$AnswerText)
table(talkAboutMentalHealthAtInterview$AnswerText)

#Checking for NAs
sapply(talkAboutMentalHealthAtInterview, function(x) sum(is.na(x)))

#Standardization to answers
talkAboutMentalHealthAtInterview$AnswerText <- gsub("(?i)Maybe", "No", talkAboutMentalHealthAtInterview$AnswerText)
unique(talkAboutMentalHealthAtInterview$AnswerText)

#Visualization of answers
barplot(table(talkAboutMentalHealthAtInterview$AnswerText), main="Abordaria questão de saúde mental pessoal em uma entrevista de emprego")

talkAboutMentalHealthAtInterview %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))


#Commit 10


##### Other relevant answers that can be relevant for other analysys and algorithm training ##### 

## Already diagnosed with a mental health issue - 33

#Filtering by question 33 - Do you currently have a mental health disorder?
hasMentalHealthDisorder <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 33)

hasMentalHealthDisorder
unique(hasMentalHealthDisorder$AnswerText)
#Output: #[1] "No"         "Yes"        "Maybe"      "Possibly"   "Don't Know"

summary(hasMentalHealthDisorder)

#Checking for NAs
sapply(hasMentalHealthDisorder, function(x) sum(is.na(x)))
#Output: AnswerText   SurveyID     UserID QuestionID 
#0          0          0          0 

#Applying standardization to answers 
hasMentalHealthDisorder$AnswerText <- gsub("(?i)Don't know", "I don't know", hasMentalHealthDisorder$AnswerText)
hasMentalHealthDisorder$AnswerText <- gsub("(?i)Possibly", "Maybe", hasMentalHealthDisorder$AnswerText)
hasMentalHealthDisorder$AnswerText <- gsub("(?i)I don't know", "Yes", hasMentalHealthDisorder$AnswerText)
hasMentalHealthDisorder$AnswerText <- gsub("(?i)Maybe", "Yes", hasMentalHealthDisorder$AnswerText)

unique(hasMentalHealthDisorder$AnswerText)
table(hasMentalHealthDisorder$AnswerText)

#Visualizing the final result
barplot(table(hasMentalHealthDisorder$AnswerText), main="Possui algum distúrbio mental")

#Frequency
hasMentalHealthDisorder %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))


# ## Mental health is part of wellness program of employer - 95
# 
# #Filtering by question 95 - Do you think that discussing a mental health disorder with previous employers would have negative consequences?
# hasMentalHealthInWellnessProgram <- answers %>% 
#   group_by(QuestionID) %>% 
#   filter(QuestionID == 95)
# 
# hasMentalHealthInWellnessProgram
# 
# unique(hasMentalHealthInWellnessProgram$AnswerText)
# summary(hasMentalHealthInWellnessProgram)
# 
# #Checking for NAs
# sapply(hasMentalHealthInWellnessProgram, function(x) sum(is.na(x)))
# 
# #Applying standardization to answers 
# hasMentalHealthInWellnessProgram$AnswerText <- gsub("(?i)Don't know", "No", hasMentalHealthInWellnessProgram$AnswerText)
# hasMentalHealthInWellnessProgram$AnswerText <- gsub("(?i)I No", "No", hasMentalHealthInWellnessProgram$AnswerText)
# unique(hasMentalHealthInWellnessProgram$AnswerText)
# table(hasMentalHealthInWellnessProgram$AnswerText)

#Visualizing the final result
barplot(table(hasMentalHealthInWellnessProgram$AnswerText))

#Commit 11



# ##Has already been diagnosed with some mental disorder - 34
# alreadyDiagnosedMentalHealthDisorder <- answers %>% 
#   group_by(QuestionID) %>% 
#   filter(QuestionID == 34)
# 
# alreadyDiagnosedMentalHealthDisorder
# 
# summary(alreadyDiagnosedMentalHealthDisorder)
# 
# unique(alreadyDiagnosedMentalHealthDisorder$AnswerText)
# 
# #Checking for NAs
# sapply(alreadyDiagnosedMentalHealthDisorder, function(x) sum(is.na(x)))
# 
# 
# #Applying standardization to answers 
# alreadyDiagnosedMentalHealthDisorder$AnswerText <- gsub("(?i)-1|(?i)Yes", "Yes", alreadyDiagnosedMentalHealthDisorder$AnswerText)
# 
# barplot(table(alreadyDiagnosedMentalHealthDisorder$AnswerText))

#Commit 12



##Mental health conditions diagnosed - 115 
#Filtering 
diagnosedConditions <- answers %>% 
  group_by(QuestionID) %>% 
  filter(QuestionID == 115)

diagnosedConditions

summary(diagnosedConditions)
unique(diagnosedConditions$AnswerText)
#Output: 34 different answers

#Looking for mode
find_mode(diagnosedConditions$AnswerText) 
#Output: [1] "-1"

#Removing -1 since there no indication of what it means
diagnosedConditions = filter(diagnosedConditions, !(AnswerText %in% "-1"))
find_mode(diagnosedConditions$AnswerText)

#First attempt of visualiztion
barplot(summary(as.factor(diagnosedConditions$AnswerText)))


#Word cloud with most frequent words in the diagnoses answers
diagnosesWords <- as.character(diagnosedConditions$AnswerText)
diagnosesWords <- as.character(diagnosesWords)

diagnosesWordsCorpus <- Corpus(VectorSource(diagnosesWords))  #Corpus

diagnosesWordsCorpus<- diagnosesWordsCorpus %>%
  tm_map(removePunctuation)%>% #eliminate special chars
  tm_map(removeNumbers)%>% #eliminate numbers
  tm_map(stripWhitespace) #eliminate spaces

diagnosesWordsCorpus <- tm_map(diagnosesWordsCorpus, removeWords, c("etc", "-1"))

diagnosesWordsCounts <- as.matrix(TermDocumentMatrix(diagnosesWordsCorpus))
diagnosesWordsFreq <- sort(rowSums(diagnosesWordsCounts), decreasing = TRUE)
head(diagnosesWordsFreq)  #top diagnosesWords


wordcloud(words = names(diagnosesWordsFreq), freq = diagnosesWordsFreq, scale = c(3, 0.5), max.words = 120, 
          random.order = FALSE)

#Frequency
diagnosedConditions %>%
  group_by(AnswerText) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), '%'))



#Commit 13


############### JOIN OF DATAFRAMES - START  ############### 

firstMergeDf <- merge(standardizedGenders,standardizedAges, by = "UserID")
firstMergeDf
colnames(firstMergeDf) <- c("UserID", "Gender", "SurveyYear", "GenderQuestionID", "Age", "SurveyYear", "AgeQuestionID")
firstMergeDf

secondMergeDf <- firstMergeDf[!duplicated(as.list(firstMergeDf))]
secondMergeDf <- merge(firstMergeDf,familyMentalHealthHistory, by = "UserID")
secondMergeDf
colnames(secondMergeDf) <- c("UserID", "Gender", "SurveyYear", "GenderQuestionID", "Age", "SurveyYear", "AgeQuestionID", "MentalHealthHistory", "SurveyYear", "MentalHealthHistoryQuestionID")
secondMergeDf


thirdMergeDf <- secondMergeDf[!duplicated(as.list(secondMergeDf))]
thirdMergeDf
thirdMergeDf <- merge(secondMergeDf,hadAlreadySeekMentalHealthAssistance, by = "UserID")
colnames(thirdMergeDf) <- c("UserID", "Gender", "SurveyYear", "GenderQuestionID", "Age", "SurveyYear", "AgeQuestionID", "MentalHealthHistory", "SurveyYear", "MentalHealthHistoryQuestionID", "AlreadySeekMentalHealthAssistance", "SurveyYear", "AlreadySeekMentalHealthAssistanceQuestionID")
thirdMergeDf

#Removing duplicated data and Question IDs
thirdMergeDf$GenderQuestionID <- NULL
thirdMergeDf$AgeQuestionID <- NULL
thirdMergeDf$MentalHealthHistoryQuestionID <- NULL
thirdMergeDf$AlreadySeekMentalHealthAssistanceQuestionID <- NULL

thirdMergeDf

#Removing duplicated SurveyYear 
thirdMergeDfUnique <- thirdMergeDf[!duplicated(as.list(thirdMergeDf))]
thirdMergeDfUnique

fourthMergeDf <- merge(thirdMergeDfUnique,talkAboutMentalHealthAtInterview, by = "UserID")
colnames(fourthMergeDf) <- c("UserID", "Gender", "SurveyYear", "Age", "FamilyMentalHealthHistory", "AlreadySeekMentalHealthAssistance", "TalkAboutMentalHealthAtInterview", "SurveyYear", "TalkAboutMentalHealthAtInterviewQuestionID")

#Removing duplicated data and Question IDs
fourthMergeDf$TalkAboutMentalHealthAtInterviewQuestionID <- NULL

#Removing duplicated SurveyYear 
fourthMergeDfUnique <- fourthMergeDf[!duplicated(as.list(fourthMergeDf))]
fourthMergeDfUnique

fifthMergeDf <- merge(fourthMergeDfUnique,allCountry, by = "UserID")
fifthMergeDf
colnames(fifthMergeDf) <- c("UserID", "Gender", "SurveyYear", "Age","MentalHealthHistory", "AlreadySeekMentalHealthAssistance", "TalkAboutMentalHealthAtInterview", "Country", "SurveyYear", "CountryQuestionID")
fifthMergeDf

#Removing duplicated data and Question IDs
fifthMergeDf$CountryQuestionID <- NULL

#Removing duplicated SurveyYear 
fifthMergeDfUnique <- fifthMergeDf[!duplicated(as.list(fifthMergeDf))]
fifthMergeDfUnique

sixthMergeDf <- merge(fifthMergeDfUnique,employerProvideMentalHealthAssistance, by = "UserID")
sixthMergeDf
colnames(sixthMergeDf) <- c("UserID", "Gender", "SurveyYear", "Age","MentalHealthHistory", "AlreadySeekMentalHealthAssistance", "TalkAboutMentalHealthAtInterview", "Country", "EmployerProvideMentalHealthAssistance", "SurveyYear", "QuestionID")
sixthMergeDf

#Removing duplicated data and Question IDs
sixthMergeDf$QuestionID <- NULL

#Removing duplicated SurveyYear 
sixthMergeDfUnique <- sixthMergeDf[!duplicated(as.list(sixthMergeDf))]
sixthMergeDfUnique

seventhMergeDf <- merge(sixthMergeDfUnique,hasMentalHealthDisorder, by = "UserID")
seventhMergeDf
colnames(seventhMergeDf) <- c("UserID", "Sex", "SurveyYear", "Age","MentalHealthHistory", "AlreadySeekMentalHealthAssistance", "TalkAboutMentalHealthAtInterview", "Country", "EmployerProvideMentalHealthAssistance", "HasMentalHealthDisorder", "SurveyYear", "QuestionID")

#Removing duplicated data and Question IDs
seventhMergeDf$QuestionID <- NULL

#Removing duplicated SurveyYear 
seventhMergeDfUnique <- seventhMergeDf[!duplicated(as.list(seventhMergeDf))]
seventhMergeDfUnique

mentalHealthInTechOverview <- seventhMergeDfUnique
mentalHealthInTechOverview

###############  JOIN OF DATAFRAMES - END   ############### 

#Commit 14


###############  GENERATING OTHER RELEVANT DATAFRAMES - START   ###############

### PS: these does not contain answers from all participants mapped in the first 12 dataframes created
### It will be used to make validation for the ML model developed but always considering the USERID that exists
### in both dataframes

#Data only from 2016:

diagnosedWithMentalDisorderDf <- merge(alreadyDiagnosedMentalHealthDisorder, diagnosedConditions, by = "UserID")
diagnosedWithMentalDisorderDf
colnames(diagnosedWithMentalDisorderDf) <- c("UserID", "AlreadyDiagnosedMentalHealthDisorder", "SurveyYear", "AlreadyDiagnosedMentalHealthDisorderQuestionID", "DiagnosedConditions", "SurveyYear", "DiagnosedConditionsQuestionID")
diagnosedWithMentalDisorderDf

#Removing duplicated data and Question IDs
diagnosedWithMentalDisorderDf$AlreadyDiagnosedMentalHealthDisorderQuestionID <- NULL
diagnosedWithMentalDisorderDf$DiagnosedConditionsQuestionID <- NULL

#Removing duplicated SurveyYear 
diagnosedWithMentalDisorderDfUnique <- diagnosedWithMentalDisorderDf[!duplicated(as.list(diagnosedWithMentalDisorderDf))]
diagnosedWithMentalDisorderDfUnique


#Data only from 2014:

searchForMentalProfessionalHealthDf <- merge(hadAlreadySeekMentalHealthAssistance, hasMentalHealthInWellnessProgram, by = c("UserID", "SurveyID")) 
searchForMentalProfessionalHealthDf

colnames(searchForMentalProfessionalHealthDf) <- c("UserID", "SurveyYear", "HadAlreadySeekMentalHealthAssistance", "HadAlreadySeekMentalHealthAssistanceQuestionID", "hasMentalHealthInWellnessProgram", "hasMentalHealthInWellnessProgramQuestionID")
searchForMentalProfessionalHealthDf

#Removing duplicated data and Question IDs
searchForMentalProfessionalHealthDf$HadAlreadySeekMentalHealthAssistanceQuestionID <- NULL
searchForMentalProfessionalHealthDf$hasMentalHealthInWellnessProgramQuestionID <- NULL

#Removing duplicated SurveyYear 
searchForMentalProfessionalHealthDfUnique <- searchForMentalProfessionalHealthDf[!duplicated(as.list(searchForMentalProfessionalHealthDf))]
searchForMentalProfessionalHealthDfUnique

summary(searchForMentalProfessionalHealthDfUnique)

###############  GENERATING OTHER RELEVANT DATAFRAMES - END    ###############

#Commit 15


#Should point to a convenient directory at other personal computer
write.csv(mentalHealthInTechOverview,"J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\MentalHealthTechIndustrySurvey\\mental-health-in-tech-overview.csv", row.names = FALSE)

mentalHealthInTechOverview <- read.csv("J:\\Code\\faculdade\\ICD6-ciencia-de-dados\\projeto-final\\MentalHealthInTechIndustry\\MentalHealthTechIndustrySurvey\\mental-health-in-tech-overview.csv", na.string="", stringsAsFactors=T)


sapply(mentalHealthInTechOverview, function(x) sum(is.na(x)))

print(nrow(mentalHealthInTechOverview))
#Output: 2958 dados para treino e validação teste 

head(mentalHealthInTechOverview)


# 
# trainData <- mentalHealthInTechOverview[1:2070,]
# testData <- data.frame(mentalHealthInTechOverview[2071:2958,])


#newTestData <-testData                                # Duplicate test data set
#newTestData$Country[which(!(testData$Country %in% unique(trainData$Country)))] <- NA  # Replace new levels by NA
#newTestData


#Fixing
# rec <- 
#   recipe(AlreadySeekMentalHealthAssistance ~ ., data = trainData) %>% 
#   step_other(Country) %>% # This is where we handle new categories
#   prep()
# 
# new_train <- bake(rec, new_data = trainData)
# new_test  <- bake(rec, new_data = testData)
# 
# # We build the model again with the prepared data
# model <- rpart(
#   formula = AlreadySeekMentalHealthAssistance ~ .,
#   data = new_train
# )

# This works
# predict(model, newdata = new_test)
# 
# model <- glm(AlreadySeekMentalHealthAssistance ~.,family=binomial,data=new_train)
# 
# summary(model)
# 
# anova(model, test="Chisq")
# 
# pR2(model)
# 
# fittedResult <- predict(model,newdata=data.frame(new_test),type='response')
# fittedResult <- ifelse(fittedResult > 0.5,1,0)
# 
# misClasificError <- mean(fittedResult != new_test$AlreadySeekMentalHealthAssistance)
# print(paste('Accuracy',1-misClasificError))
# 
# 
# head(mentalHealthInTechOverview)
# 
# 

sample <- sample(c(TRUE, FALSE), nrow(mentalHealthInTechOverview), replace=TRUE, prob=c(0.7,0.3))
train <- mentalHealthInTechOverview[sample, ]
test <- mentalHealthInTechOverview[!sample, ]

# train <- mentalHealthInTechOverview %>% 
#  dplyr::sample_frac(0.70)
# 
# test  <- dplyr::anti_join(mentalHealthInTechOverview, train, by = 'UserID')

#Training with 70% of data

trainingModel <-  glm(AlreadySeekMentalHealthAssistance ~ Sex + Age + MentalHealthHistory + TalkAboutMentalHealthAtInterview + HasMentalHealthDisorder + Country, data = train, family = binomial(link="logit"))
summary(trainingModel)

glm.probs <- predict(trainingModel,type = "response")
glm.probs[1:10]

glm.pred.traning <- ifelse(glm.probs > 0.5, "Yes", "No")
glm.pred.traning

attach(train)
table(glm.pred.traning, train$AlreadySeekMentalHealthAssistance)

mean(glm.pred.traning == train$AlreadySeekMentalHealthAssistance)


#Testing with 30% of data

model <- glm(AlreadySeekMentalHealthAssistance ~ Sex + Age + MentalHealthHistory + TalkAboutMentalHealthAtInterview + HasMentalHealthDisorder + Country, data = test, family = binomial(link="logit"))
summary(model)


glm.probs <- predict(model,type = "response")
glm.probs[1:10]
summary(model)

glm.pred.test <- ifelse(glm.probs > 0.5, "Yes", "No")

attach(model)
table(glm.pred.test, test$AlreadySeekMentalHealthAssistance)

mean(glm.pred.test == test$AlreadySeekMentalHealthAssistance)

#Checking for multicollinearity

vif(model)
vif(trainingModel)

#UserID   and Survey Year are > 5, should not be considered 


plot(allEffects(model))
plot(allEffects(trainingModel))


test_roc = roc(test$AlreadySeekMentalHealthAssistance ~ glm.probs, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

#############

#split dataset into training and testing set
# set.seed(1)
# sample <- sample(c(TRUE, FALSE), nrow(mentalHealthInTechOverview), replace=TRUE, prob=c(0.7,0.3))
# train <- mentalHealthInTechOverview[sample, ]
# test <- mentalHealthInTechOverview[!sample, ]
# 
# model <-glm(AlreadySeekMentalHealthAssistance  ~ Gender + EmployerProvideMentalHealthAssistance + TalkAboutMentalHealthAtInterview  + MentalHealthHistory, family = binomial(link="logit"), data = train)
# summary(model)
# 
# predicted <- predict(model, newdata = train, type = "response")
# 
# #convert defaults from "Yes" and "No" to 1's and 0's
# 
# #find optimal cutoff probability to use to maximize accuracy
# optimal <- optimalCutoff(test$AlreadySeekMentalHealthAssistance, predicted)[1]
# 
# #create confusion matrix
# confusionMatrix(model, predicted)
# 
# sapply(predicted, levels)
# sapply(train, levels)

# Make training and test set - attempts
# dim(train)
# 
# dim(test)
# 
# glm.fit <- glm(AlreadySeekMentalHealthAssistance ~ EmployerProvideMentalHealthAssistance + MentalHealthHistory,
#                data = train,
#                family = binomial)
# 
# 
# glm.probs <- predict(glm.fit,
#                      newdata = test,
#                      type = "response")
# 
# glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
# 
# table(glm.pred, train)
# 
# 
# mean(glm.pred == test)
# 
# summary(glm.fit)
