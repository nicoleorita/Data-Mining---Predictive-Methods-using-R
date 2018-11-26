setwd('C:\\Users\\Nicole Rita\\Documents\\NOVA IMS\\2nd Semester\\Data Mining 2\\Work Project')
getwd()

library(xlsx) #Provide R functions to read/write/format Excel 2007 and Excel 97/2000/XP/2003 file formats.
library(dplyr) #A fast, consistent tool for working with data frame like objects, both in memory and out of memory.
library(plyr)
library(stringr)
library(mice) # Multiple imputation using Fully Conditional Specification (FCS) implemented by the MICE algorithm
library(VIM) #New tools for the visualization of missing and/or imputed values are introduced, which can be used 
#for exploring the data and the structure of the missing and/or imputed values.
library(caret) #Misc functions for training and plotting classification and regression models
#package forecast is required to evaluate performance
library(forecast)
require(ggplot2)
require(ggthemes)
library(extrafont)
library(randomForest)
library(plyr)
library(readxl)
library(lubridate)



churn_data <- read.csv('C:\\Users\\Nicole Rita\\Documents\\NOVA IMS\\2nd Semester\\Data Mining 2\\Work Project\\Initial Datasets\\ChurnIndicator.csv')
colnames(churn_data)[1] <- 'Employee ID'

HR_data <- read.csv('C:\\Users\\Nicole Rita\\Documents\\NOVA IMS\\2nd Semester\\Data Mining 2\\Work Project\\Initial Datasets\\HumanResourcesEvaluation.csv')
colnames(HR_data)[1] <- 'Employee ID'



historical_data <- read_excel('C:\\Users\\Nicole Rita\\Documents\\NOVA IMS\\2nd Semester\\Data Mining 2\\Work Project\\Initial Datasets\\HistoricalData.xlsx', 1)
colnames(historical_data)[1] <- 'Employee ID'

#read.table
satisfaction_data <- read.table('C:\\Users\\Nicole Rita\\Documents\\NOVA IMS\\2nd Semester\\Data Mining 2\\Work Project\\Initial Datasets\\SatisfactionSurvey.txt', sep = '\t', header = TRUE)
colnames(satisfaction_data)[1] <- 'Employee ID'


#########################################################################################################
#                                         DATA CLEANING                                                 #
#########################################################################################################

#check and remove duplicated rows
satisfaction_data[duplicated(satisfaction_data),]
satisfaction_data <- satisfaction_data[!duplicated(satisfaction_data),]


#join HR_data, Historical_data, churn_data, satisfacton_data
historical_hr_churn_satisfaction <- cbind(historical_data, HR_data, churn_data, satisfaction_data)

#remove duplicated columns -> Employee ID
historical_hr_churn_satisfaction <- historical_hr_churn_satisfaction[, !duplicated(colnames(historical_hr_churn_satisfaction))]

colnames(historical_hr_churn_satisfaction)[4] <- 'MaritalStatus'


#-------------------------------------------------------------------------------------------------------#
#--------------------------------------------DEALING WITH DATES-------------------------------------------#
#-------------------------------------------------------------------------------------------------------#

historical_hr_churn_satisfaction$BirthDate
class(historical_hr_churn_satisfaction$BirthDate)


sapply(historical_hr_churn_satisfaction$BirthDate, class)

dates = historical_hr_churn_satisfaction$BirthDate



for(i in 1:length(dates)){
  date = dates[i]
  #print(nchar(date))
  if(nchar(date) >=9){
    date = as.Date(date, "%d-%m-%Y")
    #print(date)
  }else if(nchar(date) <= 5){
    date = as.numeric(date)
    date = as.Date(date, origin="1899-12-30")
    dates[i] = as.character(date)
    date = as.Date(date, "%d-%m-%Y")
  }
  dates[i] = as.character(date)
}
print(dates)

#Store cleaned dates in BirthDate column
historical_hr_churn_satisfaction$BirthDate <- dates

#Check if there are missing values in BirthDate
sum(is.na(historical_hr_churn_satisfaction$BirthDate))

#remove objects that we are not going to use anymore - in first case
rm('historical_data', 'HR_data', 'churn_data', 'satisfaction_data')

#check the class for all variables
sapply(historical_hr_churn_satisfaction, class)

#check the structure
str(historical_hr_churn_satisfaction)

#--------------------------------------------- 1 - save dtaframe & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")
#-----------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------#
#------------------------------------------TYPE OF VARIABLE MANIPULATION--------------------------------#
#-------------------------------------------------------------------------------------------------------#
#VARIABLES - from 'character' to factor


head(historical_hr_churn_satisfaction$Gender)
class(historical_hr_churn_satisfaction$Gender)
historical_hr_churn_satisfaction$Gender <- as.factor(historical_hr_churn_satisfaction$Gender)
levels(historical_hr_churn_satisfaction$Gender)

class(historical_hr_churn_satisfaction$MaritalStatus)
historical_hr_churn_satisfaction$MaritalStatus <- as.factor(historical_hr_churn_satisfaction$MaritalStatus)
levels(historical_hr_churn_satisfaction$MaritalStatus)

class(historical_hr_churn_satisfaction$JobType)
historical_hr_churn_satisfaction$JobType <- as.factor(historical_hr_churn_satisfaction$JobType)
levels(historical_hr_churn_satisfaction$JobType)

class(historical_hr_churn_satisfaction$JobRole)
historical_hr_churn_satisfaction$JobRole <- as.factor(historical_hr_churn_satisfaction$JobRole)
levels(historical_hr_churn_satisfaction$JobRole)

class(historical_hr_churn_satisfaction$Education)
historical_hr_churn_satisfaction$Education <- as.factor(historical_hr_churn_satisfaction$Education)
levels(historical_hr_churn_satisfaction$Education)

class(historical_hr_churn_satisfaction$EducationArea)
historical_hr_churn_satisfaction$EducationArea <- as.factor(historical_hr_churn_satisfaction$EducationArea)
levels(historical_hr_churn_satisfaction$EducationArea)

# genders to lower case
historical_hr_churn_satisfaction$Gender <- tolower(historical_hr_churn_satisfaction$Gender)

#Department names
historical_hr_churn_satisfaction$DepartmentName <- gsub('(.*, )|(\\..*)', '', historical_hr_churn_satisfaction$Department)
#Show title counts by gender
table(historical_hr_churn_satisfaction$Gender, historical_hr_churn_satisfaction$DepartmentName)

#Change variable values names to -> {'HUman Resources', 'IT', 'Marketing' }
historical_hr_churn_satisfaction$DepartmentName[historical_hr_churn_satisfaction$DepartmentName == 'Commercial'] <- 'Marketing' 
historical_hr_churn_satisfaction$DepartmentName[historical_hr_churn_satisfaction$DepartmentName == 'HR' ] <- 'Human Resources'
historical_hr_churn_satisfaction$DepartmentName[historical_hr_churn_satisfaction$DepartmentName == 'Information Technologies'] <- 'IT' 
levels(historical_hr_churn_satisfaction$DepartmentName)
class(historical_hr_churn_satisfaction$DepartmentName)
historical_hr_churn_satisfaction$DepartmentName <- as.character(historical_hr_churn_satisfaction$DepartmentName)
#--------------------------------------------- 2 - save dtaframe & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------MISSING VALUES----------------------------------------#
#-------------------------------------------------------------------------------------------------------#

#CHECK FOR MISSING VALUES

#check missing values for each column
colSums(is.na(historical_hr_churn_satisfaction))

#check which rows ('ID') have missing values
historical_hr_churn_satisfaction[!complete.cases(historical_hr_churn_satisfaction), 'Employee ID']

#-------------------------------------------------------------------------------------------------------#
#HANDLING MISSING DATA

# Creating a new dataset without missing data
noMissing <- na.omit(historical_hr_churn_satisfaction)

#create a copy of the raw_data dataset in order to test the several options to handle missing values
test_miss <- historical_hr_churn_satisfaction


# 1.1 a) - Fill missing values with mode in categorical/logical values - MARITAL STATUS
test_miss <- historical_hr_churn_satisfaction
mode_testMissMaritaStatus <- names(sort(table(test_miss$MaritalStatus)))[length(sort(table(test_miss$MaritalStatus)))]
test_miss$MaritalStatus[is.na(test_miss$MaritalStatus)] <- mode_testMissMaritaStatus
class(test_miss$MaritalStatus)

#1.1 b) - Check missing values on MaritalStatus. Should now be = 0 
colSums(is.na(test_miss))

historical_hr_churn_satisfaction <- test_miss
colSums(is.na(historical_hr_churn_satisfaction))


# 1.2 a) - Fill missing values with mode in categorical/logical values - BIRTH DATE
test_miss <- historical_hr_churn_satisfaction
mode_testMissBirthDate <- names(sort(table(test_miss$BirthDate)))[length(sort(table(test_miss$BirthDate)))]
test_miss$BirthDate[is.na(test_miss$BirthDate)] <- mode_testMissBirthDate
class(test_miss$BirthDate)

#1.2 b) - Check missing values on MaritalStatus. Should now be = 0 
colSums(is.na(test_miss))

historical_hr_churn_satisfaction <- test_miss
colSums(is.na(historical_hr_churn_satisfaction))


#--------------------------------------------- 4 -SAVE DATA FRAME & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------CHECKING OUTLIERS-------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#Create a boxplot
boxplot(historical_hr_churn_satisfaction$MonthlyIncome, horizontal = TRUE)
class(historical_hr_churn_satisfaction$MonthlyIncome)
historical_hr_churn_satisfaction$MonthlyIncome = as.numeric(historical_hr_churn_satisfaction$MonthlyIncome)
# For categorical variable

historical_hr_churn_satisfaction$MaritalStatus<- as.character(historical_hr_churn_satisfaction$MaritalStatus)
class(historical_hr_churn_satisfaction$MaritalStatus)
boxplot( Age~MaritalStatus, data=historical_hr_churn_satisfaction, main="Marital Status versus Age", xlab="Marital Status", ylab="Age")  # clear pattern is noticeable.
boxplot(MonthlyIncome ~ JobDedication, data=historical_hr_churn_satisfaction, main="Job Dedication versus Monthly Income",  xlab="Job Dedication", ylab="Monthly Income")  # this may not be significant, as day of week variable is a subset of the month var.

class(historical_hr_churn_satisfaction$Churn)
historical_hr_churn_satisfaction$Churn = as.character(historical_hr_churn_satisfaction$Churn)
boxplot(MonthlyIncome ~ Churn, data=historical_hr_churn_satisfaction, main="Churn versus Monthly Income",  xlab="Churn", ylab="Monthly Income")  # this may not be significant, as day of week variable is a subset of the month var.
boxplot(JobDedication ~ Churn, data=historical_hr_churn_satisfaction, main="Churn versus Monthly Income",  xlab="Churn", ylab="Job Dedication")  # this may not be significant, as day of week variable is a subset of the month var.
boxplot(RoleSatisfaction ~ Churn, data=historical_hr_churn_satisfaction, main="Churn versus Monthly Income",  xlab="Churn", ylab="Role Satisfaction")  # this may not be significant, as day of week variable is a subset of the month var.
#boxplot( ~ Churn, data=historical_hr_churn_satisfaction, main="Churn versus Monthly Income",  xlab="Churn", ylab="Role Satisfaction")  # this may not be significant, as day of week variable is a subset of the month var.

plot(historical_hr_churn_satisfaction$Age, historical_hr_churn_satisfaction$MonthlyIncome, xlim=c(0,100), ylim=c(0,4000), main="With Outliers", xlab="Age", ylab="Monthly Income", pch="*", col="red", cex = 2)
abline(lm(MonthlyIncome ~ Age, data = historical_hr_churn_satisfaction), col="blue", lwd=3, lty=2)
#tenure company vs Income
plot(historical_hr_churn_satisfaction$TenureCompany, historical_hr_churn_satisfaction$MonthlyIncome, xlim=c(0,50), ylim=c(0,4000), main="With Outliers", xlab="Tenure Company", ylab="MonthlyIncome", pch="*", col="red", cex = 2)
abline(lm(MonthlyIncome ~ TenureCompany, data = historical_hr_churn_satisfaction), col="blue", lwd=3, lty=2)

hist(historical_hr_churn_satisfaction$TenureWorking)

#----------------------------------------HANDLING THE OUTLIERS WITH CAPPING---------------------------------------------
boxplot(historical_hr_churn_satisfaction$MonthlyIncome, main = "Monthly Income Boxplot with Outliers",xlab = "Income",horizontal = TRUE,col = c("#638A8D"), frame = F)
hist(historical_hr_churn_satisfaction$MonthlyIncome, main="Distribution of Income", xlab="", col = c("#638A8D"))

x <- historical_hr_churn_satisfaction$MonthlyIncome
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

hist(x, main="Distribution of Income (with Capping approach)", xlab="", col = c("#93C1C6"))
boxplot(x, horizontal = TRUE)
boxplot(x, main = "Monthly Income Boxplot without Outliers",xlab = "Income",horizontal = TRUE,col = c("#93C1C6"), frame = F)

boxplot(historical_hr_churn_satisfaction$TenureWorking)
hist(historical_hr_churn_satisfaction$TenureWorking)
historical_hr_churn_satisfaction$MonthlyIncome <- x

#--------------------------------------------- 4 -SAVE DATA FRAME & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")


#-------------------------------------------------------------------------------------------------------#
#--------------------------------------CONVERTING vars to 1/0-------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#AfterHours
class(historical_hr_churn_satisfaction$AfterHours)
historical_hr_churn_satisfaction$NewAfterHours <- historical_hr_churn_satisfaction$AfterHours 
historical_hr_churn_satisfaction$NewAfterHours <- as.character(historical_hr_churn_satisfaction$NewAfterHours)
historical_hr_churn_satisfaction$NewAfterHours[historical_hr_churn_satisfaction$NewAfterHours == 'Yes'] <- 1
historical_hr_churn_satisfaction$NewAfterHours[historical_hr_churn_satisfaction$NewAfterHours == 'No'] <- 0
historical_hr_churn_satisfaction$NewAfterHours <- as.factor(historical_hr_churn_satisfaction$NewAfterHours)
levels(historical_hr_churn_satisfaction$NewAfterHours)

#Gender
class(historical_hr_churn_satisfaction$Gender)
historical_hr_churn_satisfaction$NewGender <- historical_hr_churn_satisfaction$Gender 
historical_hr_churn_satisfaction$NewGender[historical_hr_churn_satisfaction$NewGender == 'female'] <- 1
historical_hr_churn_satisfaction$NewGender[historical_hr_churn_satisfaction$NewGender == 'male'] <- 0
historical_hr_churn_satisfaction$NewGender <- as.factor(historical_hr_churn_satisfaction$NewGender)
levels(historical_hr_churn_satisfaction$NewGender)

#Churn
class(historical_hr_churn_satisfaction$Churn)
historical_hr_churn_satisfaction$Churn <- as.character(historical_hr_churn_satisfaction$Churn)
historical_hr_churn_satisfaction$NewChurn <- historical_hr_churn_satisfaction$Churn 
historical_hr_churn_satisfaction$NewChurn <- as.character(historical_hr_churn_satisfaction$NewChurn)
historical_hr_churn_satisfaction$NewChurn[historical_hr_churn_satisfaction$NewChurn == 'Yes'] <- 1
historical_hr_churn_satisfaction$NewChurn[historical_hr_churn_satisfaction$NewChurn == 'No'] <- 0
historical_hr_churn_satisfaction$NewChurn <- as.factor(historical_hr_churn_satisfaction$NewChurn)
levels(historical_hr_churn_satisfaction$NewChurn)

#After hours
class(historical_hr_churn_satisfaction$AfterHours)
historical_hr_churn_satisfaction$AfterHours <- as.character(historical_hr_churn_satisfaction$AfterHours)
historical_hr_churn_satisfaction$NewAfterHours <- historical_hr_churn_satisfaction$AfterHours 
historical_hr_churn_satisfaction$NewAfterHours <- as.character(historical_hr_churn_satisfaction$NewAfterHours)
historical_hr_churn_satisfaction$NewAfterHours[historical_hr_churn_satisfaction$NewAfterHours == 'Yes'] <- 1
historical_hr_churn_satisfaction$NewAfterHours[historical_hr_churn_satisfaction$NewAfterHours == 'No'] <- 0
historical_hr_churn_satisfaction$NewAfterHours <- as.factor(historical_hr_churn_satisfaction$NewAfterHours)
levels(historical_hr_churn_satisfaction$NewAfterHours)


#Balance Work Life
class(historical_hr_churn_satisfaction$BalanceWork.Life)
historical_hr_churn_satisfaction$BalanceWork.Life <- as.character(historical_hr_churn_satisfaction$BalanceWork.Life)
historical_hr_churn_satisfaction$NewBalanceWorkLife <- historical_hr_churn_satisfaction$BalanceWork.Life 
historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.character(historical_hr_churn_satisfaction$NewBalanceWorkLife)
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == 'Bad'] <- 1
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == 'Medium'] <- 2
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == 'Good'] <- 3
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == 'Great'] <- 4

historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.factor(historical_hr_churn_satisfaction$NewBalanceWorkLife)
levels(historical_hr_churn_satisfaction$NewBalanceWorkLife)

#Role Satisfaction binary
class(historical_hr_churn_satisfaction$NewRoleSatisfaction)
historical_hr_churn_satisfaction$NewRoleSatisfaction <- historical_hr_churn_satisfaction$RoleSatisfaction 
historical_hr_churn_satisfaction$NewRoleSatisfaction <- as.character(historical_hr_churn_satisfaction$NewRoleSatisfaction)

historical_hr_churn_satisfaction$NewRoleSatisfaction[historical_hr_churn_satisfaction$NewRoleSatisfaction == '1'] <- 0
historical_hr_churn_satisfaction$NewRoleSatisfaction[historical_hr_churn_satisfaction$NewRoleSatisfaction == '2'] <- 0
historical_hr_churn_satisfaction$NewRoleSatisfaction[historical_hr_churn_satisfaction$NewRoleSatisfaction == '3'] <- 1
historical_hr_churn_satisfaction$NewRoleSatisfaction[historical_hr_churn_satisfaction$NewRoleSatisfaction == '4'] <- 1
historical_hr_churn_satisfaction$NewRoleSatisfaction <- as.factor(historical_hr_churn_satisfaction$NewRoleSatisfaction)
historical_hr_churn_satisfaction$NewRoleSatisfaction <- as.numeric(as.character(historical_hr_churn_satisfaction$NewRoleSatisfaction))
levels(historical_hr_churn_satisfaction$NewRoleSatisfaction)

#NewFacilitiesSatisfaction Satisfaction binary
class(historical_hr_churn_satisfaction$NewFacilitiesSatisfaction)
historical_hr_churn_satisfaction$RoleSatisfaction <- as.character(historical_hr_churn_satisfaction$FacilitiesSatisfaction)
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction <- historical_hr_churn_satisfaction$FacilitiesSatisfaction 
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction <- as.character(historical_hr_churn_satisfaction$NewFacilitiesSatisfaction)
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction[historical_hr_churn_satisfaction$NewFacilitiesSatisfaction == '1'] <- 0
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction[historical_hr_churn_satisfaction$NewFacilitiesSatisfaction == '2'] <- 0
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction[historical_hr_churn_satisfaction$NewFacilitiesSatisfaction == '3'] <- 1
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction[historical_hr_churn_satisfaction$NewFacilitiesSatisfaction == '4'] <- 1
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction <- as.factor(historical_hr_churn_satisfaction$NewFacilitiesSatisfaction)
levels(historical_hr_churn_satisfaction$NewFacilitiesSatisfaction)
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction <- as.numeric(as.character(historical_hr_churn_satisfaction$NewFacilitiesSatisfaction))


#New hierarchy satisfaction Satisfaction binary
class(historical_hr_churn_satisfaction$NewHierarchySatisfaction)
historical_hr_churn_satisfaction$NewHierarchySatisfaction <- historical_hr_churn_satisfaction$HierarchySatisfaction 
historical_hr_churn_satisfaction$NewHierarchySatisfaction <- as.character(historical_hr_churn_satisfaction$NewHierarchySatisfaction)
historical_hr_churn_satisfaction$NewHierarchySatisfaction[historical_hr_churn_satisfaction$NewHierarchySatisfaction == '1'] <- 0
historical_hr_churn_satisfaction$NewHierarchySatisfaction[historical_hr_churn_satisfaction$NewHierarchySatisfaction == '2'] <- 0
historical_hr_churn_satisfaction$NewHierarchySatisfaction[historical_hr_churn_satisfaction$NewHierarchySatisfaction == '3'] <- 1
historical_hr_churn_satisfaction$NewHierarchySatisfaction[historical_hr_churn_satisfaction$NewHierarchySatisfaction == '4'] <- 1
historical_hr_churn_satisfaction$NewHierarchySatisfaction <- as.factor(historical_hr_churn_satisfaction$NewHierarchySatisfaction)
levels(historical_hr_churn_satisfaction$NewHierarchySatisfaction)
historical_hr_churn_satisfaction$NewHierarchySatisfaction <- as.numeric(as.character(historical_hr_churn_satisfaction$NewHierarchySatisfaction))


#Balance worklife binary
class(historical_hr_churn_satisfaction$NewBalanceWorkLife)
historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.character(historical_hr_churn_satisfaction$NewBalanceWorkLife)
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == '1'] <- 0
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == '2'] <- 0
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == '3'] <- 1
historical_hr_churn_satisfaction$NewBalanceWorkLife[historical_hr_churn_satisfaction$NewBalanceWorkLife == '4'] <- 1
historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.factor(historical_hr_churn_satisfaction$NewBalanceWorkLife)
levels(historical_hr_churn_satisfaction$NewBalanceWorkLife)
historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.numeric(as.character(historical_hr_churn_satisfaction$NewBalanceWorkLife))

#New Job dedication
class(historical_hr_churn_satisfaction$JobDedication)
historical_hr_churn_satisfaction$NewJobDedication <- historical_hr_churn_satisfaction$JobDedication
historical_hr_churn_satisfaction$NewJobDedication <- as.character(historical_hr_churn_satisfaction$NewJobDedication)
historical_hr_churn_satisfaction$NewJobDedication[historical_hr_churn_satisfaction$NewJobDedication == '1'] <- 0
historical_hr_churn_satisfaction$NewJobDedication[historical_hr_churn_satisfaction$NewJobDedication == '2'] <- 0
historical_hr_churn_satisfaction$NewJobDedication[historical_hr_churn_satisfaction$NewJobDedication == '3'] <- 1
historical_hr_churn_satisfaction$NewJobDedication[historical_hr_churn_satisfaction$NewJobDedication == '4'] <- 1
historical_hr_churn_satisfaction$NewJobDedication <- as.factor(historical_hr_churn_satisfaction$NewJobDedication)
levels(historical_hr_churn_satisfaction$NewJobDedication)
historical_hr_churn_satisfaction$NewJobDedication <- as.numeric(as.character(historical_hr_churn_satisfaction$NewJobDedication))

#New Job performance
class(historical_hr_churn_satisfaction$NewJobPerformance)
historical_hr_churn_satisfaction$NewJobPerformance <- historical_hr_churn_satisfaction$JobPerformance
historical_hr_churn_satisfaction$NewJobPerformance <- as.character(historical_hr_churn_satisfaction$NewJobPerformance)
historical_hr_churn_satisfaction$NewJobPerformance[historical_hr_churn_satisfaction$NewJobPerformance == '1'] <- 0
historical_hr_churn_satisfaction$NewJobPerformance[historical_hr_churn_satisfaction$NewJobPerformance == '2'] <- 0
historical_hr_churn_satisfaction$NewJobPerformance[historical_hr_churn_satisfaction$NewJobPerformance == '3'] <- 1
historical_hr_churn_satisfaction$NewJobPerformance[historical_hr_churn_satisfaction$NewJobPerformance == '4'] <- 1
historical_hr_churn_satisfaction$NewJobPerformance <- as.factor(historical_hr_churn_satisfaction$NewJobPerformance)
levels(historical_hr_churn_satisfaction$NewJobPerformance)
historical_hr_churn_satisfaction$NewJobPerformance <- as.numeric(as.character(historical_hr_churn_satisfaction$NewJobPerformance))

#New Education
class(historical_hr_churn_satisfaction$Education)
historical_hr_churn_satisfaction$NewEducation <- historical_hr_churn_satisfaction$Education
historical_hr_churn_satisfaction$NewEducation <- as.character(historical_hr_churn_satisfaction$NewEducation)
historical_hr_churn_satisfaction$NewEducation[historical_hr_churn_satisfaction$NewEducation == 'College'] <- 0
historical_hr_churn_satisfaction$NewEducation[historical_hr_churn_satisfaction$NewEducation == 'Associate Degree'] <- 0
historical_hr_churn_satisfaction$NewEducation[historical_hr_churn_satisfaction$NewEducation == 'Bachelor Degree'] <- 1
historical_hr_churn_satisfaction$NewEducation[historical_hr_churn_satisfaction$NewEducation == 'Masters Degree'] <- 1
historical_hr_churn_satisfaction$NewEducation[historical_hr_churn_satisfaction$NewEducation == 'PhD'] <- 1
historical_hr_churn_satisfaction$NewEducation <- as.factor(historical_hr_churn_satisfaction$NewEducation)
levels(historical_hr_churn_satisfaction$NewEducation)
historical_hr_churn_satisfaction$NewEducation <- as.numeric(as.character(historical_hr_churn_satisfaction$NewEducation))


historical_hr_churn_satisfaction$NewBalanceWorkLife <- as.factor(historical_hr_churn_satisfaction$NewBalanceWorkLife)
levels(historical_hr_churn_satisfaction$NewBalanceWorkLife)

#--------------------------------------------- 4 -SAVE DATA FRAME & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")

#-------------------------------------------------------------------------------------------------------#
#------------------------------------------CREATE NEW VARIABLES-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#-CREATE NEW COLUMN - AGE

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  period$year
}

birthDates = historical_hr_churn_satisfaction$BirthDate

for (i in 1:length(birthDates)){
  new_age = calc_age(birthDates[i])
  #print(new_age)
  birthDates[i] = new_age
}

historical_hr_churn_satisfaction$Age <- birthDates
#CHECK NUMBER OF OCCURENCES PER AGE
a <- table(historical_hr_churn_satisfaction$Age)
print(a)


str(historical_hr_churn_satisfaction)
historical_hr_churn_satisfaction$NewFacilitiesSatisfaction<- as.numeric(as.character( historical_hr_churn_satisfaction$NewFacilitiesSatisfaction))

#total satisfaction
attach(historical_hr_churn_satisfaction)
historical_hr_churn_satisfaction$totalSatisfaction = historical_hr_churn_satisfaction$NewFacilitiesSatisfaction + historical_hr_churn_satisfaction$NewRoleSatisfaction+ historical_hr_churn_satisfaction$NewHierarchySatisfaction + historical_hr_churn_satisfaction$NewBalanceWorkLife

#total job rewarding
historical_hr_churn_satisfaction$totalJobRewarding = historical_hr_churn_satisfaction$NewJobDedication + historical_hr_churn_satisfaction$NewJobPerformance

#-----------------------------------------------------------------------------------------------------
#COUNTS

count(historical_hr_churn_satisfaction, 'Gender')
count(historical_hr_churn_satisfaction, 'EducationArea')

#--------------------------------------------- 3 - save dtaframe & LOAD
save(historical_hr_churn_satisfaction,file="historical_hr_churn_satisfaction.Rda")
load("historical_hr_churn_satisfaction.Rda")

#-------------------------------------------------------------------------------------------------------#
#----------------------------------VARIABLE IMPORTANCE------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#Random Forest 

#all_numerical_vars
randForTrans <- all_numerical_vars[, 1:29]
randforlabelTrans <- as.factor(all_numerical_vars$NewChurn)
set.seed(1367)
Variable_Importance_trans <- randomForest(x = randForTrans, y = randforlabelTrans, importance = TRUE, ntree = 1000)
Variable_Importance_trans
varImpPlot(Variable_Importance_trans) #Shows the predictive importance of each variable

#-------------------------------BORUTA 
library(Boruta)
#MIXED DF
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(NewChurn ~ ., data=na.omit(mixed), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------CORRELATION MATRIX-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#

all_numerical_vars$RoleSatisfaction<-as.numeric(as.character(all_numerical_vars$RoleSatisfaction))
all_numerical_vars$NewAfterHours<-as.numeric(as.character(all_numerical_vars$NewAfterHours))
all_numerical_vars$NewGender<-as.numeric(as.character(all_numerical_vars$NewGender))
all_numerical_vars$Age<-as.numeric(as.character(all_numerical_vars$Age))
all_numerical_vars$NewChurn<-as.numeric(as.character(all_numerical_vars$NewChurn))

corrMatrix <- best_vars_df
corrMatrix$NewChurn <- NULL
corrMatrix$NewAfterHours<-as.numeric(as.character(corrMatrix$NewAfterHours))
str(corrMatrix)
# correlation Matrix
cormat <- round(cor(corrMatrix),2)
head(cormat)

library(corrplot)
corrplot(cormat, method="color")



