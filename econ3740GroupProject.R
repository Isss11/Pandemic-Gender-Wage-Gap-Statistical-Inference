#set-up
rm(list=ls())
setwd("C:/Users/isss1/Documents/University/Semester 4/ECON-3740/Group Project/Data and Code/")

#loading libraries
library(haven)
library(tidyverse)
library(stargazer)

# loading up the two data sets
dec2019 <- read_stata("C:/Users/isss1/Documents/University/Semester 4/ECON-3740/Group Project/Data and Code/LFS-71M0001-E-2019-December/LFS-71M0001-E-2019-December_F1.dta")
dec2022 <- read_stata("C:/Users/isss1/Documents/University/Semester 4/ECON-3740/Group Project/Data and Code/LFS-71M0001-E-2022-December/LFS-71M0001-E-2022-December_F1.dta")

# making wage adjustments
# creating a log wage
dec2019 <- dec2019 %>% mutate(lwage=log(HRLYEARN))
dec2022 <- dec2022 %>% mutate(lwage=log(HRLYEARN))
dec2019 <- dec2019 %>% mutate(sex = SEX - 1)
dec2022 <- dec2022 %>% mutate(sex = SEX - 1)

# filter out all rows with missing values for lwage
dec2019 <- dec2019 %>% filter(!is.na(lwage))
dec2022 <- dec2022 %>% filter(!is.na(lwage))

# creating a data set where we can create a covidTime variable to run an unrestricted model 
# creating a covid indicator to be able to know what is post-covid and pre-covid when we created combinedData
dec2019 <- dec2019 %>% mutate(covid=0)
dec2022 <- dec2022 %>% mutate(covid=1)
combinedData <- bind_rows(dec2019, dec2022)


# filtering out data where we cannot get a weekly wage using usual hours worked, so we can compute weekly wage (usual hours is non-zero for wage reporters)
# implies that everyone in our data set must have worked
combinedData <- combinedData %>% mutate(weeklywage = UHRSMAIN * HRLYEARN)
combinedData <- combinedData %>% mutate(lweeklywage = log(weeklywage))

# getting means and proportions of variables for proposal
summary(dec2019$HRLYEARN)
summary(dec2022$HRLYEARN)

# getting specific means for female wages and men by creating new variables
dec2019Men <- dec2019 %>% filter(sex==0)
dec2022Men <- dec2022 %>% filter(sex==0)
dec2019Women <- dec2019 %>% filter(sex==1)
dec2022Women <- dec2022 %>% filter(sex==1)

summary(dec2019Men$HRLYEARN)
summary(dec2022Men$HRLYEARN)
summary(dec2019Women$HRLYEARN)
summary(dec2022Women$HRLYEARN)

summary(dec2019$lwage)
summary(dec2022$lwage)

table(dec2019$SEX)
table(dec2022$SEX)

table(dec2019$AGE_12)
table(dec2022$AGE_12)

table(dec2019$MARSTAT)
table(dec2022$MARSTAT)

table(dec2019$EDUC)
table(dec2022$EDUC)

table(dec2019$NAICS_21)
table(dec2022$NAICS_21)

table(dec2019$NOC_10)
table(dec2022$NOC_10)

summary(dec2019$AHRSMAIN)
summary(dec2022$AHRSMAIN)

# more detailed regression
r2022multiple <- lm(lwage ~ sex + factor(AGE_12) + factor(MARSTAT)
                 +factor(EDUC) + factor(COWMAIN) + factor(NAICS_21) + factor(NOC_10) + AHRSMAIN, data=dec2022)

stargazer(r2022multiple,type="text")

r2019multiple <- lm(lwage ~ factor(SEX) + factor(AGE_12) + factor(MARSTAT)
                  +factor(EDUC) + factor(COWMAIN) + factor(NAICS_21) + factor(NOC_10) + AHRSMAIN, data=dec2019)

stargazer(r2019multiple, type="text")

# doing unrestricted test to see relative effect of COVID on wages
rSensitivitySex <- lm(lwage ~ sex + factor(AGE_12) + factor(MARSTAT)
                    +factor(EDUC) + factor(COWMAIN) + factor(NAICS_21) + factor(NOC_10) + AHRSMAIN + sex*covid + covid, data=combinedData)

stargazer(rSensitivitySex, type="text")

# sensitivity test to see if weekly wage differences are different in the
rSensSexWeeklyWage <- lm(lweeklywage ~ sex + factor(AGE_12) + factor(MARSTAT)
                         +factor(EDUC) + factor(COWMAIN) + factor(NAICS_21) + factor(NOC_10) + AHRSMAIN + sex:covid + covid, data=combinedData)

stargazer(rSensSexWeeklyWage, type="text")

# sensitivity test for 2022 data set, taking out industry
rSensIndustry2019 <- lm(lwage ~ sex + factor(AGE_12) + factor(MARSTAT)
                        +factor(EDUC) + factor(COWMAIN) + factor(NOC_10) + AHRSMAIN, data=dec2019)

rSensIndustry2022 <- lm(lwage ~ sex + factor(AGE_12) + factor(MARSTAT)
                    +factor(EDUC) + factor(COWMAIN) + factor(NOC_10) + AHRSMAIN, data=dec2022)

stargazer(rSensIndustry2022, type="text")
stargazer(rSensIndustry2019, type="text")