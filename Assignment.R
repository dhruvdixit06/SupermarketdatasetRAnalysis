################################################################################
#STATISTICAL ANALYSIS WITH R AIT Assignment
#Date:14th Sep  2022
################################################################################

#Package Installation
install.packages(c('psych', 'MASS', 'GGally', 'VGAM', 'ggplot2', 'truncreg', 'boot',
                   'foreign', 'Hmisc', 'aod', 'margins', 'reshape2'))

#Calling the libraries downloaded
lapply(c('psych', 'MASS', 'GGally', 'VGAM', 'ggplot2', 'truncreg', 'boot',
         'foreign', 'Hmisc', 'aod', 'margins', 'reshape2'), library, character.only = TRUE)

################################################################################
#Task 1 - DESCRIPTIVE STATISTICS
################################################################################

#setting up your working directory
setwd("C:/Users/dhruv/Downloads/R Assignment")

#Uploading the first dataset to RStudio
dataset <- read.csv(file.choose(), header = TRUE)
attach(dataset)


#summary of the descriptive statistics
summary(dataset)

#seeing the column names of dataset
names(dataset) 

#summary of the descriptive statistics
describe(Unit.price)
describe(Quantity) 
describe(Tax.5.)
describe(Total)
describe(Rating)
describe(cogs)

#Checking Missing Values
sum(is.na(dataset))

#Calculating Average Unit price for each category
library(dplyr)
prodVar <- group_by(dataset,Product.line)
summarise(prodVar, avgUnit = mean(Unit.price))

################################################################################
## Task 2 - DATA VISUALIZATION
################################################################################

#CORRELATION ANALYSIS

#Pearson's Product Moment Correlation for Unit.price and Quantity
cor.test(Unit.price,Quantity,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is not less than 0.05. Hence, Unit.price and Quantity are not correlated and null hypothesis is not rejected.

#Pearson's Product Moment Correlation for Unit.price and Tax.5.
cor.test(Unit.price,Tax.5.,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is less than 0.05. Hence, Unit.price and Tax.5. are highly correlated and null hypothesis is rejected.

#Pearson's Product Moment Correlation for Unit.price and Total
cor.test(Unit.price,Total,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is less than 0.05. Hence, Unit.price and Total are highly correlated and null hypothesis is rejected.

#Pearson's Product Moment Correlation for Unit.price and cogs
cor.test(Unit.price,cogs,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is less than 0.05. Hence, Unit.price and cogs are highly correlated and null hypothesis is rejected.

#Pearson's Product Moment Correlation for Unit.price and Rating
cor.test(Unit.price,Rating,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is not less than 0.05. Hence, Unit.price and Rating are not correlated and null hypothesis is not rejected.

#Pearson's Product Moment Correlation for Quantity and Total
cor.test(Quantity,Total,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is less than 0.05. Hence, Quantity and Total are highly correlated and null hypothesis is rejected.

#Pearson's Product Moment Correlation for Quantity and Rating
cor.test(Quantity,Rating,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is not less than 0.05. Hence, Quantity and Rating are not correlated and null hypothesis is rejected.

#Pearson's Product Moment Correlation for Quantity and Gross Income
cor.test(Quantity,gross.income,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#Pearson's Product Moment Correlation for Total and Gross Income
cor.test(Total,gross.income,alternative = c("two.sided"), method = c("pearson"),
         exact = NULL, conf.level= 0.95, continuity = FALSE)

#It is observed clearly that p-value is less than 0.05. Hence, Quantity and Gross Income are highly correlated and null hypothesis is rejected.

#Result: Now we know that Unit.price is highly correlated with Tax.5., Total and cogs while Quantity is highly correlated with Total

#Scatterplot Data Visualization

#->Unit.price and Tax.5.

plot(Unit.price,Tax.5., xlab = 'Tax.5.', ylab = 'Unit.price')

#Scatterplot with linear regression line plotting
abline(lm(Tax.5. ~ Unit.price), col = "blue")

#We can conclude that Unit.price and Tax.5. have positive correlation

#->Unit.price and Total

plot(Unit.price,Total, xlab = 'Total', ylab = 'Unit.price')

#Scatterplot with linear regression line plotting
abline(lm(Total ~ Unit.price), col = "blue")

#We can conclude that Unit.price and Total have positive correlation

#->Unit.price and cogs

plot(Unit.price,cogs, xlab = 'cogs', ylab = 'Unit.price')

#Scatterplot with linear regression line plotting
abline(lm(cogs ~ Unit.price), col = "blue")

#We can conclude that Unit.price and Total have positive correlation

#->Quantity and Total

plot(Quantity,Total, xlab = 'Total', ylab = 'Quantity')

#Scatterplot with linear regression line plotting
abline(lm(Total ~ Quantity), col = "blue")


#We can conclude that Unit.price and Total have positive correlation

#->Total and Gross Income

plot(Total,gross.income, xlab = 'Gross Income', ylab = 'Total')

#Scatterplot with linear regression line plotting
abline(lm(gross.income ~ Total), col = "blue")



################################################################################
# Task 3 - REGRESSION ANALYSIS (LINEAR REGRESSION, MULTIPLE LINEAR REGRESSION, NON-LINEAR
#REGRESSION)
################################################################################

#Simple linear regression

#->Unit.price and Tax.5.
simpreg <- lm(Unit.price ~ Tax.5.)
plot(Unit.price ~ Tax.5.)
abline(simpreg, col = "blue")
summary(simpreg)

#->Unit.price and Total
simpreg <- lm(Unit.price ~ Total)
plot(Unit.price ~ Total)
abline(simpreg, col = "blue")
summary(simpreg)

#->Unit.price and cogs
simpreg <- lm(Unit.price ~ cogs)
plot(Unit.price ~ cogs)
abline(simpreg, col = "blue")
summary(simpreg)

#->Quantity and Total
simpreg <- lm(Quantity ~ Total)
plot(Quantity ~ Total)
abline(simpreg, col = "blue")
summary(simpreg)

#->Total and Gross Income
simpreg <- lm(Total ~ gross.income)
plot(Total ~ gross.income)
abline(simpreg, col = "blue")
summary(simpreg)


#multiple linear regression

#->Unit.price and Tax.5. with Quantity
reg <- lm(Unit.price ~ Tax.5. + Quantity)
summary(reg)

#->Unit.price and Tax.5. with Total and cogs
reg <- lm(Unit.price ~ Tax.5. + Total + cogs)
summary(reg)

#->Quantity and cogs with Total and rating
reg <- lm(Quantity ~ cogs + Total + Rating)
summary(reg)

################################################################################
#All tasks done
#Created by :- Dhruv Dixit - United College of Engineering and Management 
################################################################################

      )

