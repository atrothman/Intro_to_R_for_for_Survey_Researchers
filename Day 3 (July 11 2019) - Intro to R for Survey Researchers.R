rm(list=ls())

######################
######################
## R course - Day 3 ##
######################
######################

######################
## Homework recap ####
######################
## 1.	Using the mock pizza data, graph:
##   a.	A bar chart of the scores to "Number of times eaten pizza in the past 7 days"
##   b.	Scatterplots with a trendline
##      i.	Var1 = Number of times eaten pizza in the past 7 days
##     ii.	Var2 = Overall satisfaction with preferred pizza brand
##   c.	Scatterplots with trendline and using different colors for different genders
##      i.	Var1 = Number of times eaten pizza in the past 7 days
##     ii.	Var2 = Overall satisfaction with preferred pizza brand
##    iii.	Category = gender

## libraries 
library(ggplot2)

## load "pizza" dataset
pizza <- read.csv("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\mock_pizza_data.csv", header = TRUE)
head(pizza)

##1a
ggplot(pizza, aes(num7)) + geom_bar() + ggtitle("Bar Graph") +  xlab("Number of times eaten pizza in the past 7 days") + ylab("number of observations")

##1b 
ggplot(pizza, aes(x=num7, y=brand_sat)) + geom_point()


##1c
ggplot(pizza, aes(x=num7, y=brand_sat, color=as.factor(gender))) + geom_point()



#########################
#########################
## Day 3 Code ###########
#########################
#########################

## library
library(MASS)
library(dplyr)

## Load "UScereal" dataset
data(UScereal)
??UScereal
head(UScereal)
table(UScereal$mfr, exclude=NULL)
table(UScereal$vitamins, exclude=NULL)
psych::describe(UScereal)

###########################
## Correlation Analysis: ##
###########################
ggplot(UScereal, aes(fat, sodium)) + geom_point() + geom_smooth(method="lm") + xlab("fat") + ylab("sodium")

## "cor" function
cor(UScereal$fat, UScereal$sodium, method="pearson")
cor(UScereal$fat, UScereal$sodium, method="spearman")

## "cor.test" function
cor.test(UScereal$fat, UScereal$sodium, alternative=c("two.sided"), conf.level=0.95, method=c("pearson"))
cor.test(UScereal$fat, UScereal$sodium, alternative=c("two.sided"), conf.level=0.95, method=c("spearman"))

## look at the "rcorr" function
library(Hmisc)
UScereal_sub <- as.matrix(dplyr::select(UScereal, fat, sodium, carbo, sugars))
rcorr(UScereal_sub, type=c("pearson"))
rcorr(UScereal_sub, type=c("spearman"))


######################################
## regressions #######################
######################################

## univariate
## Y = sodium
## X = fat
summary(lm(sodium ~ fat, data=UScereal))


## multivariate 1
## Y = sodium
## X1 = fat
## X2 = fibre
summary(lm(sodium ~ fat + fibre, data=UScereal))


## multivariate 2
## Y = sodium
## X1 = fat
## X2 = fibre
## X3 = shelf
summary(lm(sodium ~ poly(fat,2) + fibre + as.factor(shelf), data=UScereal))



####################
## LR test #########
####################

## univariate
## Y = sodium
## X = fat
summary(lm(sodium ~ fat, data=UScereal))
model_1 <- lm(sodium ~ fat, data=UScereal)

## multivariate 1
## Y = sodium
## X1 = fat
## X2 = fibre
summary(lm(sodium ~ fat + fibre + protein, data=UScereal))
model_2 <- lm(sodium ~ fat + fibre + protein, data=UScereal)

library(lmtest)
lrtest(model_1, model_2)

## AIC and BIC
AIC(model_1)
BIC(model_1)




#################################### 
## scatterplot to assess variance ##
####################################
ggplot(UScereal, aes(fat, sodium)) + geom_point() + geom_smooth(method="lm") + xlab("fat") + ylab("sodium")
model_1 <- lm(sodium ~ fat, data=UScereal)
predicted_values <- model_1$fitted.values
residuals <- model_1$residuals

plot(predicted_values, residuals)


###########################
## t-Test #################
###########################

## dichtomize "shelf" variable into just "shelf 1" or "not shelf 1"
table(UScereal$shelf)
UScereal$shelf_1 <- 0
UScereal$shelf_1[UScereal$shelf==1] <- 1
table(UScereal$shelf_1)
table(UScereal$shelf, UScereal$shelf_1)


## run the t-test
var.test(fat ~ shelf_1, data=UScereal, alternative = "two.sided")
t.test(fat ~ shelf_1, data=UScereal, paried=FALSE)
t.test(fat ~ shelf_1, data=UScereal, var.equal = TRUE, paried=FALSE)


##############################
## crosstabs #################
##############################
hist(UScereal$protein)
UScereal$protein_descrete <- 0
UScereal$protein_descrete[UScereal$protein>3] <- 1
table(UScereal$protein_descrete)

mytable <- xtabs(~ protein_descrete + shelf_1, data=UScereal)
table1 <- chisq.test(mytable, correct = FALSE)

table1$expected

fisher.test(mytable)




###############################
## Logistic regression ########
###############################
summary(glm(protein_descrete ~ carbo, data=UScereal, family="binomial"))












