rm(list=ls())

######################
######################
## R course - Day 2 ##
######################
######################

######################
## Homework recap ####
######################

## load "pizza" dataset
pizza <- read.csv("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\mock_pizza_data.csv", header = TRUE)
head(pizza)

## create dataframe with varaibels for "number of times eaten pizza in past 7 days
## and overall satisfaction with preferred pizza brand
library(dplyr)
pizza_sub <- select(pizza, num7, brand_sat)
head(pizza_sub)

## run descriptive stats on dataframe
library(psych)
psych::describe(pizza_sub)
describe(pizza_sub)

## create pie charts for scores of each variable
pie(table(pizza_sub$num7), main="Number of times eaten pizza in the past 7 days")
pie(table(pizza_sub$brand_sat), main="Overall satisfaction with preferred pizza brand")




######################
## libraries #########
######################
library(ggplot2)
library(MASS)
library(car)
library(pastecs)
library(psych)
library(reshape2)


## Load "UScereal" dataset
data(UScereal)
??UScereal
head(UScereal)
table(UScereal$mfr, exclude=NULL)
table(UScereal$vitamins, exclude=NULL)
psych::describe(UScereal)


############################
### ggplot2 ################
############################

## list of geometric objects
#Geom_bar()
#Geom_point()
#Geom_line()
#Geom_smooth()
#Seom_histogram()
#Geom_boxplot()
#Geom_text()
#Geom_density()
#Geom_errorbar()
#Geom_hline()

## aesthetic
#color
#size
#shape
#Linetype
#Alpha

######################
######################
## Bar Graph #########
######################
######################
## plot a bar graph of counts of observations by manufacturer
ggplot(UScereal, aes(mfr)) + geom_bar()

## change the plot so it's blue
ggplot(UScereal, aes(mfr)) + geom_bar(fill="Blue")

## change the titles and axes names
ggplot(UScereal, aes(mfr)) + geom_bar(fill="Blue") + ggtitle("Bar graph of observations by manufacturer") +  xlab("Manufacturer") + ylab("number of observations")

## change the x-axis labels
ggplot(UScereal, aes(mfr)) + geom_bar(fill="Blue") + ggtitle("Bar graph of observations by manufacturer") +  xlab("Manufacturer") + ylab("number of observations") + 
  scale_x_discrete(labels= c("General Mills", "Kelloggs", "Nabisco", "Post", "Quaker Oats", "Ralston Purina"))

## angle the x-axis labels
ggplot(UScereal, aes(mfr)) + geom_bar(fill="Blue") + ggtitle("Bar graph of observations by manufacturer") +  xlab("Manufacturer") + ylab("number of observations") + 
  scale_x_discrete(labels= c("General Mills", "Kelloggs", "Nabisco", "Post", "Quaker Oats", "Ralston Purina")) + 
  theme(axis.text.x=element_text(color = "black", size=9, angle=30, vjust=0.8, hjust=0.8), plot.title = element_text(hjust = 0.5))


######################
######################
## Scatterplot #######
######################
######################
## scatterplot of fat vs sodium
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point()

## fill in the points as red
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point(color="red")

## scatterplot with colors by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, color=mfr)) + geom_point()

## scatterplot with colors and shape by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, color=mfr, shape=mfr)) + geom_point()

## scatterplot with colors, shape, and size by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, color=mfr, shape=mfr, size=mfr)) + geom_point()

## include connected line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red")

## include smooth trend line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + geom_smooth(color="blue")

## include linear line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + geom_smooth(color="blue") + geom_smooth(color="green", method="lm")

## include polynomial line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + geom_smooth(color="blue") + geom_smooth(color="green", method="lm") + 
  geom_smooth(method="lm", formula= y~poly(x,5), colour="black")

##################
##################
## box plot ######
##################
##################

## box plot of just "fat"
ggplot(UScereal, aes(x="fat", y=fat)) + geom_boxplot()

## box plots of fat by the "manufacturer" categorical variable
ggplot(UScereal, aes(x=mfr, y=fat)) + geom_boxplot()

## have a different color for the outline each manufacturer
ggplot(UScereal, aes(x=mfr, y=fat, color=mfr)) + geom_boxplot()

## have a different color for the outline each manufacturer that is grey scale
ggplot(UScereal, aes(x=mfr, y=fat, color=mfr)) + geom_boxplot() + scale_color_grey()

## have a different fill color for each manufacturer
ggplot(UScereal, aes(x=mfr, y=fat, fill=mfr)) + geom_boxplot()

## flip the axis
ggplot(UScereal, aes(x=mfr, y=fat, fill=mfr)) + geom_boxplot() + coord_flip()

## have box plot by more than one group (manufacturer and vitamins)
ggplot(UScereal, aes(x=mfr, y=fat, fill=vitamins)) + geom_boxplot()


##################
##################
## histogram #####
##################
##################
## histogram of carbohydrates
ggplot(UScereal, aes(carbo)) + geom_histogram()

## histogram of carbohydrates with a larger binwith
ggplot(UScereal, aes(carbo)) + geom_histogram(binwidth = 10)

## histogram of carbohydrates by manufacturer
ggplot(UScereal, aes(carbo, fill=mfr)) + geom_histogram()

## histogram of carbohydrates with a density plot
ggplot(UScereal, aes(carbo)) + geom_histogram(aes(y=..density..)) + geom_density()

## histogram of carbohydrates with a density plot, and mean line
ggplot(UScereal, aes(carbo)) + geom_histogram(aes(y=..density..)) + geom_density() + geom_vline(aes(xintercept=mean(carbo)), color="blue", linetype="dashed", size=1)



###########################
###########################
## Normality Assumptions ##
###########################
###########################
library(ggpubr)

## visually create density plot
ggdensity(UScereal$carbo, main = "Density plot of grams of carbohyrates per portion", xlab = "Grams of carbohyrates per portion")

## look at Q-Q plot
ggqqplot(UScereal$carbo)

## shapiro-wilk's test
shapiro.test(UScereal$carbo)


############################
############################
## Outliers ################
############################
############################
library(outliers)

chisq.out.test(UScereal$carbo)
outlier(UScereal$carbo)
UScereal_sub <- filter(UScereal, carbo<35)

ggdensity(UScereal_sub$carbo, main = "Density plot of grams of carbohyrates per portion", xlab = "Grams of carbohyrates per portion")

##########################
##########################
## data transformations ##
##########################
##########################
UScereal_sub$carbo_sqrt <- sqrt(UScereal_sub$carbo)

ggdensity(UScereal_sub$carbo_sqrt, main = "Density plot of grams of SQRT(carbohyrates per portion)", xlab = "SQRT of grams of carbohyrates per portion")

shapiro.test(UScereal_sub$carbo_sqrt)


########################
## heteroskedacity #####
########################
library(car)
leveneTest(UScereal_sub$carbo, UScereal_sub$mfr, center=mean)



