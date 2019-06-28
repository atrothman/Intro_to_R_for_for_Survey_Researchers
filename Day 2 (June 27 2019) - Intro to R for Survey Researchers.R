rm(list=ls())

## load library
library(dplyr)
library(descr)
library(ggplot2)
library(MASS)

######################
######################
## R course - Day 2 ##
######################
######################

######################
## Homework recap ####
######################

## load "pizza" dataset
pizza_df <- read.csv("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\mock_pizza_data.csv")

## create dataframe with variabels for "number of times eaten pizza in past 7 days"
## and "overall satisfaction with preferred pizza brand"
pizza_df_sub <- dplyr::select(pizza_df, num7, brand_sat)

## run descriptive stats on dataframe
descr(pizza_df_sub)
table(pizza_df_sub$num7)

## create pie charts for scores of each variable
pie(table(pizza_df_sub$num7), main="pie chart for num7")
pie(table(pizza_df_sub$brand_sat), main="pie chart for brand_sat")

###############################
###############################

## load USceral dataset
data(UScereal)
head(UScereal)
tail(UScereal)
table(UScereal$mfr, exclude=NULL)
descr(UScereal)

######################
######################
## Bar Graph #########
######################
######################
## plot a bar graph of counts of observations by manufacturer
ggplot(UScereal, aes(mfr)) + geom_bar()

## plot a bar graph of counts of observations by manufacturer, make the bars blue
ggplot(UScereal, aes(mfr)) + geom_bar(fill="blue")

## plot a bar graph of counts of observations by manufacturer, make the bars blue, and add titles and better axes names
ggplot(UScereal, aes(mfr)) + geom_bar(fill="blue") + ggtitle("Bar graph of observations by manufacturer") + xlab("Manufacturer") + 
  ylab("number of observations")

## plot a bar graph of counts of observations by manufacturer, make the bars blue, and add titles and better axes names, add better category names
ggplot(UScereal, aes(mfr)) + geom_bar(fill="blue") + ggtitle("Bar graph of observations by manufacturer") + xlab("Manufacturer") + 
  ylab("number of observations") + scale_x_discrete(labels=c("General Mills", "Kelloggs", "Nabisco", "Post", "Quaker Oats", "Ralston Purina")) +
  theme(axis.text.x=element_text(color = "black", size=9, angle=30, vjust=0.8, hjust=0.8), plot.title = element_text(hjust = 0.5))


######################
######################
## Scatterplot #######
######################
######################
## scatterplot of x=fat vs y=sodium
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point()

## scatterplot of x=fat vs y=sodium, color in the points in red
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point(color="red")

## scatterplot with colors by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, color=mfr)) + geom_point()

## scatterplot with different shapes by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, shape=mfr)) + geom_point()

## scatterplot with different sizes by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, size=mfr)) + geom_point()

## scatterplot with different colors and shapes by the manufacturer
ggplot(UScereal, aes(x=fat, y=sodium, color=mfr, shape=mfr)) + geom_point()

## scatterplot of x=fat vs y=sodium, and a connected line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red")

## scatterplot of x=fat vs y=sodium, and a trend line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + geom_smooth(color="blue")

## scatterplot of x=fat vs y=sodium, and linear line
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + geom_smooth(color="green", method="lm")

## scatterplot of x=fat vs y=sodium, and cubic line (degree 3)
ggplot(UScereal, aes(x=fat, y=sodium)) + geom_point() + geom_line(color="red") + 
  geom_smooth(color="green", method="lm", formula=y~poly(x,3))


##################
##################
## histogram #####
##################
##################
## histogram of carbohydrates
ggplot(UScereal, aes(carbo)) + geom_histogram()

## histogram of carbohydrates, smaller number of bins (10)
ggplot(UScereal, aes(carbo)) + geom_histogram(binwidth=10)

## histogram of carbohydrates
ggplot(UScereal, aes(carbo)) + geom_histogram(aes(y=..density..)) + geom_density()

## histogram of carbohydrates, with mean line and density
ggplot(UScereal, aes(carbo)) + geom_histogram(aes(y=..density..)) + geom_density() +
  geom_vline(aes(xintercept=mean(carbo)), color="blue", linetype="dashed", size=1)


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




