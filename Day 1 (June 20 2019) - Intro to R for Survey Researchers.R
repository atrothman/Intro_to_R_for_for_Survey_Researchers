## clear all contents from the environment
rm(list=ls())

#############
## slide 8 ##
## Vectors ##
#############
## numeric vector
a <- c(1,2,5.3,6,-2,4) 
a
## character vector
b <- c("one","two","three")
b
## logical vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
c


##############
## slide 9  ##
## Matrices ##
##############
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
## populate the matrix by row
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames, cnames))
mymatrix
## populate the matrix by column
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE, dimnames=list(rnames, cnames))
mymatrix


#################
## slide 10    ##
## DataFrames  ##
#################
ID <- c(1,2,3,4)
Color <- c("red", "white", "red", NA)
Passed <- c(TRUE,TRUE,TRUE,FALSE)
my_df <- data.frame(ID, Color, Passed) 
my_df


#####################    
## slide 13        ##
## Importing Data  ##
#####################
Pizza_df <- read.csv("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\mock_pizza_data.csv", header = TRUE)
Pizza_df
rm(Pizza_df)
setwd("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\")
Pizza_df <- read.csv("mock_pizza_data.csv", header = TRUE)
Pizza_df


####################################    
## slide 14                       ##
## Creating Variables / View Data ##
####################################
Colors <- c("green", "red", "blue", "yellow", "orange", "purple", "brown", "black")
Ages <- c(25, 30, 35, 40, 45, 50, 55, 60)
My_df <- data.frame(Colors, Ages)
My_df
dim(My_df)
head(My_df)
tail(My_df)

head(Pizza_df)
tail(Pizza_df)


###############################    
## slide 15                  ##
## Creating/View Dataframes  ##
###############################
Gender <- c("M", "M", "F", "F", "M", "F", "M", "M")
My_df$Gender <- Gender
My_df$Day <- "Monday"
My_df


#############################    
## slide 17                ##
## Transforming variables  ##
#############################
Ages <- c(25, 30, 35, 40, 45, 50, 55, 60, 65)
AverageAges <- sum(Ages)/9
NewAge <- Ages - AverageAges
Age_df <- data.frame(Ages, NewAge)
Age_df


################################    
## slide 20                   ##
## Useful functions in dplyr  ##
################################
library(dplyr)

head(Pizza_df)
colnames(Pizza_df)

## select command
ex1 <- select(Pizza_df, Fresh, Frozen)
head(ex1)

## filter command
ex2 <- filter(Pizza_df, Fresh==1)
head(ex2)
dim(ex2)

## arrange command
table(Pizza_df$cheese_sat)
ex3 <- arrange(Pizza_df, cheese_sat)
ex3
ex3 <- arrange(Pizza_df, desc(cheese_sat))
ex3

## mutate command
ex4 <- Pizza_df %>% mutate(importance_combined = cheese_i+pepperoni_i)
head(ex4)


##########################    
## slide 22             ##
## R examples in dplyr  ##
##########################

## summarise command
ex5 <- Pizza_df %>% summarise(avg_cheese_i = mean(cheese_i))
ex5

## group_by command
ex6 <- Pizza_df %>% group_by(gender) %>% summarise(avg_cheese_i = mean(cheese_i))
ex6


################    
## slide 24   ##
## Summary()  ##
################
library(psych)

summary(Pizza_df)
describe(Pizza_df)

mean(Pizza_df$cheese_i)
var(Pizza_df$cheese_i)
sd(Pizza_df$cheese_i)
sum(Pizza_df$cheese_i)
table(Pizza_df$cheese_i)


######################################    
## slide 26                         ##
## Basic Visualizations: Pie Chart  ##
######################################
slices <- c(11, 17, 16, 16, 8)
lbls <- c("cheese", "pepperoni", "sausage", "white sauce", "pesto")
pie(slices, labels = lbls, main="Pie Chart")




