## clear list
rm(list=ls())

# ######################
# ## Homework recap ####
# ######################
# ##1.	Run correlations using three different functions
# ##a.	Var1 = Number of times eaten pizza in the past 7 days
# ##b.	Var2 = Overall satisfaction with preferred pizza brand
# ##2.	Run a regression using lm()
# ##a.	IV = Number of times eaten pizza in the past 7 days
# ##b.	IV = Importance of cheese quality
# ##c.	DV = Overall satisfaction with preferred pizza brand
# 
# ## load "pizza" dataset
# pizza <- read.csv("C:\\Users\\Andrew\\Desktop\\Research_Rockstar\\mock_pizza_data.csv", header = TRUE)
# head(pizza)
# 
# ####Problem 1
# ## method 1
# cor(pizza$num7, pizza$brand_sat, method="pearson")
# ## method 2
# cor.test(pizza$num7, pizza$brand_sat, alternative=c("two.sided"), conf.level=0.95, method=c("pearson"))
# ## method 3
# library(Hmisc)
# rcorr(pizza$num7, pizza$brand_sat, type=c("pearson"))
# 
# ####Problem 2
# summary(lm(brand_sat ~ num7 + cheese_i, data=pizza))


######################
######################
## R course - Day 4 ##
######################
######################
library(MASS)
library(dplyr)
library(cluster)
library(ggplot2)
library(leaps)
library(glmnet)
df <- data(UScereal)
df <- UScereal
df$Brand <- row.names(df)
df <- select(df, Brand, calories:sugars, potassium)
row.names(df) <- NULL

#######################
## Selection methods ##
#######################
summary(lm(potassium ~ calories + protein + fat + sodium + fibre + carbo + sugars, data=df))

## best subset selection:
model_bs <- summary(regsubsets(potassium ~ calories + protein + fat + sodium + fibre + carbo + sugars, data=df))
model_bs
model_bs$adjr2

## forward stepwise:
model_fs <- summary(regsubsets(potassium ~ calories + protein + fat + sodium + fibre + carbo + sugars, data=df, method='forward'))
model_fs
model_fs$adjr2

## backward stepwise:
model_bs <- summary(regsubsets(potassium ~ calories + protein + fat + sodium + fibre + carbo + sugars, data=df, method='backward'))
model_bs
model_bs$adjr2

## LASSO
Y <- as.matrix(df$potassium)
X <- as.matrix(select(df, calories:sugars))
CV_LASSO <- cv.glmnet(X, Y, alpha=1)
best_lambda <- CV_LASSO$lambda.min
LASSO <- glmnet(X, Y, alpha=1, lambda=best_lambda)
LASSO$a0
LASSO$beta


########################
## K means clustering ##
########################
num_clusters <- c(2:20)
df_sill <- data.frame(num_clusters)
df_sill$sill_coeff <- NA
for(i in 1:length(num_clusters)){
  km <- kmeans(Xs, num_clusters[i], nstart=20)
  ss <- silhouette(km$cluster, dist(Xs))
  df_sill$sill_coeff[i] <- mean(ss[,3])
}
plot(df_sill$num_clusters, df_sill$sill_coeff, type="o")

km3 <- kmeans(Xs, 3, nstart=20)


###############################
## hierachy based clustering ##
###############################
## can also do average and single
X <- as.matrix(select(df, -Brand))
hc.complete=hclust(dist(X), method="complete")
plot(hc.complete, main="Complete Linkage", labels=df$Brand, sub="", cex=.9)
Xs <- scale(X)
hc.complete=hclust(dist(Xs), method="complete")
plot(hc.complete, main="Complete Linkage", labels=df$Brand, sub="", cex=.9)


##################
## PCA analysis ##
##################
pr.out <- prcomp(X, scale=TRUE)
eigs <- (pr.out$sdev)^2
Var_explained <- eigs/sum(eigs)

## PC_plot
PC_num <- c(1:length(Var_explained))
PCA_df <- data.frame(PC_num)
PCA_df$Var_explained <- Var_explained
PCA_df <- filter(PCA_df, PC_num<=20)
PCA_df$PC_num <- as.factor(PCA_df$PC_num)
PCA_plot <- ggplot(PCA_df, aes(x=PC_num, y=Var_explained, group=1)) + geom_col() + geom_point() + geom_line(color="red") + scale_y_continuous(labels=scales::percent) +
  ggtitle("Scree Plot from PCA") + theme(plot.title = element_text(hjust = 0.5)) +  xlab("Principal Components") + ylab("Percentage of Explained Variances")
plot(PCA_plot)







