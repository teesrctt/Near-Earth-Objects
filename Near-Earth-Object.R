### Team 5 Final Project
### Research Question:
### How can we predict whether or not near-Earth objects
### are dangerous, and which variables are significant 
### indicators of hazardous objects?

# Import libraries
library(caret)
library(ggplot2)
library(GGally)
library(lattice)
library(dplyr)
library(corrplot)
library(VIM)
library(HH)
library(tidyr)
library(tibble)
library(readr)
library(purrr)
library(stringr)
library(forcats)
library(bigmemory)
library(class)

# Import / Inspect Data
neo_df <- read.csv("neo.csv", header = T, 
                   na.strings = "?", stringsAsFactors = T)
head(neo_df)
names(neo_df)
dim(neo_df)
View(neo_df)
summary(neo_df)

pairs(neo_df[,3:6])

# Clean Data & Remove unnecessary variables
neo_df <- neo_df[, -1]
neo_df <- neo_df[, -6]
neo_df <- neo_df[, -6]
View(neo_df)

# Name variable will be kept for 
# labeling, organization, and visualization

# Separate continuous and categorical variables
num_neo <- neo_df %>% select(where(is.numeric))
cat_neo <- neo_df %>% select(where(is.factor))
View(num_neo)
View(cat_neo)

# Explore Data
pairs(num_neo)
max(num_neo[,1])

neo_pairs_plot <- ggpairs(num_neo[,3:5])

df_1 <- num_neo[,3:5]
View(df_1)

df1_cor <- cor(df_1)
df1_cor
corrplot(df1_cor)

# Visualizing relative_velocity, miss_distance, and absolute_magnitude
vel_mag_plot1 <- ggplot(df_1, aes(x = absolute_magnitude, y = relative_velocity), binwidth = 0.5) +
  geom_point(alpha = 0.1) +
  ggtitle("Magnitude to Veolcity") + 
  theme(plot.title = element_text(hjust = 0.4))


contrasts(neo_df$hazardous)

# Test for significant relationships

lm.fit1 <- lm(miss_distance ~ est_diameter_min + est_diameter_max + 
                absolute_magnitude, data = neo_df)
summary(lm.fit1)
plot(lm.fit1)

# est_diameter_min and absolute_magnitude are significant, with 
# the latter being most significant.
# R-Squared: .07 = 7% variance explained by the model

lm.fit2 <- lm(miss_distance ~ est_diameter_min + relative_velocity + 
                absolute_magnitude, data = neo_df)
summary(lm.fit2)
plot(lm.fit2)


lm.fit3 <- lm(miss_distance ~ relative_velocity + 
                absolute_magnitude, data = df_1)
summary(lm.fit1)
plot(lm.fit1)


# relative_velocity and absolute_magnitude are both very significant
# R-Squared: .13 = 13% variance explained by the model

# Diagnostic Plots:
# Our Residuals vs Fitted plot shows somewhat equally spread points
# in a downward, linear pattern.
# Our Normal Q-Q plot shows a pattern that slopes upward towards a line, 
# follows a linear path, then deviates horizontally.
# Our Scale-Location plot shows a high concentration of residuals
# in the middle of the line (4e+07), and an gradually thinner spread
# towards the beginning and end.

# Test for Multicollinearity
vif(lm.fit3)

# There is very low multicollinearity in the final model










#Classification Model
#Logistic Regression Model

#Create a binary variable
attach(neo_df)
hazardous01=rep(0, nrow(neo_df))
hazardous01
hazardous01= ifelse(hazardous == "True", 1, 0)
hazardous01[1:20]

sum(hazardous01)
dim(hazardous01)

#Create a single data set that contains the newly-created binary variable, hazardous01,
#and all the other variables except hazardous in the neo_df data set

neo_df01=data.frame(neo_df,hazardous01)
names(neo_df01)
neo_df01$hazardous=NULL
names(neo_df01)

dim(neo_df01)
summary(neo_df01)

#Split neo_df01 into a training (containing 72,669 observations) 
#and a test set (containing 18,167 observations). 
set.seed(17)
train = sample(90836, 72669)
train[1:20]

neo_train=neo_df01[train,]
neo_test=neo_df01[-train,]
dim(neo_train)
dim(neo_test)

#Build a logistic regression model using all possible predictors 
#in order to predict the probability of dangerous objects
glm.fit=glm(hazardous01~est_diameter_max+est_diameter_min+miss_distance + relative_velocity + absolute_magnitude,neo_train,family=binomial)
summary(glm.fit)

#Making prediction for training dataset and estimated the training error
glm.probtr=predict(glm.fit,type="response")
glm.probtr[1:20]

glm.predtr=rep(0,nrow(neo_train))
glm.predtr[1:20]
glm.predtr[glm.probtr>0.5]=1
glm.predtr[1:20]

table(glm.predtr,neo_train$hazardous01)
mean(glm.predtr!=neo_train$hazardous01)
#Making prediction for test dataset and test error

glm.probts=predict(glm.fit,neo_test,type="response")
glm.predts=rep(0,nrow(neo_test))
glm.predts[glm.probts>0.5]=1

table(glm.predts,neo_test$hazardous01)
mean(glm.predts!=neo_test$hazardous01)

#Training error rate: 9.81%
#Test error rate: 9.93%
#This means that the model is able to predict hazardous objects with 
#an accuracy of 90.19% on the training dataset and 90.07% on the test dataset.

#K-nearest neighbors(KNN) model
names(neo_train)
summary(neo_train)
View(neo_train)
#we make a new training and test dataframe with only quantitative variables. 
#And then we put them on scale.

neo_trainx=neo_train[,-c(1,7)]
View(neo_trainx)
neo_trainx=scale(neo_trainx)
summary(neo_trainx)
dim(neo_trainx)

neo_testx=neo_test[,-c(1,7)]
View(neo_testx)
neo_testx=scale(neo_testx)
summary(neo_testx)
dim(neo_testx)

train.hazardous01=neo_train[,7]
train.hazardous01[1:20]

test.hazardous01=neo_test[,7]
test.hazardous01[1:20]

set.seed(17)
#k=1
knn.pred=knn(neo_trainx,neo_testx,train.hazardous01, k=1)
table(knn.pred,test.hazardous01)
mean(knn.pred!=test.hazardous01)
#Test error: 12.02%

knn.pred=knn(neo_trainx,neo_testx,train.hazardous01, k=3)
table(knn.pred,test.hazardous01)
mean(knn.pred!=test.hazardous01)
#Test error: 11.04%

knn.pred=knn(neo_trainx,neo_testx,train.hazardous01, k=5)
table(knn.pred,test.hazardous01)
mean(knn.pred!=test.hazardous01)
#Test error: 10.48%

knn.pred=knn(neo_trainx,neo_testx,train.hazardous01, k=7)
table(knn.pred,test.hazardous01)
mean(knn.pred!=test.hazardous01)
#Test error: 10.05%

knn.pred=knn(neo_trainx,neo_testx,train.hazardous01, k=9)
table(knn.pred,test.hazardous01)
mean(knn.pred!=test.hazardous01)
#Test error: 9.83%

#Test error rate for kNN
#K=1:12.02%
#K=3:11.04%
#K=5:10.48%
#K=7:10.05%
#K=9: 9.83%

#The KNN model achieved the lowest test error rate of 9.83% when k was set to 9. 
#This means that the KNN model is able to predict hazardous objects with 
#an accuracy of 90.17% on the test dataset.

#Two models were built, a logistic regression model and a K-nearest neighbors 
#(KNN) model,and their performance was compared. 
#Both models had similar accuracy, but the choice of model may depend on 
#the specific goals and requirements of the analysis





























































