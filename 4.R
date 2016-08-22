# clear the environment 
rm(list= ls())

#set working directory
setwd("~/R programming/wine quality/")

#import data sets
red <- read.csv("winequality-red.csv", sep = ";")
white <- read.csv("winequality-white.csv", sep = ";")

#view data sets
summary(red)
summary(white)

boxplot(red)
boxplot(white)
#create an outlier function to clean outliers
outlier <- function(x){
  a <- boxplot(x)$out
  x <- ifelse(x %in% a , NA, x)
}

#convert outliers to NAs and remove observations containing NAs
for (n in 1:6) {
  red <- as.data.frame(apply(red, 2, outlier))
  white <- as.data.frame(apply(white, 2, outlier))
  red <- na.omit(red)
  white <- na.omit(white)
  n <- n +1
}

#summaries data sets
summary(red)
summary(white)

# create a new features
# fbtso2 = (free so2)/(total so2)
red$fbtso2 <- (red$free.sulfur.dioxide/red$total.sulfur.dioxide)
white$fbtso2 <- (white$free.sulfur.dioxide/white$total.sulfur.dioxide)

# normalising the data using clustersim library
# using min-max method
library(clusterSim)
red_T1 <- data.Normalization(red[,-12],type = "n4")
red_T1$is.red <- 1
red_T1$quality <- red$quality

white_T1 <- data.Normalization(white[,-12],type = "n4")
white_T1$is.red <- 0
white_T1$quality <- white$quality

#combine both data sets to create a new data-set wine1
wine1 <- rbind(red_T1,white_T1)
wineTT <- wine1

#binn quality variable
wine1$qualityB <- ifelse(wine1$quality <= 5 ,0,1)
wine1$qualityB <- as.factor(wine1$qualityB)

#using corrplot find out co-related variables
library(corrplot)
corrplot(cor(wine1[,1:12]),method = "number", order = "hclust")

#find out corelation of variables with quality
cor(wine1[,1:12],wine1$quality)

#removing co-related variables 
wine2 <- wine1[,c(-6,-8,-14)]

#checking corelation of variables
corrplot(cor(wine2[,1:11]),method = "number", order = "hclust")

#sampling data 
library(sampling)
set.seed(1234)
rownames(wine2) <- c(1:4582)
stratas <- strata(wine2,stratanames = c("is.red"),
                  size = c(830,3293),method = "srswor", description = T)
train <- getdata(wine2,stratas)
train <- train[,1:12]
test <- wine2[!(1:nrow(wine2)) %in% (rownames(train)),]


#creating a classification model
library(caret)

#model1 using c5.0 algorithm
model1 <- train(qualityB~. , data = train, method = "C5.0")
pred1 <- predict(object = model1,test)
table(pred1,test$qualityB)

#model2 using random forest
model2 <- train(qualityB~. , data = train, method = "rf")
pred2 <- predict(model2,test)
table(pred2,test$qualityB)


#improving models
library(C50)
model3 <- C5.0(train[,-11],train$qualityB,trials = 30, rules = T)
pred3 <- predict(model3,test)
table(pred3,test$qualityB)

library(randomForest)
set.seed(1234)
model4 <- randomForest(qualityB~., data = train,ntree = 850,mtry =10)
pred4 <- predict(model4,test)
table(pred4,test$qualityB)
importance(model4)


#extracting rules 
library(inTrees)
treelist <- RF2List(rf = model4)
extrctrees <- extractRules(treelist,train[,-11],ntree = 50, maxdepth = 3, random = T)

ruleMetric <- getRuleMetric(extrctrees,train[,-11],train$qualityB)
ruleMetric <- pruneRule(ruleMetric,train[,-11],train$qualityB)
learner <- buildLearner(ruleMetric,train[,-11],train$qualityB)
readablerules <- presentRules(ruleMetric,colnames(train[,-11]))

#extract rules to excel
library(xlsx)
write.xlsx(readablerules, "A://rules3.xlsx")

