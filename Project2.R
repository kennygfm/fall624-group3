library(plyr) # need to load for bagged tree; load before dplyr to not mask
library("dplyr")
library("tidyr")
library("mice")
library("VIM")
library("Hmisc")
library("lubridate")
library(corrplot)
library(reshape2)
library(caret)
library(AppliedPredictiveModeling)
library(e1071)
library(rpart)
library(RWeka)
library(ipred)
library(randomForest)
library(gbm)
library(Cubist)

# data import, exploration, cleanup, & imputation ####
df <- read.csv(file = "StudentData.csv",
               na.strings = c("", " "),
               header = TRUE)


#Explore the data
str(df) #We may have an oddly named variable
df <- rename(df, Brand.Code=ï..Brand.Code)

summary(df)
describe(df) #Need to consider what to do with zero values: Brand Code of zero for example

head(df)
tail(df)

##DATA CLEANUP

#Explore the data, identify NAs
summary(df) 
md.pattern(df)
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Quite a few missing elements, especially with MFR and Brand.Code - we will remove those rows
#remove NAs
df <- df[!is.na(df$MFR),]
df <- df[!is.na(df$Brand.Code),]


#Let's check again for NAs and see the severity
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Explore the distributions of the remaining columns with fairly significant NAs to determine appropriate
#imputations
hist(df$PSC.CO2) #left skewed
df$PSC.CO2 <- impute(as.matrix(df$PSC.CO2), "median")

hist(df$PC.Volume) #Fairly normal distribution so mean should suffice
df$PC.Volume <- impute(as.matrix(df$PC.Volume), "mean")

hist(df$Fill.Ounces) #Normal, leverage the mean
df$Fill.Ounces <- impute(as.matrix(df$Fill.Ounces), "mean")

hist(df$PSC) #left skewed, so opt for the median
df$PSC <- impute(as.matrix(df$PSC), "median")

hist(df$Carb.Temp) #Normal, leverage the mean
df$Carb.Temp <- impute(as.matrix(df$Carb.Temp), "median")

hist(df$Carb.Pressure) #normal
df$Carb.Pressure <- impute(as.matrix(df$Carb.Pressure), "mean")

hist(df$Hyd.Pressure2) #a lot of zero values which seem problematic - use median, may be smarter to remove the 14 rows
df$Hyd.Pressure2 <- impute(as.matrix(df$Hyd.Pressure2), "median")

hist(df$Hyd.Pressure1) #also a lot of zero values
df$Hyd.Pressure1 <- impute(as.matrix(df$Hyd.Pressure1), "median")

hist(df$Fill.Pressure) #right skewed
df$Fill.Pressure <- impute(as.matrix(df$Fill.Pressure), "median")

hist(df$Filler.Level) #right skewed
df$Filler.Level <- impute(as.matrix(df$Filler.Level), "median")

hist(df$Filler.Speed) #far right skewed, may be wiser to simply remove rows, 3
df$Filler.Speed <- impute(as.matrix(df$Filler.Speed), "median")

hist(df$Carb.Pressure) #normal
df$Carb.Pressure <- impute(as.matrix(df$Carb.Pressure), "mean")

hist(df$Hyd.Pressure3) #a low of zeros and then right skewed
df$Hyd.Pressure3 <- impute(as.matrix(df$Hyd.Pressure3), "median")

hist(df$Hyd.Pressure4) #normal
df$Hyd.Pressure4 <- impute(as.matrix(df$Hyd.Pressure4), "mean")

hist(df$Alch.Rel) #odd distribution - median best, but may prefer to remove rows
df$Alch.Rel <- impute(as.matrix(df$Alch.Rel), "median")

hist(df$Carb.Flow) #odd distribution - median best, but may prefer to remove rows
df$Carb.Flow <- impute(as.matrix(df$Carb.Flow), "median")

hist(df$Oxygen.Filler) #left skewed
df$Oxygen.Filler <- impute(as.matrix(df$Oxygen.Filler), "median")

hist(df$Carb.Pressure1) #right skewed
df$Carb.Pressure1 <- impute(as.matrix(df$Carb.Pressure1), "median")

hist(df$Carb.Volume) #left skewed
df$Carb.Volume <- impute(as.matrix(df$Carb.Volume), "median")

hist(df$Usage.cont) #use median
df$Usage.cont <- impute(as.matrix(df$Usage.cont), "median")

hist(df$Bowl.Setpoint) #use median
df$Bowl.Setpoint <- impute(as.matrix(df$Bowl.Setpoint), "median")

hist(df$PSC.Fill)
df$PSC.Fill <- impute(as.matrix(df$PSC.Fill), "median")

hist(df$Carb.Rel)
df$Carb.Rel <- impute(as.matrix(df$Carb.Rel), "median")

hist(df$Carb.Volume)
df$Carb.Volume <- impute(as.matrix(df$Carb.Volume), "median")

hist(df$Pressure.Setpoint)
df$Pressure.Setpoint <- impute(as.matrix(df$Pressure.Setpoint), "median")


#Let's check again for NAs and see the severity
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


#Review the correlations
df_cor <- df
df_cor$Brand.Code <- NULL #Need to remove because a categorical variable
df_cor$PH <- NULL #Need to remove because our dependent variable
correlations <- cor(df_cor)
corrplot(correlations, order = "hclust")

#Identfiy high correlations
threshold <- 0.8
tooHigh <- findCorrelation(correlations, cutoff = threshold, names = TRUE, verbose = TRUE) 
tooHigh #eight columns - we should remove

#REMEMBER THIS FOR EDITING THE TEST DATA!!
df_cor$Balling <- NULL
df_cor$Hyd.Pressure3 <- NULL
df_cor$Alch.Rel <- NULL
df_cor$Balling.Lvl <- NULL
df_cor$Density <- NULL
df_cor$Density <- NULL
df_cor$Carb.Volume <- NULL
df_cor$Bowl.Setpoint <- NULL
df_cor$Filler.Speed <- NULL

# a few correlations that we shall consider removing those columns

# boxplots and histograms to check distributions
d <- melt(df_cor)

ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()
# Zero values for some variables still need to be considered: Mnf.Flow, Hyd.Pressure1 &2
#beyond that we do not appear to have significant outliers

ggplot(d, aes(x=variable, y=value)) +
  facet_wrap(~variable,scales = "free") +
  geom_boxplot() +
  ggtitle("Boxplots")
#All the more reason to consider doing something with the zeros...

#Check for skewness
skewValues <- apply(df_cor, 2, skewness) 
View(skewValues) #quite a lot of high, negative values likely due to the zeros

#Load evaluation data
df_test <- read.csv(file = "StudentEvaluation.csv",
                    na.strings = c("", " "),
                    header = TRUE)
#clean-up the test data
df_test <- rename(df_test, Brand.Code=ï..Brand.Code)
df_test$PH <- NULL #column is all null values

#remove same columns with the correlations
df_test$Balling <- NULL
df_test$Hyd.Pressure3 <- NULL
df_test$Alch.Rel <- NULL
df_test$Balling.Lvl <- NULL
df_test$Density <- NULL
df_test$Density <- NULL
df_test$Carb.Volume <- NULL
df_test$Bowl.Setpoint <- NULL
df_test$Filler.Speed <- NULL

##Nonlinear Regression Model(s)
#define training data
bc <- df$Brand.Code
trainingData_x <- cbind(df_cor, bc)
trainingData_x <- rename(trainingData_x, Brand.Code=bc)
trainingData_y <- df$PH #Not really necessary, but makes code easier to read


# non-linear models ####

#MARS
library(earth)
marsFit <- earth(x = trainingData_x, y = trainingData_y)
marsFit
summary(marsFit) #RSq = 0.4738075
plotmo(marsFit)

marsPred <- predict(marsFit, newdata = df_test)
marsPerf <- postResample(pred = marsPred, obs = testData$y)

#Now let's do NN
library(nnet)
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                        .size = c(1:10),
                        .bag = FALSE)
set.seed(100)
nnetTune <- train(x = trainingData_x, y = trainingData_y,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  maxit = 500)
nnetTune #best Rsqaured is 0.5328831
summary(nnetTune)
nnetPred <- predict(nnetTune, newdata = df_test)
nnetPerf <- postResample(pred = nnetPred, obs = testData$y) 


#Now let's do SVM
library(kernlab)

#Note that we have to convert Brand.Code to numeric or remove...
bc <- as.character(bc)
bc[bc=='A'] <-1
bc[bc=='B'] <-2
bc[bc=='C'] <-3
bc[bc=='D'] <-4
bc <- as.numeric(bc)

#some updates to the training data pre the changes
trainingData_x <- cbind(df_cor, bc)
trainingData_x <- rename(trainingData_x, Brand.Code=bc)

svmTune <- train(x = trainingData_x, y = trainingData_y,
                 method = "svmLinear", #for homework play with various values, svmRadial, svmPoly (takes the longest time), svmLinear
                 tuneLength = 14,
                 preProc = c("center", "scale"),
                 trControl = trainControl(method = "cv"))
svmTune #Rsquared is 0.3210737
summary(svmTune)

#test data BrandCode needs to be updated too
df_test$Brand.Code <- as.character(df_test$Brand.Code)
df_test$Brand.Code[df_test$Brand.Code=='A'] <-1
df_test$Brand.Code[df_test$Brand.Code=='B'] <-2
df_test$Brand.Code[df_test$Brand.Code=='C'] <-3
df_test$Brand.Code[df_test$Brand.Code=='D'] <-4
df_test$Brand.Code <- as.numeric(df_test$Brand.Code)

svmPred <- predict(svmTune, newdata = df_test)
svmPerf <- postResample(pred = svmPred, obs = testData$y) #SVM(radial) is very poor, may try other kernels



#KNN - NOT WORKING YET...
knnTune <- train(x = trainingData_x, y = trainingData_y, method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10)
knnTune #max(knnModel$results$Rsquared) 0.2349691
knnPred <- predict(knnTune, newdata = df_test) #NAs need to be removed
knnPerf <- postResample(pred = knnPred, obs = testData$y)


# regression tree models ####
# convert training data from matrix to numeric
nomatrix <- trainingData_x %>% 
  mutate_if(is.matrix, as.numeric)

# conventional tree of max depth
set.seed(100)
rpartTune <- train(x = trainingData_x, y = trainingData_y,
                   method = "rpart2",
                   preProc = c("center", "scale"),
                   metric = "Rsquared")
rpartPred <- predict(rpartTune, newdata = df_test)
rpartPerf <- postResample(pred = rpartPred, obs = testData$y)

# rule-based model
set.seed(100)
ruleTune <- train(x = trainingData_x, y = trainingData_y,
                  method = "M5Rules",
                  preProc = c("center", "scale"))
rulePred <- predict(ruleTune, newdata = df_test)
rulePerf <- postResample(pred = rulePred, obs = testData$y)


# bagged tree model
set.seed(100)
bagTune <- train(x = trainingData_x, y = trainingData_y,
                 method = "treebag",
                 preProc = c("center", "scale"))
bagPred <- predict(bagTune, newdata = df_test)
bagPerf <- postResample(pred = bagPred, obs = testData$y)


# random forest model
set.seed(100)
rfTune <- train(x = trainingData_x, y = trainingData_y,
                method = "rf",
                preProc = c("center", "scale"),
                ntrees = 1000, importance = TRUE)
rfPred <- predict(rfTune, newdata = df_test)
rfPerf <- postResample(pred = rfPred, obs = testData$y)


# boosted tree model
set.seed(100)
boostTune <- train(x = trainingData_x, y = trainingData_y,
                   method = "gbm",
                   tuneGrid = expand.grid(shrinkage = c(0.01, 0.05, 0.1),
                                          interaction.depth = seq(1, 9, 2),
                                          n.trees = seq(100, 1000, 100),
                                          n.minobsinnode = 10),
                   verbose = FALSE)
boostPred <- predict(boostTune, newdata = df_test)
boostPerf <- postResample(pred = boostPred, obs = testData$y)


# cubist model (takes a long time)
set.seed(100)
cubistTune <- train(x = trainingData_x, y = trainingData_y,
                    method = "cubist",
                    tuneGrid = expand.grid(neighbors = c(0, 1, 5, 9),
                                           committees = c(1, 25, 50, 75, 100)))
cubistPred <- predict(cubistTune, newdata = df_test)
cubistPerf <- postResample(pred = cubistPred, obs = testData$y)


# model comparision ####
# create function to compare model performance
model_perf <- function (model_set, metric) {
  mdl_names <- character()
  resampled <- numeric()
  test <- numeric()
  for (mdl in model_set) {
    mdl_names <- c(mdl_names, mdl)
    resampled <- c(resampled, min(get(paste0(mdl, "Tune"))$results[[metric]]))
    test <- c(test, get(paste0(mdl, "perf"))[metric])
  }
  pander(data.frame(`Resampled RMSE` = resampled, `Test RMSE` = test,
                    row.names = mdl_names, check.names = FALSE), digits = 4)
}
