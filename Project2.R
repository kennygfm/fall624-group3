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


df = read.csv(file = "C:\\Users\\Ken Markus\\Downloads\\StudentData.csv",
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

ggplot(d, aes(x=variable, y=value)) +
  facet_wrap(~variable,scales = "free") +
  geom_boxplot() +
  ggtitle("Boxplots")
#All the more reason to consider doing something with the zeros...

#Check for skewness
skewValues <- apply(df_cor, 2, skewness) 
View(skewValues) #quite a lot of high, negative values likely due to the zeros
