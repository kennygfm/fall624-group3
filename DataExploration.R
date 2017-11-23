#install.packages('gdata')
library("gdata")
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

df = read.xls("~/Documents/Predictive Analytics/StudentData.xlsx", sheet = 1, header = TRUE)

#Explore the data
str(df)
summary(df)
describe(df) #Need to consider what to do with zero values: Brand Code of zero for example

df_zero <- df[df$Brand.Code=='0',]
head(df)
tail(df)

#Explore the data, identify NAs
summary(df) 
md.pattern(df)
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
#No missing elements

#Review the correlations
df_cor <- df
df_cor$Brand.Code <- NULL #Need to remove because a categorical variable
df_cor$PH <- NULL #Need to remove because our dependent variable
correlations <- cor(df_cor)
corrplot(correlations, order = "hclust")

#Identfiy high correlations
threshold <- 0.8
tooHigh <- findCorrelation(correlations, cutoff = threshold, names = TRUE, verbose = TRUE) 
tooHigh
# a few correlations that we shall consider removing those columns

#Take a look at distributions to identify outliers
# plot histograms of data
ggplot(df,aes(x = EUI_per_zcta)) +
  geom_histogram()

# boxplots and histograms to check distributions
d <- melt(df_cor)

ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

ggplot(d, aes(x=variable, y=value)) +
  facet_wrap(~variable,scales = "free") +
  geom_boxplot() +
  ggtitle("Boxplots")
#All the more reason to consider doing something with the zeros...

#Check for skewness
skewValues <- apply(df_cor, 2, skewness) 
View(skewValues) #quite a lot of high, negative values likely due to the zeros


