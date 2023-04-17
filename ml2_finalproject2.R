## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats
rm(list=ls())
library(tidyverse) # metapackage with lots of helpful functions
library(lubridate)
## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

library(data.table)
###Defining types of extracted attributes
ctypes <- cols(fullVisitorId = col_character(),
               channelGrouping = col_character(),
               date = col_datetime(),
               device = col_character(),
               geoNetwork = col_character(),
               socialEngagementType = col_character(), 
               totals = col_character(),
               trafficSource = col_character(),
               visitId = col_integer(), 
               visitNumber = col_integer(),
               visitStartTime = col_integer(),
               hits = col_integer(),
               customDimensions = col_character())
train = read_csv("train_v2.csv", col_names = TRUE,
              col_types = ctypes)
test = read_csv("test_v2.csv", col_names = TRUE,
             col_types = ctypes)

print('train:'); dim(train)
print('test:'); dim(test)
library(dplyr)
train <- train %>% sample_frac(0.01, replace = FALSE)
test <- test %>% sample_frac(0.01, replace = FALSE)

###Data parsing
library(jsonlite)
flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

message("Data Parsing")
train <- parse(train)
test <- parse(test)
train$hits...55 <- as.numeric(train$hits...55)
train$hits...7 <- as.numeric(train$hits...7)
train$hits <- train$hits...7 + train$hits...55
train <- train[,-5]
train <- train[,-50]
test$hits...55 <- as.numeric(test$hits...55)
test$hits...7 <- as.numeric(test$hits...7)
test$hits <- test$hits...55 + test$hits...7
test <- test[,-50]
test <- test[,-5]

###Defining useful columns
good_cols = c("channelGrouping","date","fullVisitorId","visitId","visitNumber","visitStartTime","browser","deviceCategory",
              "isMobile","operatingSystem","city","continent","country","metro","networkDomain","region","subContinent","bounces",                              
              "hits","newVisits","pageviews","sessionQualityDim","timeOnSite","totalTransactionRevenue","transactionRevenue",
              "transactions","adContent","adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd",
              "adwordsClickInfo.page", "adwordsClickInfo.slot","campaign","isTrueDirect","keyword","medium","referralPath","source")

###Combining tables and convertion to dataframe
train = rbind(train[c(good_cols)])
train = as.data.frame(train)
# correct the data types
data_num <- train[,c(4,17:23)]
data_cat <- train[,-c(4,17:23)]
data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
train <- cbind(data_cat,data_num)
str(train)

library(chron)
train$date <- as.Date(as.character(train$date), format= "%Y-%m-%d")
train$dayOfMonth <- as.factor(days(train$date))
train$month <- as.factor(month(train$date))
str(train)

head(train)

# correct the data types
test = rbind(test[c(good_cols)])
data_num <- test[,c(4,17:23)]
data_cat <- test[,-c(4,17:23)]
data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
test <- cbind(data_cat,data_num)
str(test)

test$date <- as.Date(as.character(test$date), format= "%Y-%m-%d")
test$dayOfMonth <- as.factor(days(test$date))
test$month <- as.factor(month(test$date))
str(test)

###if transactionRevenue is NA, it means 0
train$transactionRevenue <- ifelse(is.na(train$transactionRevenue) == TRUE, 0, train$transactionRevenue)
train$transactionRevenue <- as.numeric(train$transactionRevenue)
train$totalTransactionRevenue <- ifelse(is.na(train$totalTransactionRevenue) == TRUE, 0, train$totalTransactionRevenue)
train$totalTransactionRevenue <- as.numeric(train$totalTransactionRevenue)
train$transactions <- ifelse(is.na(train$transactions) == TRUE, 0, train$transactions)
train$transactions <- as.numeric(train$transactions)
train$channelGrouping <- max(ifelse(is.na(train$channelGrouping) == TRUE, -9999, train$channelGrouping))
train$channelGrouping <- as.factor(train$channelGrouping)
train$browser <- max(ifelse(is.na(train$browser) == TRUE, -9999, train$browser))
train$browser <- as.factor(train$browser)
train$operatingSystem <- max(ifelse(is.na(train$operatingSystem) == TRUE, -9999, train$operatingSystem))
train$deviceCategory <- max(ifelse(is.na(train$deviceCategory) == TRUE, -9999, train$deviceCategory))
train$deviceCategory <- as.factor(train$deviceCategory)
train$continent <- max(ifelse(is.na(train$continent) == TRUE, -9999, train$continent))
train$subContinent <- max(ifelse(is.na(train$subContinent) == TRUE, -9999, train$subContinent))
train$country <- max(ifelse(is.na(train$country) == TRUE, -9999, train$country))
train$region <- max(ifelse(is.na(train$region) == TRUE, -9999, train$region))
train$metro <- max(ifelse(is.na(train$metro) == TRUE, -9999, train$metro))
train$city <- max(ifelse(is.na(train$city) == TRUE, -9999, train$city))
train$networkDomain <- max(ifelse(is.na(train$networkDomain) == TRUE, -9999, train$networkDomain))
train$source <- max(ifelse(is.na(train$source) == TRUE, -9999, train$source))
train$medium <- max(ifelse(is.na(train$medium) == TRUE, -9999, train$medium))
train$isVideoAd_mean <- mean(ifelse(is.na(train$adwordsClickInfo.isVideoAd) == TRUE, 0, 1))
train$isMobile <- mean(ifelse(train$isMobile == TRUE, 1 , 0))
train$isTrueDirect <- mean(ifelse(is.na(train$isTrueDirect) == TRUE, 0, 1))
train$bounces <- sum(ifelse(is.na(train$bounces) == TRUE, 0, 1))
train$newVisits <- ifelse(is.na(train$newVisits) == TRUE, 0,1 )
train$visitNumber <- as.numeric(train$visitNumber)
train$operatingSystem <- as.factor(train$operatingSystem)
train$city <- as.factor(train$city)
train$continent <- as.factor(train$continent)
train$country <- as.factor(train$country)
train$metro <- as.factor(train$metro)
train$networkDomain <- as.factor(train$networkDomain)
train$medium <- as.factor(train$medium)
train$source <- as.factor(train$source)
train$hits <- ifelse(is.na(train$hits) == TRUE, 0, train$hits)
train$fullVisitorId <- as.factor(train$fullVisitorId)
#train <- train[,-c(42,43)]
train <- train[,-c(41)]
train <- train[,-c(38,37)]
# Split Data into Train and test - 70:30
library(caret)
set.seed(7)
#train_test_split<-createDataPartition(train$transactionRevenue, p=0.7, list = FALSE)
#train_data<-train[train_test_split,]
#test_data<-train[-train_test_split,]

library(gbm)
library(randomForest)
install.packages('tree')
library(tree)
set.seed(7)
# train GBM model
gbm.fit <- gbm(
  formula = transactionRevenue ~ .,
  distribution = "gaussian",
  data = train[,-c(1,2,3,5,15,16,18,19,20,21,22,23,24,25,26,27,29,37)],
  n.trees = 3500,
  interaction.depth = 6,
  shrinkage = 0.001,
  cv.folds = 3,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
print(gbm.fit)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm.fit, method = "cv")
print(best.iter)

# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))

summary(gbm.fit)

# predict values for test data
test$transactionRevenue <- ifelse(is.na(test$transactionRevenue) == TRUE, 0, test$transactionRevenue)
test$transactionRevenue <- as.numeric(test$transactionRevenue)
test$totalTransactionRevenue <- ifelse(is.na(test$totalTransactionRevenue) == TRUE, 0, test$totalTransactionRevenue)
test$totalTransactionRevenue <- as.numeric(test$totalTransactionRevenue)
test$transactions <- ifelse(is.na(test$transactions) == TRUE, 0, test$transactions)
test$transactions <- as.numeric(test$transactions)
test$channelGrouping <- max(ifelse(is.na(test$channelGrouping) == TRUE, -9999, test$channelGrouping))
test$channelGrouping <- as.factor(test$channelGrouping)
test$browser <- max(ifelse(is.na(test$browser) == TRUE, -9999, test$browser))
test$browser <- as.factor(test$browser)
test$operatingSystem <- max(ifelse(is.na(test$operatingSystem) == TRUE, -9999, test$operatingSystem))
test$deviceCategory <- max(ifelse(is.na(test$deviceCategory) == TRUE, -9999, test$deviceCategory))
test$deviceCategory <- as.factor(test$deviceCategory)
test$continent <- max(ifelse(is.na(test$continent) == TRUE, -9999, test$continent))
test$subContinent <- max(ifelse(is.na(test$subContinent) == TRUE, -9999, test$subContinent))
test$country <- max(ifelse(is.na(test$country) == TRUE, -9999, test$country))
test$region <- max(ifelse(is.na(test$region) == TRUE, -9999, test$region))
test$metro <- max(ifelse(is.na(test$metro) == TRUE, -9999, test$metro))
test$city <- max(ifelse(is.na(test$city) == TRUE, -9999, test$city))
test$networkDomain <- max(ifelse(is.na(test$networkDomain) == TRUE, -9999, test$networkDomain))
test$source <- max(ifelse(is.na(test$source) == TRUE, -9999, test$source))
test$medium <- max(ifelse(is.na(test$medium) == TRUE, -9999, test$medium))
test$isVideoAd_mean <- mean(ifelse(is.na(test$adwordsClickInfo.isVideoAd) == TRUE, 0, 1))
test$isMobile <- mean(ifelse(test$isMobile == TRUE, 1 , 0))
test$isTrueDirect <- mean(ifelse(is.na(test$isTrueDirect) == TRUE, 0, 1))
test$bounces <- sum(ifelse(is.na(test$bounces) == TRUE, 0, 1))
test$newVisits <- ifelse(is.na(test$newVisits) == TRUE, 0,1 )
test$visitNumber <- as.numeric(test$visitNumber)
test$operatingSystem <- as.factor(test$operatingSystem)
test$city <- as.factor(test$city)
test$continent <- as.factor(test$continent)
test$country <- as.factor(test$country)
test$metro <- as.factor(test$metro)
test$networkDomain <- as.factor(test$networkDomain)
test$medium <- as.factor(test$medium)
test$source <- as.factor(test$source)
test$fullVisitorId <- as.factor(test$fullVisitorId)
test <- test[,-c(41)]
test <- test[,-c(38,37)]

# predict values for test split
pred <- predict(gbm.fit, n.trees = best.iter, test)
# results
caret::RMSE(pred, test$transactionRevenue)

test <- test[,-c(1,2,3,5,15,19,20,21,22,23,24,25,26,27,29,37)]

pred <- predict(gbm.fit, n.trees = best.iter, test)
# results
caret::RMSE(pred, test$transactionRevenue)

test$target <- ifelse(pred<0,0,pred)

tsf_data <- data.frame(fullVisitorId=unique(test$fullVisitorId))
dim(tsf_data)

temp<-test %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log(sum(test$target)+1))
tsf_data <- merge(tsf_data,temp,by="fullVisitorId",sort = FALSE,all.x=TRUE)
dim(tsf_data)

head(tsf_data)

submission_v2 <- read_csv('../input/ga-customer-revenue-prediction/sample_submission_v2.csv',col_names=TRUE)
head(submission_v2)

submission_v2<-submission_v2[,-c(2)]
submission<-merge(submission_v2,tsf_data,by="fullVisitorId",sort = FALSE,all.x=TRUE)
head(submission)

# --- write to csv
write.csv(submission, "submission.csv",row.names = FALSE)