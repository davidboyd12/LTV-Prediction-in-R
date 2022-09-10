train = read.csv("C:\\Users\\boydd\\Downloads\\train.csv (1)\\train.csv")
test = read.csv("C:\\Users\\boydd\\OneDrive\\Documents\\test.csv")

dfTrain = subset(train, select = -c(LTV))
df = rbind(dfTrain, test)
str(df)
#work with payment method
table(df$PAYMENT_METHOD)

#new columns for apple pay, card, and paypal
df$apple = ifelse(df$PAYMENT_METHOD == 'apple-pay',1,0)
df$card = ifelse(df$PAYMENT_METHOD == 'card',1,0)
df$paypal = ifelse(df$PAYMENT_METHOD == 'paypal',1,0)

#Drop old column
df = subset(df, select = -c(PAYMENT_METHOD))

table(df$CARD_TYPE)
df$credit = ifelse(df$CARD_TYPE == 'credit',1,0)
df$debit = ifelse(df$CARD_TYPE == 'debit',1,0)
df$prepaid = ifelse(df$CARD_TYPE == 'prepaid',1,0)
df$unknown = ifelse(df$CARD_TYPE == 'unknown',1,0)

table(df$USER_HEARD_ABOUT_US)

df$socialMedia = ifelse(df$USER_HEARD_ABOUT_US == 'tiktok' || df$USER_HEARD_ABOUT_US == 'twitter'||
                        df$USER_HEARD_ABOUT_US == 'youtube' || df$USER_HEARD_ABOUT_US == 'tumblr'||
                        df$USER_HEARD_ABOUT_US == 'snapchat' || df$USER_HEARD_ABOUT_US == 'reddit'||
                        df$USER_HEARD_ABOUT_US == 'quora' || df$USER_HEARD_ABOUT_US == 'pinterest'||
                        df$USER_HEARD_ABOUT_US == 'instagram' || df$USER_HEARD_ABOUT_US == 'facebook'||
                        df$USER_HEARD_ABOUT_US == 'blog post' || df$USER_HEARD_ABOUT_US == 'mobile app',1,0)
df$TV = ifelse(df$USER_HEARD_ABOUT_US == 'tv' || df$USER_HEARD_ABOUT_US == 'streaming tv / hulu / netflix'||
               df$USER_HEARD_ABOUT_US == 'radio / pandora' || df$USER_HEARD_ABOUT_US == 'radio / podcast / spotify / pandora'||
               df$USER_HEARD_ABOUT_US == 'podcast' || df$USER_HEARD_ABOUT_US == 'hulu',1,0)
df$ad = ifelse(df$USER_HEARD_ABOUT_US == 'search engine' || df$USER_HEARD_ABOUT_US == 'online magazine or newspaper article'||
               df$USER_HEARD_ABOUT_US == 'online banner ad'||df$USER_HEARD_ABOUT_US == 'internet search (google / bing / yahoo)'||
               df$USER_HEARD_ABOUT_US == 'nextdoor',1,0)
df$reference = ifelse(df$USER_HEARD_ABOUT_US == 'email from another company' || df$USER_HEARD_ABOUT_US == 'family / friend'||
                        df$USER_HEARD_ABOUT_US == 'promotion card from another company',1,0)
df$oldFashion = ifelse(df$USER_HEARD_ABOUT_US == 'usps mail' || df$USER_HEARD_ABOUT_US == 'printed magazine or newspaper article',1,0)
df$sample = ifelse(df$USER_HEARD_ABOUT_US == 'bless box' || df$USER_HEARD_ABOUT_US == 'box/package insert'||
                     df$USER_HEARD_ABOUT_US == 'curology sample' || df$USER_HEARD_ABOUT_US == 'cocotique'||
                     df$USER_HEARD_ABOUT_US == 'product sample'||df$USER_HEARD_ABOUT_US == 'whisper',1,0)
df$other = ifelse(df$USER_HEARD_ABOUT_US == 'other (please specify)',1,0)
df = subset(df, select = -c(USER_HEARD_ABOUT_US))
table(df$USER_OS)
table(df$USER_DEVICE)

df$chromeOS = ifelse(df$USER_OS == "Chrome"|df$USER_OS == "Chrome Mobile"|
                     df$USER_OS == "Chrome WebView",1,0)
df$firefox = ifelse(df$USER_OS == "Firefox"|df$USER_OS == "Firefox Focus",1,0)
df$safari = ifelse(df$USER_OS == "Safari"|df$USER_OS == "Mobile Safari",1,0)
df$facebook = ifelse(df$USER_OS == "Facebook",1,0)
df$edge = ifelse(df$USER_OS == "Edge",1,0)
df$gsa = ifelse(df$USER_OS == "GSA",1,0)
df$otherOS = ifelse(df$USER_OS == "Yandex" | df$USER_OS == "WeChat"|
                      df$USER_OS == "Vivaldi"|df$USER_OS == "UCBrowser"|
                      df$USER_OS == "Silk"|df$USER_OS == "Samsung Browser"|
                      df$USER_OS == "Opera Touch"|df$USER_OS == "Opera"|
                      df$USER_OS == "MIUI Browser"|df$USER_OS == "Line"|
                      df$USER_OS == "jasmine",1,0)
df$webKit = ifelse(df$USER_OS == "WebKit",1,0)

library(stringr)
for (i in 1:nrow(df)){
  df[i, "numAddOns"] = str_count(df[i,"PRODUCT_ADD_ONS"], ",")
  df[i, "numAtt1"] = str_count(df[i, "USER_ATTRIBUTE1"], ",")
  df[i, "numAtt2"] = str_count(df[i, "USER_ATTRIBUTE2"], ",")
  df[i, "numAtt3"] = str_count(df[i, "USER_ATTRIBUTE3"], ",")
  df[i, "numAtt4"] = str_count(df[i, "USER_ATTRIBUTE4"], ",")
  df[i, "numAtt5"] = str_count(df[i, "USER_ATTRIBUTE5"], ",")
}
for (i in 1:nrow(df)){
  if (df[i, "SIGNUP_YEAR"] == 2020){
    df[i, "month"] = df[i, "SIGNUP_MONTH"]
  }
  
  else{
    if (df[i, "SIGNUP_MONTH"] == 1){
      df[i, "month"] = 13
    } 
    if (df[i, "SIGNUP_MONTH"] == 2){
      df[i, "month"] = 14
    }
    if (df[i, "SIGNUP_MONTH"] == 3){
      df[i, "month"] = 15
    }
    if (df[i, "SIGNUP_MONTH"] == 4){
      df[i, "month"] = 16
    }
    if (df[i, "SIGNUP_MONTH"] == 5){
      df[i, "month"] = 17
    }
    if (df[i, "SIGNUP_MONTH"] == 6){
      df[i, "month"] = 18
    }
    if (df[i, "SIGNUP_MONTH"] == 7){
      df[i, "month"] = 19
    }
    if (df[i, "SIGNUP_MONTH"] == 8){
      df[i, "month"] = 20
    }
    if (df[i, "SIGNUP_MONTH"] == 9){
      df[i, "month"] = 21
    }
    if (df[i, "SIGNUP_MONTH"] == 10){
      df[i, "month"] = 22
    }
    if (df[i, "SIGNUP_MONTH"] == 11){
      df[i, "month"] = 23
    }
    if (df[i, "SIGNUP_MONTH"] == 12){
      df[i, "month"] = 24
    }
  }
}
df[, "sumAtt1"] = 0
for (i in 1:nrow(df)){
  if(str_detect(df[i, "USER_ATTRIBUTE1"], "1") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 1
  }
  if(str_detect(df[i, "USER_ATTRIBUTE1"], "2") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 2
  }
  if (str_detect(df[i, "USER_ATTRIBUTE1"], "3") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 3
  }
  if (str_detect(df[i, "USER_ATTRIBUTE1"], "4") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 4
  }
  if (str_detect(df[i, "USER_ATTRIBUTE1"], "5") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 5
  }
  if (str_detect(df[i, "USER_ATTRIBUTE1"], "6") == TRUE){
    df[i, "sumAtt1"] = df[i, "sumAtt1"] + 6
  }
}
df[, "sumAtt2"] = 0
for (i in 1:nrow(df)){
  if(str_detect(df[i, "USER_ATTRIBUTE2"], "10") == TRUE){
    df[i, "sumAtt2"] = df[i, "sumAtt2"] + 10
  }
  if(str_detect(df[i, "USER_ATTRIBUTE2"], "11") == TRUE){
    df[i, "sumAtt2"] = df[i, "sumAtt2"] + 11
  }
  if (str_detect(df[i, "USER_ATTRIBUTE2"], "9") == TRUE){
    df[i, "sumAtt2"] = df[i, "sumAtt2"] + 9
  }
  if (str_detect(df[i, "USER_ATTRIBUTE2"], "8") == TRUE){
    df[i, "sumAtt2"] = df[i, "sumAtt2"] + 8
  }
  if (str_detect(df[i, "USER_ATTRIBUTE2"], "7") == TRUE){
    df[i, "sumAtt2"] = df[i, "sumAtt2"] + 7
  }
}
df[, "sumAtt3"] = 0
for (i in 1:nrow(df)){
  if(str_detect(df[i, "USER_ATTRIBUTE3"], "12") == TRUE){
    df[i, "sumAtt3"] = df[i, "sumAtt3"] + 12
  }
  if(str_detect(df[i, "USER_ATTRIBUTE3"], "13") == TRUE){
    df[i, "sumAtt3"] = df[i, "sumAtt3"] + 13
  }
  if (str_detect(df[i, "USER_ATTRIBUTE3"], "14") == TRUE){
    df[i, "sumAtt3"] = df[i, "sumAtt3"] + 14
  }
  if (str_detect(df[i, "USER_ATTRIBUTE3"], "15") == TRUE){
    df[i, "sumAtt3"] = df[i, "sumAtt3"] + 15
  }
}

df[, "sumAtt4"] = 0
for (i in 1:nrow(df)){
  if(str_detect(df[i, "USER_ATTRIBUTE4"], "16") == TRUE){
    df[i, "sumAtt4"] = df[i, "sumAtt4"] + 16
  }
  if(str_detect(df[i, "USER_ATTRIBUTE4"], "17") == TRUE){
    df[i, "sumAtt4"] = df[i, "sumAtt4"] + 17
  }
  if (str_detect(df[i, "USER_ATTRIBUTE4"], "18") == TRUE){
    df[i, "sumAtt4"] = df[i, "sumAtt4"] + 18
  }
  if (str_detect(df[i, "USER_ATTRIBUTE4"], "19") == TRUE){
    df[i, "sumAtt4"] = df[i, "sumAtt4"] + 19
  }
  if (str_detect(df[i, "USER_ATTRIBUTE4"], "20") == TRUE){
    df[i, "sumAtt4"] = df[i, "sumAtt4"] + 20
  }
}

df[, "sumAtt5"] = 0
for (i in 1:nrow(df)){
  if(str_detect(df[i, "USER_ATTRIBUTE5"], "21") == TRUE){
    df[i, "sumAtt5"] = df[i, "sumAtt5"] + 21
  }
  if(str_detect(df[i, "USER_ATTRIBUTE5"], "22") == TRUE){
    df[i, "sumAtt5"] = df[i, "sumAtt5"] + 22
  }
  if (str_detect(df[i, "USER_ATTRIBUTE5"], "23") == TRUE){
    df[i, "sumAtt5"] = df[i, "sumAtt5"] + 23
  }
  if (str_detect(df[i, "USER_ATTRIBUTE5"], "24") == TRUE){
    df[i, "sumAtt5"] = df[i, "sumAtt5"] + 24
  }
  if (str_detect(df[i, "USER_ATTRIBUTE5"], "25") == TRUE){
    df[i, "sumAtt5"] = df[i, "sumAtt5"] + 25
  }
}

df$sumAttributes = df$sumAtt1 + df$sumAtt2 + df$sumAtt3 + df$sumAtt4 + df$sumAtt5

df = subset(df, select = -c(USER_OS,CARD_TYPE,PRODUCT_ADD_ONS))
nrow(test)
nrow(train)

dfTrain = df[1:97308,]
dfTest = as.data.frame(df[97309:150000,])
dfTest$LTV=rep(0,nrow(dfTest))
dfTrain = subset(dfTrain, select = -c(ZIPCODE))
dfTrain = subset(dfTrain, select = -c(USER_ATTRIBUTE1, USER_ATTRIBUTE2, USER_ATTRIBUTE3, USER_ATTRIBUTE4,
                                      USER_ATTRIBUTE5))
dfTest = subset(dfTest, select = -c(USER_ATTRIBUTE1, USER_ATTRIBUTE2, USER_ATTRIBUTE3, USER_ATTRIBUTE4,
                                    USER_ATTRIBUTE5))
dfTest = subset(dfTest, select = -c(ZIPCODE))
dfTrain = subset(dfTrain, select = -c(USER_DEVICE))
dfTest = subset(dfTest, select = -c(USER_DEVICE))
dfTrain = cbind(dfTrain,train$LTV)
library(dplyr)
dfTrain = rename(dfTrain, "LTV" = "train$LTV")

colSums(is.na(dfTrain))
library(tidyverse)
dfTrain = na.omit(dfTrain)
dfTrain$signUpSqt = sqrt(dfTrain$USER_SIGNUP_TIME)
dfTrain$signUpZero = ifelse(dfTrain$USER_SIGNUP_TIME == 0, 1,0)
dfTest$signUpSqt = sqrt(dfTest$USER_SIGNUP_TIME)
dfTest$signUpZero = ifelse(dfTest$USER_SIGNUP_TIME == 0, 1, 0)

for (i in 1:nrow(dfTrain)){
  if (dfTrain[i, "LTV"] > 0){
    dfTrain[i, "logitLTV"] = 1
  } else{
    dfTrain[i, "logitLTV"] = 0
  }
}
dfTest$logitLTV = 0

logModel = glm(logitLTV ~.-ID-credit-debit-prepaid-paypal-card-apple-socialMedia-unknown
               -TV-ad-reference-oldFashion-sample-other-safari-gsa-otherOS-PRODUCT
               -USER_SIGNUP_TIME-sumAttributes-SIGNUP_MONTH-SIGNUP_YEAR-LTV-firefox
               -facebook-edge, data=dfTrain,
               family = "binomial")
summary(logModel)
predLogit = predict(logModel, dfTrain, type = "response")
logitTab = table(dfTrain$logitLTV, predLogit > 0.5)
accuracyLog = sum(diag(logitTab)) / sum(logitTab)

library(rpart)
log.tree = rpart(logitLTV ~.-ID-credit-debit-prepaid-paypal-card-apple-socialMedia-unknown
                 -TV-ad-reference-oldFashion-sample-other-safari-gsa-otherOS-PRODUCT
                 -USER_SIGNUP_TIME-sumAttributes-SIGNUP_MONTH-SIGNUP_YEAR-LTV-firefox
                 -facebook-edge, data=dfTrain, method = 'class')
predTree = predict(log.tree, dfTrain, type="class")
tableTree = table(dfTrain$logitLTV, predTree)
accuracyTree = sum(diag(tableTree)) / sum(tableTree)

predLogitTest = predict(log.tree, dfTest, type = 'class')
dfTest$logitLTV = predLogitTest

split_dummy = sample(c(rep(0, 0.7 * nrow(dfTrain)),
                       rep(1, 0.3 * nrow(dfTrain))))
dfTrainTrain = dfTrain[split_dummy == 0,]
dfTrainTest = dfTrain[split_dummy == 1, ]

#Try gradient boosting
require(gbm)
train.boost = gbm(LTV~.-ID-credit-debit-prepaid-paypal-card-apple-socialMedia-unknown
                  -TV-ad-reference-oldFashion-sample-other-safari-gsa-otherOS-PRODUCT-logitLTV
                  -USER_SIGNUP_TIME-sumAttributes-SIGNUP_MONTH-SIGNUP_YEAR, data=dfTrainTrain,
                  distribution = "gaussian", n.trees = 1000,
                  shrinkage = 0.01, interaction.depth = 5)

summary(train.boost)
predTrBoost = predict(train.boost, dfTrainTest)
for (i in 1:length(predTrBoost)){
  if (predTrBoost[i] < 20){
    predTrBoost[i] = 0
  }
}

predTest3 = predict(train.boost, dfTest)
summary(predTest3)
for (i in 1:length(predTest3)){
  if (predTest3[i] < 30){
    predTest3[i] = 0
  }
}

predTest2 = predict(regModel2, dfTest)
summary(predTest2)
predTest2 = as.data.frame(predTest2)
for (i in 1:nrow(predTest2)){
  if (predTest2[i, 1] < 30){
    predTest2[i,1] = 0
  }
}

full.boost = gbm(LTV ~.-ID-credit-debit-prepaid-paypal-card-apple-socialMedia-unknown
                 -TV-ad-reference-oldFashion-sample-other-safari-gsa-otherOS-PRODUCT
                 -USER_SIGNUP_TIME-sumAttributes-SIGNUP_MONTH-SIGNUP_YEAR-signUpZero
                 -edge-facebook, data=dfTrain,
                 distribution = "gaussian", n.trees = 2000,
                 shrinkage = 0.01, interaction.depth = 3, cv.folds = 5)
print(full.boost)
summary(full.boost)
predFullBoost = predict(full.boost, dfTrain)
for (i in 1:length(predFullBoost)){
  if (predFullBoost[i] < 30){
    predFullBoost[i] = 0
  }
}
library(Metrics)
rmse(dfTrain$LTV, predFullBoost)
predTest4 = predict(full.boost, dfTrain)
for (i in 1:length(predTest4)){
  if (predTest4[i] < 20){
    predTest4[i] = 0
  }
}


predTest3 = predict(train.boost, dfTest)
summary(predTest3)
for (i in 1:length(predTest3)){
  if (predTest3[i] < 30){
    predTest3[i] = 0
  }
}


Predicted = cbind(dfTest$ID, predTest3)

colnames(Predicted) = c("ID", "Predicted")
write.csv(Predicted, "C:\\Users\\boydd\\OneDrive\\Documents\\R\\Predicted.csv", row.names = F)
