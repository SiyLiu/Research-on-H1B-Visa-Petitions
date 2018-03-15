library(MASS)
library(pROC)
library(boot)
library(ROCR)
library(Deducer)

train%>%filter(!is.na())
write.csv(h1b_data, "L:\\AFTER\\GoForIt\\cleaned.csv")
h1b.data = read_csv("L:\\AFTER\\GoForIt\\cleaned.csv")
h1b.data = h1b.data[,-2]
# Address the missing value
h1b.data$SOC_NAME = ifelse(!is.na(h1b.data$SOC_NAME), h1b.data$SOC_NAME, "undefined")
h1b.data$PREVAILING_WAGE[is.na(h1b.data$PREVAILING_WAGE)] = 
  median(na.omit(h1b.data$PREVAILING_WAGE[(h1b.data$SOC_NAME==h1b.data$SOC_NAME[is.na(h1b.data$PREVAILING_WAGE)] & 
                             h1b.data$WORKSITE==h1b.data$WORKSITE[is.na(h1b.data$PREVAILING_WAGE)])]))
h1b.data$log_wage = log(h1b.data$PREVAILING_WAGE)
h1b.data = h1b.data[!is.na(h1b.data$EMPLOYER_NAME) & !is.na(h1b.data$JOB_TITLE) & !is.na(h1b.data$FULL_TIME_POSITION),] 
dim(h1b.data)
h1b.data$CASE_STATUS=as.factor(h1b.data$CASE_STATUS)
h1b.data$YEAR = as.factor(h1b.data$YEAR)


# ind = sample(1:nrow(h1b_data),2000000) 
# train = h1b_data[ind,]
# sum(train$CASE_STATUS=="1")/nrow(train)#8979725
# 
# test = h1b_data[-ind,]
# sum(test$CASE_STATUS=="1")/nrow(test)#0.8981408
# 
# train_1<-file('L:\\AFTER\\GoForIt\\train_1.csv',encoding="UTF-8")
# write.csv(train, file=train_1)
# test_1 = file('L:\\AFTER\\GoForIt\\test_1.csv',encoding = "UTF-8")
# write.csv(test,file = test_1)
# 
# # Due to the capability of the PC, I decide to use the undersampling method to deal
# # with the imbalanced dataset
# h1b_data = read_csv("L:\\AFTER\\GoForIt\\cleaned.csv")
# h1b_data = h1b_data[!is.na(h1b_data$State),]
# h1b_data = h1b_data[!is.na(h1b_data$City),]
# 
# train = read_csv("L:\\AFTER\\GoForIt\\train_1.csv")
# test = read_csv("L:\\AFTER\\GoForIt\\test_1.csv")
# 
# train$FULL_TIME_POSITION=ifelse(train$FULL_TIME_POSITION=="Y",1,0)
# test$FULL_TIME_POSITION= ifelse(test$FULL_TIME_POSITION=="Y",1,0)
# 
# train$FULL_TIME_POSITION= as.factor(train$FULL_TIME_POSITION)
# test$FULL_TIME_POSITION= as.factor(test$FULL_TIME_POSITION)
# train$CASE_STATUS = as.factor(train$CASE_STATUS)
# test$CASE_STATUS = as.factor(test$CASE_STATUS)
# train$YEAR= as.factor(train$YEAR)
# test$YEAR = as.factor(test$YEAR)
# 
# train = train[,c(-1,-9,-10,-11)]
# positive_sample = sample(which(train$CASE_STATUS=="1"), size = nrow(test))
# negative = which(train$CASE_STATUS=="0")
# new_ind = c(positive_sample, negative)
# sample1 = train_1[new_ind,]
# train_1 = train[,c(-2,-3,-4)]
# # Logistic
# LR = glm(CASE_STATUS~., data = sample1, family = "binomial")
# test_1 = test[,c(-1,-2,-4,-5,-6,-11,-12,-10)]
# pre = predict(LR, test ,type = "response")

## MAKE a demo
## Sampling

sample_ind = sample(nrow(h1b_data),size = 3000)
sample_data = h1b_data[sample_ind,]
sample_data = sample_data[,-c(1,2)]
sample_data$YEAR = as.factor(sample_data$YEAR)
sample_data$FULL_TIME_POSITION = as.factor(sample_data$FULL_TIME_POSITION)

train_ind = sample(3000,2500)
training = sample_data[train_ind,] #0.9094444
testing = sample_data[-train_ind,] #0.8971429
training = training[,-c(2,3,4,8,9,10,11,13)]
testing = testing[,-c(2,3,4,8,9,10,11,13)]
training = as.data.frame(training)
testing = as.data.frame(testing)
training$CASE_STATUS = as.factor(training$CASE_STATUS)
testing$CASE_STATUS = as.factor(testing$CASE_STATUS)
training$State = as.factor(training$State)
testing$State =as.factor(testing$State)
sum(training$CASE_STATUS=="1")/nrow(training)
sum(testing$CASE_STATUS=="1")/nrow(testing)
## Oversampling
library(DMwR)
over_train = SMOTE(CASE_STATUS~.,training,perc.over=900,perc.under=100)
table(over_train$CASE_STATUS)
#    0    1 
#  1780 1602 

#Logistic Regression
LR= glm(CASE_STATUS~.-log_wage, data= over_train, family = "binomial")
LR.pre = predict(LR, newdata = testing,type = "response")

LR.roc = roc(testing$CASE_STATUS,LR.pre) 
plot(LR.roc, col="red")

# Cross-Validation
over_train['log_wage']=log(over_train$PREVAILING_WAGE)
over_t = over_train[,-10]
fold = sample(1:nrow(over_train), nrow(over_train))
k = 10
len = floor(nrow(over_train)/k)
pd.auc = matrix(0,1,k)
lr.res = rep(0,nrow(over_train))
lr.cutoff = rep(0,k)
for (i in 1:k){
  ind = fold[(len*i-len+1) :(len*i)]
  train_ds = over_t[-ind,]
  test_ds = over_t[ind,]
  
  #model technique
  reg.k = glm(CASE_STATUS~., train_ds, family = "binomial")
  
  pre.k = predict(reg.k, newdata = test_ds, type = "response")
  lr.res[ind] = ifelse(pre.k>=0.5, 1,0)
  #pred.rocr <- prediction(pre.k, test_ds$CASE_STATUS)
  #perf.k= performance(pred.rocr,"tpr","fpr")
  #plot(perf.k, add = ifelse(i ==1, FALSE, TRUE))
  
  
  
  k.roc = roc(test_ds$CASE_STATUS, pre.k)
  #pd.auc[i] =auc(k.roc)
  lr.cutoff[i]=coords(k.roc, "best", ret = "threshold")
  
}
table(over_t$CASE_STATUS[order(fold)], lr.res)
# 0    1
# 0 1295 1275
# 1 1176 1137

# 0.7937787
# [,1]      [,2]      [,3]    [,4]      [,5]      [,6]      [,7]      [,8]
# [1,] 0.7985607 0.7707053 0.7822587 0.77541 0.8139168 0.8386158 0.8056012 0.7897892
# [,9]     [,10]
# [1,] 0.7829749 0.7799546


#Random Forest


library(caret)
library(pROC)
data(iris)

forest.model <- train(CASE_STATUS ~.-log_wage, over_train)
RF.model = randomForest(CASE_STATUS~.-log_wage,data=over_train,importance=TRUE)
RF.pre <- predict(forest.model, testing, type="prob") # Prediction

RF.auc = matrix(0,1,k)
RF.res = rep(0, 4883)
RF.cutoff = rep(0,k)
for (i in 1:k){
  ind = fold[(len*i-len+1) :(len*i)]
  train_ds = over_train[-ind,]
  test_ds = over_train[ind,]
  
  #model technique
  reg.k = randomForest(CASE_STATUS~., data = train_ds )
  pre.k = predict(reg.k, newdata = test_ds, type = "prob")
  RF.res[ind] = ifelse(pre.k[,2]>=0.5, 10,0)
  # pred.rocr <- prediction(pre.k[,2], test_ds$CASE_STATUS)
  # k.perf= performance(pred.rocr,"tpr","fpr")
  # plot(k.perf, add = TRUE, col="blue")
  k.roc = roc(test_ds$CASE_STATUS, pre.k[,2])
  # RF.auc[i] =auc(k.roc)
  RF.cutoff[i]=coords(k.roc, "best", ret = "threshold")
  
}









result.roc <- roc(testing$CASE_STATUS, RF.pre$`1`) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy

auc(result.roc)
# RF.res    0    1
# 0  1908 1708
# 10  662  605


#0.177 0.243 0.191 0.189 0.185 0.194 0.213 0.221 0.267 0.215
## Adaboost

Ada = boosting(CASE_STATUS~.-log_wage, over_train)
Ada.roc = roc(over_train$CASE_STATUS, Ada$prob[,2])
plot(Ada.roc, print.thres="best", print.thres.best.method="closest.topleft")
pre.ada = predict(Ada, newdata = testing)
pre.roc = roc(testing$CASE_STATUS,pre.ada$prob[,2])

ada.pd.auc = matrix(0,1,k)
Ada.res=rep(0,4883)
Ada.cutoff = rep(0,k)
for (i in 1:k){
  ind = fold[(len*i-len+1) :(len*i)]
  train_ds = over_train[-ind,]
  test_ds = over_train[ind,]
  
  #model technique
  reg.k = boosting(CASE_STATUS~., train_ds)
  pre.k = predict(reg.k, newdata = test_ds)
  Ada.res[ind] = ifelse(pre.k$prob[,2]>=0.5, 10,0)
  # pred.rocr <- prediction(pre.k$prob[,2], test_ds$CASE_STATUS)
  # k.perf= performance(pred.rocr,"tpr","fpr")
  # plot(k.perf, add = TRUE, col="red")
   k.roc = roc(test_ds$CASE_STATUS, pre.k$prob[,2])
  # ada.pd.auc[i] =auc(k.roc)
  Ada.cutoff[i]=coords(k.roc, "best", ret = "threshold")
  print(i)
  
}
table(over_train$CASE_STATUS[order(fold)], Ada.res)

# [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]
# [1,] 0.9610741 0.9710025 0.9553696 0.9645783 0.9781721 0.9604535 0.9679161 0.9626427
# [,9]     [,10]
# [1,] 0.9672751 0.9747539

Ada.res
# 0   10
# 0 1336 1234
# 1 1170 1143

library("ROCR")
