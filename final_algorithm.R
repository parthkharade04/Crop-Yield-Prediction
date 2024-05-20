f1=read.csv("crop_prediction.csv.csv")


#installing packages
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(Metrics)
library(randomForest)
library(dplyr)
library(stats)
library(caret)
library(xgboost)
#library(caret)
#library(FSelector)
#data<-lapply(data,as.numeric)
#str(data)

#data visualisation
dim(f1)
head(f1)
sum(duplicated(f1))
data<-subset(f1,!duplicated(f1))
data<-subset(f1,select=-c(1))
sum(is.na(f1))
summary(f1)

#f1=data_test_mean
#f1 = read.csv("main_dataset.csv")
#f2<-read.csv("main_dataset.csv")

#normalization
f1_new<-f1[ ,1:14]
n<- function(b){
  (b-min(b)) / (max(b) - min(b))
}
f2 <- as.data.frame(lapply(f1_new,n))
f3 <- cbind(f2,Production = f1$Production)

df2 <- f1[,!names(f1) %in% c( "District_Name", "Season","Crop")]
boxplot(df2)
is.na(f1)
summary(f1)

#splitting
set.seed(1234)
sample_data = sample.split(f1, SplitRatio = 0.8)
train_data <- subset(f1, sample_data == TRUE)
test_data <- subset(f1, sample_data == FALSE)

#linear model
l1<-lm(f1$Production ~ f1$Crop+f1$Area+f1$pH_min+f1$pH_max+f1$pH_Req+f1$min_temp+f1$min_rainfall+f1$max_rainfall+f1$pH_min_Range+f1$pH_max_Range,data=train_data)
s1<-summary(l1)
print(s1)

#Decision tree

#train the decision tree
model1 = rpart(Production ~ Season+Crop+Area+pH_min_Range+pH_max_Range+pH_min+pH_max+pH_Req+min_temp+max_temp+min_rainfall+max_rainfall, data=train_data)
#value
a=data.frame(Season=c(3),Crop=c(1),Area=c(6200),pH_min_Range=c(5.8),pH_max_Range=c(6.8),pH_min=c(7),pH_max=c(7.5),pH_Req=c(6.3),min_temp=c(18),max_temp=c(28),min_rainfall=c(45),max_rainfall=c(100))
result=predict(model1,a)
print(result)

#tree diagram
rpart.plot(model1)

#prediction
pred1 = predict(model1, train_data)

#R square
SSR_DT <- sum((pred1 - mean(f1$Production)))
SSE_DT <- sum((f1$Production - pred1))
R2_DT <- (SSR_DT)/(SSR_DT+SSE_DT)
print(R2_DT)

#rmse
rmse<-sqrt(mean((f1$Production - pred1)^2))
print(rmse)

#mae
mae<-mean(abs(f1$Production - pred1))
print(mae)

#Random Forest

str(f1)

#train rf
RFM = randomForest(Production~ Season+Crop+Area+pH_min_Range+pH_max_Range+pH_min+pH_max+pH_Req+min_temp+max_temp+min_rainfall+max_rainfall,data=train_data)
print(RFM)
#value
a=data.frame(Season=c(3),Crop=c(1),Area=c(6200),pH_min_Range=c(5.8),pH_max_Range=c(6.8),pH_min=c(7),pH_max=c(7.5),pH_Req=c(6.3),min_temp=c(18),max_temp=c(28),min_rainfall=c(45),max_rainfall=c(100))
result=predict(RFM,a)
print(result)


#prediction
pred2 = predict(RFM, train_data)

#R square
SSR_RF <- sum((pred2 - mean(f1$Production)))
SSE_RF <- sum((f1$Production - pred2))
R2_RF <- (SSR_RF)/(SSR_RF+SSE_RF)
print(R2_RF)

#rmse
rmse2<-sqrt(mean((f1$Production - pred2)^2))
print(rmse2)

#mae
mae2<-mean(abs(f1$Production - pred2))
print(mae2)

#xgboost
summary(f1)
head(f1)
sapply(f1,class)

#f1 <- f1[,!names(f1) %in% c( "District_Name", "Season","Crop")]

f1[]<-sapply(f1,as.numeric)
set.seed(678)
indices<-createDataPartition(f1$Production,p=0.8,list=FALSE)
train_new<-f1[indices,]
test_new<-f1[indices,]

grid_tune<-expand.grid(
  nrounds=c(500,1000,1500),
  max_depth=c(2,4,6),
  eta=0.3,
  gamma=0,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1
)
train_control<-trainControl(method="cv",number=3,verboseIter=TRUE,allowParallel=TRUE)
xgb_tune<-train(x=train_new[,-15],
                y=train_new[,15],
                trControl=train_control,
                tuneGrid=grid_tune,
                method="xgbTree",
                verbose=TRUE)
print(xgb_tune)
#a=data.frame(Season=c(3),Crop=c(1),Area=c(6200),pH_min_Range=c(5.8),pH_max_Range=c(6.8),pH_min=c(7),pH_max=c(7.5),pH_Req=c(6.3),min_temp=c(18),max_temp=c(28),min_rainfall=c(45),max_rainfall=c(100))
#result=predict(xgb_tune,a)
#print(result)

#predict(xgb_tune,test_data)
pred3<-predict(xgb_tune,train_new)

#R square
SSR_XG <- sum((pred3 - mean(f1$Production)))
SSE_XG <- sum((f1$Production - pred3))
R2_XG <- (SSR_XG)/(SSR_XG+SSE_XG)
print(R2_XG)

#rmse
rmse3<-sqrt(mean((f1$Production - pred3)^2))
print(rmse3)

#mae
mae3<-mean(abs(f1$Production - pred3))
print(mae3)



