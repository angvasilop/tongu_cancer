# prepare data
data<-as.matrix(read.csv("~/Downloads/tongue.csv"))[,-1]
data[,7]<-ifelse(grepl("4",data[,7]),4,data[,7])
data<-data[,-c(1,5,6,8)]
data[,1]<-ifelse(data[,1]=="M",0,1)
data<-apply(data,2,as.numeric)
colnames(data)<-NULL

# load libraries
library(xgboost)
library(mltools)
library(PRROC)

# tuning with k-fold cross validation
set.seed(200)
index<-sample(1:nrow(data))
groups<-cut(1:nrow(data),5,labels=FALSE)
folds<-split(index,groups)
all.auprc<-array(dim=c(9,10,10))
auprc<-c()
for(j in 2:10){
  for(i in 1:10){
    for(h in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)){
      for(k in 1:5){
        test<-data[unlist(folds[k]),]
        train<-data[-unlist(folds[k]),]
        set.seed(200)
        model<-xgboost(train[,-3],train[,3],objective="binary:logistic",
                       nrounds=j,max.depth=i,eta=h)
        true<-test[,3]
        pred<-ifelse(predict(model,test[,-3])>0.5,1,0)
        auprc[k]<-pr.curve(scores.class0=predict(model,test[,-3],type="prob")[true==1],
                           scores.class1=predict(model,test[,-3],type="prob")[true==0],curve=TRUE)$auc.integral
      }
      all.auprc[j-1,i,h*10]<-mean(auprc)
    }
  }
}

max(all.auprc)
which(all.auprc==max(all.auprc),arr.ind=TRUE)
all.auprc[7,2,5]
