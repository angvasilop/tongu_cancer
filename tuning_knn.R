# prepare data
data<-read.csv("~/Downloads/tongue.csv")[,-1]
data$stage<-ifelse(grepl("4",data$stage),4,data$stage)
data$gender<-ifelse(data$gender=="M",0,1)
data[,c(1,5,6,8)]<-NULL
data[,-2]<-lapply(data[,-2],as.factor)
for(i in 1:9){
  data[,i]<-as.numeric(data[,i])
}
normalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}
for(i in 1:9){
  data[,i]<-normalize(data[,i])
}

# load libraries
library(class)
library(PRROC)

# tuning with k-fold cross validation
set.seed(200)
index<-sample(1:nrow(data))
groups<-cut(1:nrow(data),5,labels=FALSE)
folds<-split(index,groups)
auprc<-c()
all.auprc<-c()
for(j in 1:300){
  for(k in 1:5){
    set.seed(200)
    test<-data[unlist(folds[k]),]
    train<-data[-unlist(folds[k]),]
    model<-knn(train[,-3],test[,-3],train$survival,k=j,prob=TRUE)
    true<-test$survival
    pred<-model
    auprc[k]<-pr.curve(scores.class0=attributes(model)$prob[true==1],
                       scores.class1=attributes(model)$prob[true==0],curve=TRUE)$auc.integral
  }
  all.auprc[j]<-mean(auprc)
}

which.max(all.auprc)
max(all.auprc)