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

# permutation predictor importance with k-fold cross validation
set.seed(200)
index<-sample(1:nrow(data))
groups<-cut(1:nrow(data),5,labels=FALSE)
folds<-split(index,groups)
auprc.def<-c()
auprc.perm<-c()
auprc.dec<-c()
auprc.varmean<-matrix(nrow=5,ncol=9)
for(k in 1:5){
  set.seed(200)
  test<-data[unlist(folds[k]),]
  train<-data[-unlist(folds[k]),]
  model<-knn(train[,-3],test[,-3],train$survival,k=85,prob=TRUE)
  true<-test$survival
  pred<-model
  auprc.def<-pr.curve(scores.class0=attributes(model)$prob[true==1],
                      scores.class1=attributes(model)$prob[true==0],curve=TRUE)$auc.integral
  for(j in c(1,2,4,5,6,7,8,9)){
    for(i in 1:100){
      set.seed(i*j*k)
      train[,j]<-sample(train[,j],size=length(train[,j]))
      model<-knn(train[,-3],test[,-3],train$survival,k=85,prob=TRUE)
      true<-test$survival
      pred<-model
      auprc.spec<-pr.curve(scores.class0=attributes(model)$prob[true==1],
                           scores.class1=attributes(model)$prob[true==0],curve=TRUE)$auc.integral
      auprc.dec[i]<-auprc.def-auprc.spec
    }
    auprc.varmean[k,j]<-mean(auprc.dec)
  }
}

# order raw importance
importance.knn<-data.frame(features=names(data[,-3]),
                           mdauprc=na.omit(colMeans(auprc.varmean)))
importance.knn<-importance.knn[order(importance.knn$mdauprc,decreasing=TRUE),]

# normalize ordered importance
norm.importance.knn<-data.frame(features=importance.knn$features,
                                mdauprc=importance.knn$mdauprc/max(importance.knn$mdauprc))

saveRDS(importance.knn,"importance.knn.RDS")