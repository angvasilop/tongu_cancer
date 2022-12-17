# read data
data<-read.csv("~/Downloads/tongue.csv")[,-1]
data$stage<-ifelse(grepl("4",data$stage),4,data$stage)
data[,c(1,5,6,8)]<-NULL
data[,-2]<-lapply(data[,-2],as.factor)
data$survival<-as.factor(data$survival)

# load libraries
library(randomForest)
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
  model<-randomForest(survival~.,data=train,
                      ntree=100,mtry=1,nodesize=3)
  true<-test$survival
  pred<-predict(model,test,type="prob")
  auprc.def<-pr.curve(scores.class0=pred[,2][true==1],
                      scores.class1=pred[,2][true==0],curve=TRUE)$auc.integral
  for(j in c(1,2,4,5,6,7,8,9)){
    for(i in 1:100){
      set.seed(i*j*k)
      train[,j]<-sample(train[,j],size=length(train[,j]))
      model<-randomForest(survival~.,data=train,
                          ntree=100,mtry=1,nodesize=3)
      true<-test$survival
      pred<-predict(model,test,type="prob")
      auprc.spec<-pr.curve(scores.class0=pred[,2][true==1],
                           scores.class1=pred[,2][true==0],curve=TRUE)$auc.integral
      auprc.dec[i]<-auprc.def-auprc.spec
    }
    auprc.varmean[k,j]<-mean(auprc.dec)
  }
}

# order raw importance
importance.rf<-data.frame(features=names(data[,-3]),
                          mdauprc=na.omit(colMeans(auprc.varmean)))
importance.rf<-importance.rf[order(importance.rf$mdauprc,decreasing=TRUE),]

# normalize ordered importance
norm.importance.rf<-data.frame(features=importance.rf$features,
                               mdauprc=importance.rf$mdauprc/max(importance.rf$mdauprc))

# save results
saveRDS(importance.rf,file="importance.rf.RDS")