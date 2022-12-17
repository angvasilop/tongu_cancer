# prepare data
data<-as.matrix(read.csv("~/Downloads/tongue.csv"))[,-1]
data[,7]<-ifelse(grepl("4",data[,7]),4,data[,7])
data<-data[,-c(1,5,6,8)]
data[,1]<-ifelse(data[,1]=="M",0,1)
data<-apply(data,2,as.numeric)
colnames(data)<-NULL

# load libraries
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
auprc.spec<-c()
for(k in 1:5){
  set.seed(200)
  test<-data[unlist(folds[k]),]
  train<-data[-unlist(folds[k]),]
  model<-xgboost(train[,-3],train[,3],objective="binary:logistic",
                 nrounds=7,max.depth=2,eta=0.5)
  true<-test[,3]
  pred<-predict(model,test[,-3])
  auprc.def<-pr.curve(scores.class0=pred[true==1],
                      scores.class1=pred[true==0],curve=TRUE)$auc.integral
  for(j in c(1,2,4,5,6,7,8,9)){
    for(i in 1:100){
      set.seed(i*j*k)
      train[,j]<-sample(train[,j],size=length(train[,j]))
      model<-xgboost(train[,-3],train[,3],objective="binary:logistic",
                     nrounds=7,max.depth=2,eta=0.5)
      true<-test[,3]
      pred<-predict(model,test[,-3])
      auprc.spec<-pr.curve(scores.class0=pred[true==1],
                           scores.class1=pred[true==0],curve=TRUE)$auc.integral
      auprc.dec[i]<-auprc.def-auprc.spec
    }
    auprc.varmean[k,j]<-mean(auprc.dec)
  }
}

# order raw importance
importance.xgboost<-data.frame(features=names(data[,-3]),
                               mdauprc=na.omit(colMeans(auprc.varmean)))
importance.xgboost<-importance.rf[order(importance.xgboost$mdauprc,decreasing=TRUE),]

# normalize ordered importance
norm.importance.xgboost<-data.frame(features=importance.rf$features,
                                    mdauprc=importance.xgboost$mdauprc/max(importance.xgboost$mdauprc))

# save results
saveRDS(importance.xgboost,file="importance.xgboost.RDS")