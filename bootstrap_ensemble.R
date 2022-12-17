# rf data
data.rf<-read.csv("~/Downloads/tongue.csv")[,-1]
data.rf$stage<-ifelse(grepl("4",data.rf$stage),4,data.rf$stage)
data.rf[,c(1,5,6,8)]<-NULL
data.rf[,-2]<-lapply(data.rf[,-2],as.factor)
data.rf$survival<-as.factor(data.rf$survival)

# knn data
data.knn<-read.csv("~/Downloads/tongue_cancer.csv")
data.knn$stage<-ifelse(grepl("4",data.knn$stage),4,data.knn$stage)
data.knn$gender<-ifelse(data.knn$gender=="M",0,1)
data.knn[,c(1,5,6,8)]<-NULL
data.knn[,-2]<-lapply(data.knn[,-2],as.factor)
for(i in 1:9){
  data.knn[,i]<-as.numeric(data.knn[,i])
}
normalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}
for(i in 1:9){
  data.knn[,i]<-normalize(data.knn[,i])
}

# xgboost data
data.xgboost<-as.matrix(read.csv("~/Downloads/tongue_cancer.csv"))
data.xgboost[,7]<-ifelse(grepl("4",data.xgboost[,7]),4,data.xgboost[,7])
data.xgboost<-data.xgboost[,-c(1,5,6,8)]
data.xgboost[,1]<-ifelse(data.xgboost[,1]=="M",0,1)
data.xgboost<-apply(data.xgboost,2,as.numeric)
colnames(data.xgboost)<-NULL

# glmnet data
data.glmnet<-as.matrix(read.csv("~/Downloads/tongue_cancer.csv"))
data.glmnet[,7]<-ifelse(grepl("4",data.glmnet[,7]),4,data.glmnet[,7])
data.glmnet<-data.glmnet[,-c(1,5,6,8)]
data.glmnet[,1]<-ifelse(data.glmnet[,1]=="M",0,1)
data.glmnet<-apply(data.glmnet,2,as.numeric)
colnames(data.glmnet)<-NULL

# load libraries
library(mltools)
library(PRROC)
library(class)
library(glmnet)
library(randomForest)
library(xgboost)

# create vectors
acc<-c()
auprc<-c()
prec<-c()
recall<-c()
tnr<-c()
mcc<-c()
acc.mean<-c()
auprc.mean<-c()
prec.mean<-c()
recall.mean<-c()
tnr.mean<-c()
mcc.mean<-c()

# bootstrapping with metric calculations in inner loop
set.seed(2022)
for(i in 1:1000){print(i)
  groups<-cut(1:nrow(data),5,labels=FALSE)
  index<-sample(1:nrow(data))
  folds<-split(index,groups)
  for(k in 1:5){
    test.rf<-data.rf[unlist(folds[k]),]
    train.rf<-data.rf[-unlist(folds[k]),]  
    model.rf<-randomForest(survival~.,data=train.rf,
                           ntree=100,mtry=1,nodesize=3)
    pred.rf<-as.vector(predict(model.rf,test.rf,type="prob")[,2])
    
    test.knn<-data.knn[unlist(folds[k]),]
    train.knn<-data.knn[-unlist(folds[k]),]  
    model.knn<-knn(train.knn[,-3],test.knn[,-3],train.knn$survival,k=85,prob=TRUE)
    pred.knn<-attributes(model.knn)$prob
    
    test.xgboost<-data.xgboost[unlist(folds[k]),]
    train.xgboost<-data.xgboost[-unlist(folds[k]),]  
    model.xgboost<-xgboost(train.xgboost[,-3],train.xgboost[,3],objective="binary:logistic",
                           nrounds=7,max.depth=2,eta=0.5)
    pred.xgboost<-predict(model.xgboost,test.xgboost[,-3],type="prob")
    
    test.glmnet<-data.glmnet[unlist(folds[k]),]
    train.glmnet<-data.glmnet[-unlist(folds[k]),]
    model.glmnet<-glmnet(train.glmnet[,-3],train.glmnet[,3],family="binomial",
                         alpha=1,lamda=0.02)
    pred.glmnet<-predict(model.glmnet,test.glmnet[,-3],s=0.02,type="response")
    
    prob<-(as.numeric(pred.rf)+as.numeric(pred.knn)+as.numeric(pred.xgboost)+as.numeric(pred.glmnet))/4
    pred<-ifelse(prob>0.5,1,0)
    
    acc[k]<-mean(pred==test.rf[,3])
    mtx<-table(true=factor(test.rf[,3],levels=0:1),pred=factor(pred,levels=0:1))
    prec[k]<-mtx[2,2]/(mtx[2,2]+mtx[1,2])
    recall[k]<-mtx[2,2]/(mtx[2,2]+mtx[2,1])
    tnr[k]<-mtx[1,1]/(mtx[1,1]+mtx[1,2])
    mcc[k]<-mcc(confusionM=matrix(c(mtx[1,1],mtx[1,2],mtx[2,1],mtx[2,2]),nrow=2))
    auprc[k]<-pr.curve(scores.class0=prob[test.rf[,3]==1],
                       scores.class1=prob[test.rf[,3]==0])$auc.integral
  }
  acc.mean[i]<-mean(acc)
  prec.mean[i]<-mean(prec)
  recall.mean[i]<-mean(recall)
  tnr.mean[i]<-mean(tnr)
  mcc.mean[i]<-mean(mcc)
  auprc.mean[i]<-mean(auprc)
}
results.bootstrap<-data.frame(acc=acc.mean,prec=prec.mean,recall=recall.mean,tnr=tnr.mean,mcc=mcc.mean,auprc=auprc.mean)
saveRDS(results.bootstrap,"inner.results.bootstrap.ensemble.RDS")