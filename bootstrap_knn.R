# read data
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
library(mltools)
library(PRROC)

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
    test<-data[unlist(folds[k]),]
    train<-data[-unlist(folds[k]),]
    bootstrap<-sample(nrow(train),replace=TRUE)
    train<-train[bootstrap,]
    model<-knn(train[,-3],test[,-3],train[,3],
               k=85,prob=TRUE)
    pred<-model
    acc[k]<-mean(pred==test$survival)
    auprc[k]<-pr.curve(scores.class0=attributes(model)$prob[test[,3]==1],
                       scores.class1=attributes(model)$prob[test[,3]==0])$auc.integral
    mtx<-table(factor(test[,3],levels=0:1),factor(pred,levels=0:1))
    prec[k]<-mtx[2,2]/(mtx[2,2]+mtx[1,2])
    recall[k]<-mtx[2,2]/(mtx[2,2]+mtx[2,1])
    tnr[k]<-mtx[1,1]/(mtx[1,1]+mtx[1,2])
    mcc[k]<-mcc(confusionM=matrix(c(mtx[1,1],mtx[1,2],mtx[2,1],mtx[2,2]),nrow=2))
  }
  acc.mean[i]<-mean(acc)
  auprc.mean[i]<-mean(auprc)
  prec.mean[i]<-mean(prec)
  recall.mean[i]<-mean(recall)
  tnr.mean[i]<-mean(tnr)
  mcc.mean[i]<-mean(mcc)
}
results.bootstrap<-data.frame(acc=acc.mean,prec=prec.mean,recall=recall.mean,tnr=tnr.mean,mcc=mcc.mean,auprc=auprc.mean)
sapply(results.bootstrap,mean)
saveRDS(results.bootstrap,"inner.results.bootstrap.knn.RDS")