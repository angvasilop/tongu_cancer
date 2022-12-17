# prepare data
data<-as.matrix(read.csv("~/Downloads/tongue.csv"))[,-1]
data[,7]<-ifelse(grepl("4",data[,7]),4,data[,7])
data<-data[,-c(1,5,6,8)]
data[,1]<-ifelse(data[,1]=="M",0,1)
data<-apply(data,2,as.numeric)
colnames(data)<-NULL
data<-cbind(data,NA,NA)

# load libraries
library(glmnet)
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
    model<-glmnet(train[,-3],train[,3],family="binomial",
                  alpha=0,lamda=0.02)
    pred<-ifelse(predict(model,test[,-3],s=0.02,type="response")>0.5,1,0)
    
    acc[k]<-mean(pred==test[,3])
    mtx<-table(true=factor(test[,3],levels=0:1),pred=factor(pred,levels=0:1))
    prec[k]<-mtx[2,2]/(mtx[2,2]+mtx[1,2])
    recall[k]<-mtx[2,2]/(mtx[2,2]+mtx[2,1])
    tnr[k]<-mtx[1,1]/(mtx[1,1]+mtx[1,2])
    mcc[k]<-mcc(confusionM=matrix(c(mtx[1,1],mtx[1,2],mtx[2,1],mtx[2,2]),nrow=2))
    auprc[k]<-pr.curve(scores.class0=predict(model,test[,-3])[test[,3]==1],
                       scores.class1=predict(model,test[,-3])[test[,3]==0])$auc.integral
  }
  acc.mean[i]<-mean(acc)
  prec.mean[i]<-mean(prec)
  recall.mean[i]<-mean(recall)
  tnr.mean[i]<-mean(tnr)
  mcc.mean[i]<-mean(mcc)
  auprc.mean[i]<-mean(auprc)
}
results.bootstrap<-data.frame(acc=acc.mean,prec=prec.mean,recall=recall.mean,tnr=tnr.mean,mcc=mcc.mean,auprc=auprc.mean)
sapply(results.bootstrap,mean)
saveRDS(results.bootstrap,"inner.results.bootstrap.glmnet.RDS")