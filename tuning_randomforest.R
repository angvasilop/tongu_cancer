# prepare data
data<-read.csv("~/Downloads/tongue.csv")[,-1]
data$stage<-ifelse(grepl("4",data$stage),4,data$stage)
data[,c(1,5,6,8)]<-NULL
data[,-2]<-lapply(data[,-2],as.factor)
data$survival<-as.factor(data$survival)

# load libraries
library(randomForest)
library(PRROC)

# tuning with 5-fold cross validation
set.seed(200)
index<-sample(1:nrow(data))
groups<-cut(1:nrow(data),5,labels=FALSE)
folds<-split(index,groups)
all.auprc<-array(dim=c(10,5,10))
auprc<-c()
for(j in c(1000,1100,1200,1300,1400,1500,1600,1700,1800,1900)){
  for(i in 1:5){
    for(h in 1:10){
      for(k in 1:5){
        test<-data[unlist(folds[k]),]
        train<-data[-unlist(folds[k]),]
        set.seed(200)
        model<-randomForest(survival~grade+n_stage+t_stage,data=train,
                            ntree=j,mtry=i,nodesize=h)
        true<-test$survival
        pred<-predict(model,test)
        auprc[k]<-pr.curve(scores.class0=predict(model,test,type="prob")[,2][true==1],
                           scores.class1=predict(model,test,type="prob")[,2][true==0],curve=TRUE)$auc.integral
      }
      all.auprc[j/100-9,i,h]<-mean(auprc)
    }
  }
}
saveRDS(all.auprc,"rf.all.auprc.3.RDS")
max(all.auprc)
which(all.auprc==max(all.auprc),arr.ind=TRUE)
all.auprc[1,1,3]