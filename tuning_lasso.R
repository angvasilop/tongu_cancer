# prepare data
data<-as.matrix(read.csv("~/Downloads/tongue_cancer.csv"))
data[,7]<-ifelse(grepl("4",data[,7]),4,data[,7])
data<-data[,-c(1,5,6,8)]
data[,1]<-ifelse(data[,1]=="M",0,1)
data<-apply(data,2,as.numeric)
colnames(data)<-NULL

# load libraries
library(glmnet)
library(PRROC)

# tuning with k-fold cross validation
set.seed(200)
index<-sample(1:nrow(data))
groups<-cut(1:nrow(data),5,labels=FALSE)
folds<-split(index,groups)
all.auprc<-c()
auprc<-c()
for(j in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)){
  for(k in 1:5){
    test<-data[unlist(folds[k]),]
    train<-data[-unlist(folds[k]),]
    model<-glmnet(train[,-3],train[,3],family="binomial",
                  alpha=1,lamda=j)
    true<-test[,3]
    pred<-predict(model,test[,-3],s=j,type="response")
    auprc[k]<-pr.curve(scores.class0=pred[true==1],
                       scores.class1=pred[true==0])$auc.integral
  }
  all.auprc[j*10]<-mean(auprc)
}

max(all.auprc)
which(all.auprc==max(all.auprc),arr.ind=TRUE)