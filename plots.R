# bootstrap results
rf<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.rf.RDS")
knn<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.knn.RDS")
xgboost<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.xgboost.RDS")
glmnet<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.glmnet.RDS")
ensemble<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.ensemble.RDS")

acc<-data.frame(knn$acc,rf$acc,xgboost$acc,glmnet$acc,ensemble$acc)
prec<-data.frame(knn$prec,rf$prec,xgboost$prec,glmnet$prec,ensemble$prec)
recall<-data.frame(knn$recall,rf$recall,xgboost$recall,glmnet$recall,ensemble$recall)
tnr<-data.frame(knn$tnr,rf$tnr,xgboost$tnr,glmnet$tnr,ensemble$tnr)
mcc<-data.frame(knn$mcc,rf$mcc,xgboost$mcc,glmnet$mcc,ensemble$mcc)
auprc<-data.frame(knn$auprc,rf$auprc,xgboost$auprc,glmnet$auprc,ensemble$auprc)

results.bootstrap.all<-list(acc=acc,prec=prec,recall=recall,tnr=tnr,mcc=mcc,auprc=auprc)
saveRDS(results.bootstrap.all,"inner.results.bootstrap.all.RDS")

# read data
metrics<-readRDS("~/Documents/research/tongue/bootstrap/inner_loop_results/inner.results.bootstrap.all.RDS")
metrics[[5]]<-NULL

# construct confidence intervals
sapply(metrics$acc,quantile,p=c(0.025,0.975))
sapply(metrics$prec,quantile,p=c(0.025,0.975))
sapply(metrics$recall,quantile,p=c(0.025,0.975))
sapply(metrics$tnr,quantile,p=c(0.025,0.975))
sapply(metrics$auprc,quantile,p=c(0.025,0.975))

# calculate means
sapply(metrics$acc,mean)
sapply(metrics$prec,mean)
sapply(metrics$recall,mean)
sapply(metrics$tnr,mean)
sapply(metrics$auprc,mean)

# read libraries
library(ggplot2)
library(gridExtra)

sapply(metrics$tnr,mean)

# plot metrics
acc<-ggplot(stack(metrics$acc),aes(x=ind,y=values))+
  geom_boxplot()
prec<-ggplot(stack(metrics$prec),aes(x=ind,y=values))+
  geom_boxplot()
recall<-ggplot(stack(metrics$recall),aes(x=ind,y=values))+
  geom_boxplot()
tnr<-ggplot(stack(metrics$tnr),aes(x=ind,y=values))+
  geom_boxplot()
auprc<-ggplot(stack(metrics$auprc),aes(x=ind,y=values))+
  geom_boxplot()

meter<-list()
head(metrics[[5]])
titles<-c("Accuracy","Precision","Recall","TNR","AUPRC")
for(i in 1:5){
  meter[[i]]<-ggplot(stack(metrics[[i]]),aes(x=ind,y=values,fill=ind))+
    ggtitle(paste(titles[i]))+
    geom_boxplot(lwd=0.4,fatten=1,outlier.size=0.01,show.legend=FALSE)+
    labs(x=NULL,y=NULL)+
    scale_x_discrete(labels=c("kNN","RF","XGBoost","LASSO","Ensemble"))+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank(),
          axis.line=element_line(colour="black",size=0.4),
          axis.text=element_text(size=10,color="black"),
          plot.title=element_text(hjust=0.5,size=10))+
    scale_fill_brewer(palette="Pastel1")
}
grid.arrange(meter[[1]],meter[[5]],meter[[2]],meter[[3]],meter[[4]],ncol=3)

# feature importance visualization
imp<-readRDS("~/Documents/research/tongue/feature_selection/importance.all.RDS")
imp$means<-imp$means/max(imp$means)
imp<-imp[order(imp$means,decreasing=TRUE),]
imp$perc<-as.numeric(format(round(imp$means*100,2)))

normimp<-ggplot(data=imp,aes(x=reorder(features,perc),y=perc))+
  ggtitle("Normalized permutation predictor importance")+
  geom_bar(lwd=0.4,stat="identity",fill=brewer.pal(9,"Pastel1")[2],color="black")+
  geom_text(aes(label=sprintf("%.2f",round(perc,2))),hjust="left",nudge_y=1,size=3.5)+
  coord_flip()+
  labs(x=NULL,y="Percentage (%)")+
  scale_x_discrete(labels=c("Gender","Age","Stage","Radiation therapy",
                            "Chemotherapy","T stage","N stage","Grade"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black",size=0.4),
        axis.text=element_text(size=10,color="black"),
        axis.title=element_text(size=10,color="black"),
        plot.title=element_text(hjust=0.5,size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  expand_limits(y = 105)