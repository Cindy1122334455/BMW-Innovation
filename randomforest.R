library(randomForest)
dfleadnonlead1<-dfleadnonlead[,-1]
#data partition
set.seed(113)
dfleadnonlead1<-na.omit(dfleadnonlead1)
id<-sample(2,nrow(dfleadnonlead1),replace=TRUE,prob=c(0.7,0.3))
train<-dfleadnonlead1[id==1,]
test<-dfleadnonlead1[id==2,]
dfleadnonlead1$Geo.Country[which(dfleadnonlead1$Geo.Country!="usa")]<-"other country"
rf<-randomForest(factor(Lead.Complete) ~ .,data=train,importance=TRUE)
print(rf)
#what variables are important
varImpPlot(rf)
# Contruct ROC curve
library(ROCR)
pred<-predict(rf,newdata=test,'prob')
pred<-prediction(pred,test$Lead.Complete)
roc<-performance(pred,'tpr','fpr')
plot(roc,colorize=T,main='ROC curve',xlab='1-Specificity',ylab='Sensitivity',print.curoffs.at=seq(0,1,0.5),text.adj=c(-0.3,2))
abline(0,1)
#Area Under Curve(AUC)
auc<-performance(pred,'auc')
unlist(slot(auc,'y.values'))
auc

#prediction with train data
p1<-predict(rf,train)
ct<-table(train$Lead.Complete,p1); ct
accuracy<-(ct[1,1]+ct[2,2])/nrow(train); accuracy
confusionMatrix(p1,train$Lead.Complete)

#prediction with test data
p2<-predict(rf,newdata=test)
confusionMatrix(p2,test$Lead.Complete)
accuracy<-(ct[1,1]+ct[2,2])/nrow(test); accuracy

set.seed(1234)
rf<-randomForest(factor(Lead.Complete) ~ ., data=train)
plot(rf,ylim=c(0,2))
legend('topright',colnames(rf$err.rate),col=1:3,fill=1:3)

# the importance of different variables
importance<-importance(rf)
varImportance<-data.frame(Variables=row.names(importance),Importance=round(importance[ ,'MeanDecreaseGini'],2))
install.packages("magrittr")
library(magrittr)
rankImportance<-varImportance %>% 
  mutate(Rank=paste0('#',dense_rank(desc(importance))))
library(ggplot2)
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
geom_bar(stat='identity') + 
geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
labs(x = 'Variables') +
coord_flip() + 
theme_bw()
























