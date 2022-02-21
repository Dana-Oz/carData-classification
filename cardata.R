library(dplyr)
#Q1 - Upload carData and import chile dataset to a dataframe:
#install.packages('carData')
library(carData)
df <- Chile

#Q2 variable analysis:

summary(df) %>% print
sd(df$population, na.rm = T)
sd(df$age, na.rm = T)
sd(df$statusquo, na.rm = T)
sd(df$income, na.rm = T) 


#install.packages("ggplot2")
library(ggplot2)

#CATEGORIAL variable plots
ggplot(data = df) +
  geom_bar(mapping = aes(x = sex,fill = vote)) +
  ggtitle("Distribution of class 'Vote' segmented by 'Sex' ")

ggplot(data = df) +
  geom_bar(mapping = aes(x = region,fill = vote))  +
  ggtitle("Distribution of class 'Vote' segmented by 'Region' ")

ggplot(data = df) +
  geom_bar(mapping = aes(x = region,fill = sex))  +
  ggtitle("Distribution of class 'REGION' segmented by 'sex' ")

ggplot(data = df) +
  geom_bar(mapping = aes(x = education,fill = vote))  +
  ggtitle("Distribution of class 'Vote' segmented by 'Education' ")

ggplot(data = df) +
  geom_bar(mapping = aes(x = region,fill = education))  +
  ggtitle("Distribution of class 'REGION' segmented by 'education' ")


#Numerical variable plots
ggplot(data = df) + #STATUSQUO -VOTE
  geom_density(mapping = aes(x =statusquo,fill=vote))+
  facet_wrap( ~vote)
#Q3 -Variable transformation:
#q3.1
df$vote_ind <-ifelse(df$vote == 'Y',1, 0)

#q3.2-Handeling missing data
#check the ditribution of the data frame with high missing data
summary(df[which(is.na(df$vote)),]) # vote has 168 observations missing
summary(df[which(is.na(df$income)),]) #income has 98 observations missing168
summary(df) #compare- Looks OK!
df$income[is.na(df$income)] <- mean(df$income,na.rm=T) #Replace with mean
#delete NA's
df<-na.omit(df)

#q3.3 Factorize variable 
df$region <- factor(df$region)
df$sex <- factor(df$sex)
df$education <- factor(df$education, levels=c('S','PS','P'))
#set vote as a factor
df$vote_ind<-factor(df$vote_ind, levels=c(0,1))

#Q3- calc correlation (population, age, income)
#Numeric variables- calc by pearson correlation
#install.packages("corrplot")
library(corrplot)
df.cor<- df %>%select_if(is.numeric) %>%cor()
print(df.cor) #get correlation values
corrplot(df.cor) #get correlation plot

#numeric variables and categorial variable vote_ind- Linear regression
ggplot(data = df) +
  geom_point(mapping = aes(x = statusquo, y = vote_ind),color="blue")

#Q4- categorial features relation to vote_ind (Y variable)
#install.packages("gmodels")
library(gmodels)

CrossTable(df$vote_ind, df$region,chisq = TRUE, digits=2) #region
CrossTable(df$vote_ind, df$sex,chisq = TRUE, digits=2)#sex
CrossTable(df$vote_ind, df$education,chisq = TRUE, digits=2)#education

#MORE ON REGION
df1 <- df %>% filter((region=="C"|region=="S" | region=="SA") & (vote_ind==1|vote_ind==0))
CrossTable( df1$region, df1$vote_ind,chisq = TRUE, digits=2)
df2 <- df %>% filter((region=="C" | region=="SA") & (vote_ind==1|vote_ind==0))
CrossTable( df2$region, df2$vote_ind,chisq = TRUE, digits=2)

#MORE ON EDUCATION
df1 <- df %>% filter((education=="P"|education=="S") & (vote_ind==1|vote_ind==0))
CrossTable( df1$region, df1$vote_ind,chisq = TRUE, digits=2)
df2 <- df %>% filter((region=="C" | region=="SA") & (vote_ind==1|vote_ind==0))
CrossTable( df2$region, df2$vote_ind,chisq = TRUE, digits=2)

#linear reg
lm_cars<-lm(as.numeric(as.character(vote_ind))~., data=df) ##if vote_ind is numeric
summary(lm_cars)

#EXTRA- show how it looks in a plot the way we binarized the vote column:
ggplot(data = df) +
  geom_bar(mapping = aes(x = vote_ind,fill = vote)) +
  ggtitle("Distribution of class 'Vote' segmented by 'Sex' ")
#not equally ditributed amoung binary class vote_ind

#Q5 - delete 2 variables:
df$vote<-NULL
df$statusquo<-NULL

#Q6 - split to train-test:
set.seed(100)
ntrain <- round(nrow(df)*0.8)
tindex <- sample(nrow(df),ntrain) 
train <- df[tindex, ]
test <- df[-tindex, ]

#Q7 -Modeling
#DECISION TREE #can you add direction ="both"\"forward"\"backward"
library(rpart)
library(pROC)
tree<-rpart(vote_ind~., data=train, method='class',cp=0.005)

#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(tree)

#7.2 - Summary for trained model:
summary(tree)
#Accuracy: (and full confusion matrix)
train_pred <- predict(tree, train,type="class") #predict class type
train_confMat <- table(train$vote_ind,train_pred) %>% print()
train_accuracy <- ((train_confMat[1,1]+train_confMat[2,2])/sum(train_confMat)) %>% print()
print(paste('Accuracy:',train_accuracy )) #accuracy
train_sensitivity <-(train_confMat[2,2] /sum(train_confMat[2,])) %>%print()
train_specificity <-(train_confMat[1,1] /sum(train_confMat[1,])) %>% print()

#ROC,AUC:
#predict probability to classify '1' using desicion tree:
train_pred <- predict(tree,train ,type="prob")
train_rocObject <- roc(train$vote_ind,train_pred[,2]) #create roc object
print(paste('AUC:',train_rocObject$auc)) #print auc result value
train_TPR=rev(train_rocObject$sensitivities)
train_FPR=rev(1 - train_rocObject$specificities)
#Plot curve:
plot(train_FPR,train_TPR, type="o", main='Decision Tree ROC on TRAIN')
abline(0:1)

#predict on TEST
test_pred <-  predict (tree,test,type="class")

#Calculate Accuracy (and full confusion matrix)
test_confMat <- table(test$vote_ind,test_pred[])  %>%print()
test_accuracy <- ((test_confMat[1,1]+test_confMat[2,2])/sum(test_confMat)) %>%print()
test_sensitivity <-(test_confMat[2,2] /sum(test_confMat[2,])) %>%print()
test_specificity <-(test_confMat[1,1] /sum(test_confMat[1,])) %>%print()
#ROC, AUC:
test_pred <- predict(tree, test, type="prob")
test_rocObject <- roc(test$vote_ind,test_pred[,2])
test_rocObject$auc %>%print()
test_TPR=rev(test_rocObject$sensitivities)
test_FPR=rev(1 - test_rocObject$specificities)
plot(test_FPR,test_TPR, type="o",main='Decision Tree ROC on TEST')
abline(0:1)


#LOGISTIC REGRESSION
library(glmnet)
Model_glm<-glm(vote_ind~., data= train, family="binomial")
#STEPWISE #ALL have same AIC =2501 as Model_glm
step(glm(vote_ind~1,family=binomial, data=train),direction="forward", scop=~region+population+education+sex+age+income)
step(glm(vote_ind~.,family=binomial, data=train),direction="both")
step(glm(vote_ind~.,family=binomial, data=train),direction="backward")

#Model output 
summary(Model_glm)

#predict on train 
train_pred<- predict (Model_glm,train,type="response")
train_pred_ind <- ifelse(train_pred > 0.5,1,0)

#Measurment on TRAIN
train_confMat <- table(train$vote_ind,train_pred_ind) %>%print ()
train_accuracy <- ((train_confMat[1,1]+train_confMat[2,2])/sum(train_confMat)) %>% print()
train_accuracy <-mean(train$vote_ind == train_pred_ind)%>% print()
train_sensitivity <-(train_confMat[2,2] /sum(train_confMat[2,])) %>%print()
train_specificity <-(train_confMat[1,1] /sum(train_confMat[1,])) %>%print()

#ROC AUC
train_rocObject<-roc(train$vote_ind,train_pred)
train_rocObject$auc %>%print()
train_TPR=rev(train_rocObject$sensitivities)
train_FPR=rev(1 - train_rocObject$specificities)
#Plot curve:
plot(train_FPR,train_TPR, type="o", main='Logistic Regression ROC on TRAIN')
abline(0:1)

#predict on test
test_pred <- predict (Model_glm, test,type="response")
test_pred_ind <- ifelse(test_pred > 0.5,1,0)

#Measurment on TEST
test_confMat <- table(test$vote_ind,test_pred_ind) %>%print ()
test_accuracy <- ((test_confMat[1,1]+test_confMat[2,2])/sum(test_confMat)) %>% print()
test_sensitivity <-(test_confMat[2,2] /sum(test_confMat[2,])) %>%print()
test_specificity <-(test_confMat[1,1] /sum(test_confMat[1,])) %>%print()

#ROC AUC
test_rocObject <- roc(test$vote_ind,test_pred)
test_rocObject$auc %>%print()
test_TPR=rev(test_rocObject$sensitivities)
test_FPR=rev(1 - test_rocObject$specificities)
plot(test_FPR,test_TPR, type="o", main='Logistic Regression ROC on TEST')
abline(0:1)
