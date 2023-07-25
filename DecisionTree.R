rm(list=ls())
cat("\014")
setwd("/Users/shwetshah/Desktop") #setting working directory

#Q1 a)Loading the data
bank <- read.csv("bank.csv", stringsAsFactors = FALSE)
# Converting these variables to factor type: job, marital, education, default, housing, loan, contact, month, campaign, poutcome, y
bank$job= as.factor(bank$job)
bank$marital= as.factor(bank$marital)
bank$education= as.factor(bank$education)
bank$default= as.factor(bank$default)
bank$housing= as.factor(bank$housing)
bank$loan= as.factor(bank$loan)
bank$contact= as.factor(bank$contact)
bank$month= as.factor(bank$month)
bank$campaign= as.factor(bank$campaign)
bank$job= as.factor(bank$poutcome)
bank$y= as.factor(bank$y)

#Q1 b)Splitting the data into train and test 
set.seed(123)
train <- sample(1:nrow(bank), nrow(bank)*(2/3))
bank.test <- bank[-train,]
bank.train<- bank[train,]

#Q2 a)Building a decision tree
fit <- rpart(y ~ ., 
             data=bank.train, # dataframe used
             method="class",  # treat churn as a categorical variable, default
             control=rpart.control(xval=10, minsplit=50), # xval: num of cross validation for gini estimation # minsplit=1000: stop splitting if node has 1000 or fewer obs
             parms=list(split="gini"))

fit
#Q2 b)plotting the tree
rpart.plot(fit, type = 1, extra = 1, main="Decision Tree")

#Q2 c)Defining a Confusion Matrix and extracting TP,TN,FP,FN
test.pred <- predict(fit, bank.test, type="class")
test.actual <- bank.test$y
cm <- table(test.pred, test.actual)
cm
addmargins(cm)

tp<- cm[2,2];tp;
tn<- cm[1,1];tn;
fp<- cm[2,1];fp;
fn<- cm[1,2];fn;

#Q2 d)
FPR<- fp/(tn+fp); FPR 
FNR<- fn/(tp+fn);FNR
TNR<- tn/(tn+fp);TNR #Specificity
TPR<- tp/(tp+fn);TPR #Sensitivity
pt[1,1] + pt[2,2] # accuracy

#Q2 e)
#1. We get the equation:  1800(1-𝛼)  + 1800(𝛼) + 200(1-β)  + 200(β)= a
#2. We multiply (a) by the expected profit from each segment that is (0-10)=-10 for FP and (50-10)=40 for TP
#On solving we get, 1800(𝛼)*-10 + 200(1-β)*40 which is nothing but 8000(TPR)-18000(FPR)
Profit<-(8000*TPR)-(18000*FPR)
Profit
