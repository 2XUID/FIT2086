setwd("D:/2022S1/2086/a3")
fuel = read.csv("fuel.ass3.2022.csv")
mlr = lm(formula = Comb.FE~. ,data = fuel)
summary(mlr)
BIS = step(object = mlr,direction = "both",k = log(length(fuel$Comb.FE)))
summary(BIS)
t_score = qt(p =1-0.05/2,df = 488-11-1,lower.tail = TRUE)
yhat = predict(mlr,fuel)
yhat[33]


#Q2.1
rm(list = ls())
source("my.prediction.stats.R")
source("wrappers.R")
library(glmnet)
library(rpart)
library(boot)

heart.train = read.csv("heart.train.ass3.2022.csv",stringsAsFactors = TRUE)
cv = learn.tree.cv(HD~.,data=heart.train,nfolds=10,m=5000)
cv$best.tree
#Q2.2

plot(cv$best.tree)
text(cv$best.tree,pretty=12)
cv$cv.stats
cv

#2.3
tree.heart = rpart(HD~.,heart.train)
tree.heart
plot(tree.heart)
text(tree.heart, pretty=12)



#2.5
heart.logistic = glm(HD~., data=heart.train, family=binomial)
kic = step(heart.logistic, k=3, direction="both")
summary(kic)


# 2.6

kic$coefficients



# 2.8

heart.test = read.csv('heart.test.ass3.2022.csv', stringsAsFactors = TRUE)
my.pred.stats(predict(cv$best.tree, heart.test)[,2], heart.test$HD)
my.pred.stats(predict(kic, heart.test, type='response'), heart.test$HD)


# 2.9

heart.test[10,]
prob.tree = predict(kic, heart.test)
odds = exp(prob.tree[10])
odds



# 2.10

boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = heart.test[65,]
  rv = predict(fit, target, type="response")
  return(rv)
}

bs = boot(data=heart.train, statistic=boot.auc, R=1000, formula=as.factor(HD)~CP+SLOPE+OLDPEAK+CHOL+CA+THAL+AGE)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)



boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = heart.test[66,]
  rv = predict(fit, target, type="response")
  return(rv)
}

bs = boot(data=heart.train, statistic=boot.auc, R=1000, formula=as.factor(HD)~CP+SLOPE+OLDPEAK+CHOL+CA+THAL+AGE)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)







# 3.1




































