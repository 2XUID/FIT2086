####################################################################################
#	Script-file:   xiaoying.haw.Q2.R                                                 #
#	Project:       FIT2086 Assignment 3                                              #
#                                                                                  #
# Purpose:  	   Code for questions in Assignment 3                                #
####################################################################################

library(pROC)

# =========================================================================================== #
#                                                                                             #
#                                       Question 2                                            #
#                                                                                             #
# =========================================================================================== #

rm(list=ls())

setwd("D:/2022S1/2086/a3/jiu")

source("my.prediction.stats.R")
source("wrappers.R")
library(glmnet)
library(rpart)
library(boot)


# -------------------------------------------------------------------------------------------- #
# 2.1

heart.train = read.csv('heart.train.ass3.2020.csv')

cv = learn.tree.cv(HD~.,data=heart.train, nfolds=10, m=5000)
cv$best.tree

# The variables used in the best tree include: THAL, CP, CA, EXANG, AGE
# There are 7 terminal nodes (leaves) in the best tree


# -------------------------------------------------------------------------------------------- #
# 2.2

plot(cv$best.tree)
text(cv$best.tree, pretty=12)
cv$cv.stats
cv


# -------------------------------------------------------------------------------------------- #
# 2.3

tree.heart = rpart(HD~., heart.train)
tree.heart
plot(tree.heart)
text(tree.heart, pretty=12)


# -------------------------------------------------------------------------------------------- #
# 2.5

heart.logistic = glm(as.factor(heart.train$HD)~., data=heart.train, family=binomial)
step.fit.bic = step(heart.logistic, k=log(nrow(heart.train)), direction="both", trace = 0)
summary(step.fit.bic)


# -------------------------------------------------------------------------------------------- #
# 2.6

step.fit.bic$coefficients


# -------------------------------------------------------------------------------------------- #
# 2.7

heart.test = read.csv('heart.test.ass3.2020.csv')
my.pred.stats(predict(cv$best.tree, heart.test)[,2], as.factor(heart.test$HD))
my.pred.stats(predict(step.fit.bic, heart.test, type='response'), as.factor(heart.test$HD))


# -------------------------------------------------------------------------------------------- #
# 2.8

heart.test[69,]
prob.tree = predict(step.fit.bic, heart.test)
odds = exp(prob.tree[69])
odds


# -------------------------------------------------------------------------------------------- #
# 2.9

boot.auc = function(formula, data, indices)
{
  d = data[indices,]
  
  fit = glm(formula, d, family=binomial)
  
  target = heart.test[69,]
  rv = predict(fit, target, type="response")
  return(rv)
  
}

bs = boot(data=heart.train, statistic=boot.auc, R=5000, formula=as.factor(HD)~CP+THALACH+OLDPEAK+CA+THAL)
boot.ci(bs, conf=0.95, type="bca")
