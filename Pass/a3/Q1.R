setwd("D:/2022S1/2086/a3")
#Q1.1

fuel = read.csv("fuel.ass3.2022.csv")
mlr = lm(formula = Comb.FE~. ,data = fuel)
summary(mlr)

#Q1.4

BIS = step(object = mlr,direction = "both",k = log(length(fuel$Comb.FE)))
summary(BIS)

result = fuel[33,]
predict(mlr,result,interval = 'confidence')



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
  df = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, df, family=binomial)
  
  # Compute the AUC and return it
  target = heart.test[65,]
  rv = predict(fit, target, type="response")
  return(rv)
}

bs = boot(data=heart.train, statistic=boot.auc, R=5000, formula=HD ~ SEX + CP + TRESTBPS + CHOL + OLDPEAK + SLOPE + CA)
boot.ci(bs,conf=0.95,type="bca")



boot.auc = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  df = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, df, family=binomial)
  
  # Compute the AUC and return it
  target = heart.test[66,]
  rv = predict(fit, target, type="response")
  return(rv)
}

bs = boot(data=heart.train, statistic=boot.auc, R=5000, formula=HD ~ SEX + CP + TRESTBPS + CHOL + OLDPEAK + SLOPE + CA)
boot.ci(bs,conf=0.95,type="bca")


#Q3

library(pROC)

rm(list=ls())

setwd("D:/2022S1/2086/a3")

source("my.prediction.stats.R")
source("wrappers.R")
library(boot)
library(glmnet)
library(rpart)
library(kknn)


# 3.1

ms.measured = read.csv('ms.measured.2022.csv')
ms.truth = read.csv('ms.truth.2022.csv')
x = c(1:25)
y = c()

for (i in 1:25){
  
  # use k-NN to estimate the values of the spectrum associated with the MZ values in ms.truth.2022$MZ
  mztruth.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=i))
  
  # Compute root-mean-squared error
  mse = sqrt(sum((mztruth.hat - ms.truth$intensity)^2)/length(mztruth.hat))
  
  cat('When k =',i,', root-mean-squared error between my estimates and the true values is',mse,'\n')
  y = c(y,mse)
  
}

# Produce a plot of these errors against the various values of k.
plot(x, y, xlab='k-value', ylab='root-mean-squared errors', main='Plot of root-mean-squared errors against k-values')



# 3.2

# k = 2
k2.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=2))
plot(ms.truth$MZ, ms.truth$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=2')
lines(ms.measured$MZ, ms.measured$intensity, type='l', col='blue')
lines(ms.truth$MZ, k2.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 6
k6.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=6))
plot(ms.truth$MZ, ms.truth$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=6')
lines(ms.measured$MZ, ms.measured$intensity, type='l', col='blue')
lines(ms.truth$MZ, k6.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 12
k12.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=12))
plot(ms.truth$MZ, ms.truth$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=12')
lines(ms.measured$MZ, ms.measured$intensity, type='l', col='blue')
lines(ms.truth$MZ, k12.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 25
k25.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=25))
plot(ms.truth$MZ, ms.truth$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=25')
lines(ms.measured$MZ, ms.measured$intensity, type='l', col='blue')
lines(ms.truth$MZ, k25.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))



# 3.3

mse2 = sqrt(sum((k2.hat - ms.truth$intensity)^2)/length(k2.hat))
mse6 = sqrt(sum((k6.hat - ms.truth$intensity)^2)/length(k6.hat))
mse12 = sqrt(sum((k12.hat - ms.truth$intensity)^2)/length(k12.hat))
mse25 = sqrt(sum((k25.hat - ms.truth$intensity)^2)/length(k25.hat))
mse2
mse6
mse12
mse25


# 3.4

k6.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=6))
plot(ms.truth$MZ, ms.truth$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=6')
lines(ms.truth$MZ, k6.hat, type='l', col='black')
legend('topright', legend=c('Truth spectrum','Estimated spectrum'), col=c('red','black'), lwd=2, lty=c(1,1))



# 3.5
kknncv = train.kknn(intensity~., data=ms.measured, kmax=25, kernel='optimal')
kknncv$best.parameters$k




# 3.6
sqrt(var(fitted(kknn(intensity~., ms.measured, ms.truth, kernel='optimal', k=kknncv$best.parameters$k)) - ms.truth$intensity))


# 3.7

k6.hat = fitted(kknn(intensity~ ., ms.measured, ms.truth, kernel='optimal', k=6))
ms.truth[which.max(k6.hat),1]



# 3.8

k3.hat = fitted(kknn(intensity~., ms.measured, ms.truth, kernel='optimal', k=3))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.truth[which.max(k3.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=3))
  return(mz)
}
bs = boot(data=ms.measured, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="bca")


k5.hat = fitted(kknn(intensity~., ms.measured, ms.truth, kernel='optimal', k=5))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.truth[which.max(k6.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=5))
  return(mz)
}
bs = boot(data=ms.measured, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="bca")


k20.hat = fitted(kknn(intensity~., ms.measured, ms.truth, kernel='optimal', k=20))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.truth[which.max(k20.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=20))
  return(mz)
}
bs = boot(data=ms.measured, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="bca")

