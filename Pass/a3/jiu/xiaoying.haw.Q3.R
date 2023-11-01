####################################################################################
#	Script-file:   xiaoying.haw.Q3.R                                                 #
#	Project:       FIT2086 Assignment 3                                              #
#                                                                                  #
# Purpose:  	   Code for questions in Assignment 3                                #
####################################################################################

library(pROC)

# =========================================================================================== #
#                                                                                             #
#                                       Question 3                                            #
#                                                                                             #
# =========================================================================================== #

rm(list=ls())

setwd("D:/2022S1/2086/a3/jiu")

source("my.prediction.stats.R")
source("wrappers.R")
library(boot)
library(kknn)

# -------------------------------------------------------------------------------------------- #
# 3.1(a)

ms.train = read.csv('ms.train.ass3.2020.csv')
ms.test = read.csv('ms.test.ass3.2020.csv')
x = c(1:25)
y = c()
  
for (i in 1:25){
  
  # Find estimate values of the spectrum for the MZ values in ms.test$MZ
  mztest.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=i))
  
  # Compute mean-squared error
  mse = mean((mztest.hat - ms.test$intensity)^2)
  
  cat('When k =',i,', mean-squared error between my estimates and the true values is',mse,'\n')
  y = c(y,mse)
  
}

# Produce a plot of plot of these errors against the various values of k
plot(x, y, xlab='k-value', ylab='Mean-squared error', main='Plot of mean-squared errors against k-values')


# -------------------------------------------------------------------------------------------- #
# 3.1(b)

# k = 2
mztest.k2.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=2))
plot(ms.test$MZ, ms.test$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=2')
lines(ms.train$MZ, ms.train$intensity, type='l', col='blue')
lines(ms.test$MZ, mztest.k2.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 5
mztest.k5.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=5))
plot(ms.test$MZ, ms.test$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=5')
lines(ms.train$MZ, ms.train$intensity, type='l', col='blue')
lines(ms.test$MZ, mztest.k5.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 10
mztest.k10.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=10))
plot(ms.test$MZ, ms.test$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=10')
lines(ms.train$MZ, ms.train$intensity, type='l', col='blue')
lines(ms.test$MZ, mztest.k10.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))

# k = 25
mztest.k25.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=25))
plot(ms.test$MZ, ms.test$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=25')
lines(ms.train$MZ, ms.train$intensity, type='l', col='blue')
lines(ms.test$MZ, mztest.k25.hat, type='l', col='black')
legend('topright', legend=c('Training data points','True spectrum','Estimated spectrum'), col=c('blue','red','black'), lwd=2, lty=c(1,1))


# -------------------------------------------------------------------------------------------- #
# 3.1(c)

mse2 = mean((mztest.k2.hat - ms.test$intensity)^2)
mse5 = mean((mztest.k5.hat - ms.test$intensity)^2)
mse10 = mean((mztest.k10.hat - ms.test$intensity)^2)
mse25 = mean((mztest.k25.hat - ms.test$intensity)^2)
mse2
mse5
mse10
mse25


# -------------------------------------------------------------------------------------------- #
# 3.2

kknncv = train.kknn(intensity~., data=ms.train, kmax=25, kernel='optimal')
kknncv$best.parameters$k


# -------------------------------------------------------------------------------------------- #
# 3.3

var(fitted(kknn(intensity~., ms.train, ms.test, kernel='optimal', k=kknncv$best.parameters$k)) - ms.test$intensity)


# -------------------------------------------------------------------------------------------- #
# 3.4

mztest.k6.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=6))
plot(ms.test$MZ, ms.test$intensity, type='l', col='red', xlab='Mass/Charge(MZ)', ylab='Relative Intensity', 
     main='Graph produced by k-NN method when k=6')
lines(ms.test$MZ, mztest.k6.hat, type='l', col='black')
legend('topright', legend=c('True spectrum','Estimated spectrum'), col=c('red','black'), lwd=2, lty=c(1,1))


# -------------------------------------------------------------------------------------------- #
# 3.5

mztest.k6.hat = fitted(kknn(intensity~ ., ms.train, ms.test, kernel='optimal', k=6))
ms.test[which.max(mztest.k6.hat),1]


# -------------------------------------------------------------------------------------------- #
# 3.6

mztest.k3.hat = fitted(kknn(intensity~., ms.train, ms.test, kernel='optimal', k=3))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.test[which.max(mztest.k3.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=3))
  return(mz)
}
bs = boot(data=ms.train, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="basic")


mztest.k6.hat = fitted(kknn(intensity~., ms.train, ms.test, kernel='optimal', k=6))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.test[which.max(mztest.k6.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=6))
  return(mz)
}
bs = boot(data=ms.train, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="basic")


mztest.k20.hat = fitted(kknn(intensity~., ms.train, ms.test, kernel='optimal', k=20))
boot.max = function(formula, data, indices){
  d = data[indices,]
  target = ms.test[which.max(mztest.k20.hat),]
  mz = fitted(kknn(formula, d, target, kernel='optimal', k=20))
  return(mz)
}
bs = boot(data=ms.train, statistic=boot.max, R=5000, formula=intensity~.)
boot.ci(bs,conf=0.95,type="basic")
