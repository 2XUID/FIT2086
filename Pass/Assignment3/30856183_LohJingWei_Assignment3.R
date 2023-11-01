# ------------------------------------------
#             Question 1
# ------------------------------------------
 
# 1.1
setwd("/Users/ljw/Documents/CS/Semester 3/FIT2086/Assignment3")
fuel_efficiency = read.csv("fuel.ass3.2022.csv", stringsAsFactors=T)
fullmod = lm(Comb.FE ~ ., data=fuel_efficiency)
summary(fullmod)

# The predictors Eng.Displacement, AspirationSC, AspirationTC, AspirationTS, No.Gears, Lockup.Torque.ConverterY,
# Drive.SysF are possibly associated with fuel efficiency as they have small p-value. 
# The 3 variables appear to be the strongest predictors are Eng.Displacement, AspirationTC, Drive.SysF.
# The reason being these variables have smallest p-value

# 1.2
pvalues = coefficients(summary(fullmod))[,4]
# show 
pvalues < 0.05/17
sum(pvalues < 0.05/17)
0.05/17
# 5 of our predictors have a p-value small enough to pass the Bonferroni threshold of 0.05/9 = 0.005555556. 
# Using the Bonferroni procedure, the predictors possibly associated with fuel efficiency are 
# Eng.Displacement, AspirationTC, No.Gears, Drive.SysF, Lockup.Torque.ConverterY. 

# 1.3
# coefficient for variable Eng.Displacement is -1.330991
fullmod$coefficients[[3]]
# coefficient for variable Drive.SysF is 1.535284
fullmod$coefficients[[12]]

# 1.4 
step.fit.bic = step(fullmod, k=log(nrow(fuel_efficiency)), direction="both")
step.fit.bic$coefficients

# 1.5
# Using predict function to find the mean fuel efficiency of the car in thirty-third row
# the lwr and upr is the confidence interval 
y_hat = predict(step.fit.bic,fuel_efficiency[33,],interval="confidence")
y_hat
# Alternatively, we can manually key in the values into the regression equation found in 1.4
16.3611935 - 1.3164709*1.4 - 1.0717452*1 - 0.1747731*6  - 0.5731985*1 + 1.5475411*1

# ------------------------------------------
#             Question 2
# ------------------------------------------
setwd("/Users/ljw/Documents/CS/Semester 3/FIT2086/Assignment3")
source("my.prediction.stats.R")
source("wrappers.R")
install.packages("glmnet")
library(glmnet)
install.packages("rpart")
library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("kknn")
library(kknn)
library(boot)

# 2.1
heart.train = read.csv("heart.train.ass3.2022.csv", stringsAsFactors=T)
# fit decision tree to data
tree.hd = rpart(HD ~ ., heart.train)
tree.hd
plot(tree.hd)
text(tree.hd, digits=3)

# cross validation
cv.tree.hd = learn.tree.cv(HD~.,data=heart.train, nfolds=10, m=5000)
# The CV plot shows the best tree size is around 8.
plot.tree.cv(cv.tree.hd)
# Find the variables use in best tree
cv.tree.hd$best.tree
plot(cv.tree.hd$best.tree)
text(cv.tree.hd$best.tree, digits=3)

# 2.2
plot(cv.tree.hd$best.tree)
text(cv.tree.hd$best.tree, pretty=12)

# 2.5
# fit logistic regression model to data
fullmod.hd = glm(HD ~ ., data=heart.train, family=binomial)
summary(fullmod.hd)
# stepwise selection with KIC score 
kic.hd = step(fullmod.hd, k=3,direction="both")
summary(kic.hd)

# 2.8
heart.test = read.csv("heart.test.ass3.2022.csv", stringsAsFactors=T)
# prediction statistic for cv pruned tree
my.pred.stats(predict(cv.tree.hd$best.tree,heart.test)[,2],heart.test$HD)
# prediction statistic for step-wise logistic regression model
my.pred.stats(predict(kic.hd,heart.test,type="response"),heart.test$HD)

# 2.9
# conditional probabilities of 10th patient using cv pruned tree
predict(cv.tree.hd$best.tree,heart.test[10,])
# conditional probabilities of 10th patient using logistic regression model
predict(kic.hd,heart.test[10,],type="response")
# odds using tree
0.9298246/0.07017544
# odds using logistic regression model
0.9994987/(1 - 0.9994987)


# 2.10

# for 65th patient
boot.odd = function(data, indices=65)
{
  #Create a bootstrapped version of our data
  d = data[indices,]
  #Predict the conditional probability
  prob = predict(kic.hd, d, type="response")
  #Compute odd
  odd = prob/(1-prob)
  return(odd)
}

bs = boot(data=heart.test, statistic=boot.odd, R=5000)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)

# for 66th patient
boot.odd = function(data, indices=66)
{
  #Create a bootstrapped version of our data
  d = data[indices,]
  #Predict the conditional probability
  prob = predict(kic.hd, d, type="response")
  #Compute odd
  odd = prob/(1-prob)
  return(odd)
}

bs = boot(data=heart.test, statistic=boot.odd, R=5000)
boot.ci(bs,conf=0.95,type="bca")
plot(bs)


# ------------------------------------------
#             Question 3
# ------------------------------------------

# 3.1
ms.train = read.csv("ms.measured.2022.csv")
ms.test = read.csv("ms.truth.2022.csv")

# use k-NN to estimate the values of spectrum associated with the MZ values in ms.truth.2022.csv$MZ
# the code uses kernel type "optimal"
k <- 1:25
rmse <- c()
for(i in k) {
  ytest.hat <- fitted(kknn(intensity ~ ., ms.train, ms.test, kernel = "optimal", k =i) )
  rmse_of_each_k <- sqrt(mean((ytest.hat - ms.test$intensity)^2))
  rmse <- c(rmse,rmse_of_each_k)
}
rmse

# plot of errors against the various value of k
plot(rmse~k, lwd=2, xlab="k", ylab="errors", 
     main="Errors for each value of k from 1 to 25")

# 3.2
# For k=2
# i. Training data points (ms.measured.2022$intensity)
plot(x=ms.test$MZ, y=ms.train$intensity, xlab="Mass/Charge (MZ)", ylab="Relative Intensity", 
     main="Comparison between training data points, true spectrum and estimated spectrum by k=2", pch=16, cex = 0.5,  col="blue")
# ii. the true spectrum (ms.truth.2022$intensity)
lines(x=ms.test$MZ, y=ms.test$intensity, lwd=5,  type="l", col="orange")
# iii. predicted intensity values
lines(x=ms.test$MZ, y=fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=2) ), lwd=2, type="l")
# add legend
legend("topright", legend = c("Training data points", "True spectrum", "Estimated spectrum"),
       lwd = 2, col = c("blue", "orange", "black"))

# For k=6
# i. Training data points (ms.measured.2022$intensity)
plot(x=ms.test$MZ, y=ms.train$intensity, xlab="Mass/Charge (MZ)", ylab="Relative Intensity", 
     main="Comparison between training data points, true spectrum and estimated spectrum by k=6", pch=16, cex = 0.5,  col="blue")
# ii. the true spectrum (ms.truth.2022$intensity)
lines(x=ms.test$MZ, y=ms.test$intensity, lwd=5, type="l", col="orange")
# iii. predicted intensity values
lines(x=ms.test$MZ, y=fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=6) ), lwd=2, type="l")
# add legend
legend("topright", legend = c("Training data points", "True spectrum", "Estimated spectrum"),
       lwd = 2, col = c("blue", "orange", "black"))

# For k=12
# i. Training data points (ms.measured.2022$intensity)
plot(x=ms.test$MZ, y=ms.train$intensity, xlab="Mass/Charge (MZ)", ylab="Relative Intensity", 
     main="Comparison between training data points, true spectrum and estimated spectrum by k=12", pch=16, cex = 0.5,  col="blue")
# ii. the true spectrum (ms.truth.2022$intensity)
lines(x=ms.test$MZ, y=ms.test$intensity, lwd=5, type="l", col="orange")
# iii. predicted intensity values
lines(x=ms.test$MZ, y=fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=12) ), lwd=2, type="l")
# add legend
legend("topright", legend = c("Training data points", "True spectrum", "Estimated spectrum"),
       lwd = 2, col = c("blue", "orange", "black"))

# For k=25
# i. Training data points (ms.measured.2022$intensity)
plot(x=ms.test$MZ, y=ms.train$intensity, xlab="Mass/Charge (MZ)", ylab="Relative Intensity", 
     main="Comparison between training data points, true spectrum and estimated spectrum by k=25", pch=16, cex = 0.5,  col="blue")
# ii. the true spectrum (ms.truth.2022$intensity)
lines(x=ms.test$MZ, y=ms.test$intensity, lwd=5, xlab="Ions", type="l", col="orange")
# iii. predicted intensity values
lines(x=ms.test$MZ, y=fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=25) ), lwd=2, type="l")
# add legend
legend("topright", legend = c("Training data points", "True spectrum", "Estimated spectrum"),
       lwd = 2, col = c("blue", "orange", "black"))

# 3.3
# Quantitatively 
sqrt(mean((fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=2) ) - ms.test$intensity)^2))
sqrt(mean((fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=6) ) - ms.test$intensity)^2))
sqrt(mean((fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=12) ) - ms.test$intensity)^2))
sqrt(mean((fitted(kknn(intensity~., ms.train, ms.test, kernel="optimal", k=25) ) - ms.test$intensity)^2))

# 3.5
# using cv functionality in kknn to select best value of k
knn = train.kknn(intensity ~ ., data = ms.train, kmax=25, kernel="optimal")
knn$best.parameters$k

# 3.6
ytest.hat = fitted(kknn(intensity ~ .,ms.train, ms.test,
                         kernel = "optimal", k = 5) )
mean.diff = mean(ytest.hat - ms.train$intensity)
actual.dff = ytest.hat-ms.train$intensity
# Standard deviation of sensor/measurement noise
sqrt(sum((actual.dff-mean.diff)^2)/(501-1))

# 3.7
# find the index in which max peaks occurs
max_index_estimated = which.max(ytest.hat)
max_index_estimated
# find the MZ for the index found 
max_MZ_estimated = ms.train$MZ[max_index_estimated]
max_MZ_estimated

# 3.8
boot.intensity = function(data, indices, k_value)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # use k-NN to estimate the values of spectrum associated with the MZ values in ms.truth.2022.csv$MZ
  ytest.hat = fitted(kknn(intensity ~ ., d, ms.test, kernel = "optimal", k = k_value) )  

  return(ytest.hat[160])
}

# k=3
bs_intensity = boot(data=ms.train, statistic=boot.intensity, R=5000, k_value=3)
boot.ci(bs_intensity,conf=0.95,type="bca")

# k=5
bs_intensity = boot(data=ms.train, statistic=boot.intensity, R=5000, k_value=5)
boot.ci(bs_intensity,conf=0.95,type="bca")

# k=20
bs_intensity = boot(data=ms.train, statistic=boot.intensity, R=5000, k_value=20)
boot.ci(bs_intensity,conf=0.95,type="bca")
