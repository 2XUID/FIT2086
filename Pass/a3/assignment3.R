###1
library(readr)
install.packages("kknn")
library(kknn)
library(boot)
library(ggplot2)

truth <- read_csv("ms.truth.2022.csv")


measured <- read_csv("ms.measured.2022.csv")


kernels = c("optimal")
knn = train.kknn(measured$intensity~ ., data = measured, kmax=25, kernel=kernels)


ytest.hat
ytest.hat[[1]]
output <- vector("double", length(df))
 for (i in 1:25) {
ytest.hat = fitted(kknn(measured$intensity~ .,measured,truth, kernel=kernels,k=i)) 
output[[i]] <- sqrt(mean((ytest.hat[[i]]-truth$intensity)^2))

 }
output
plot(output)

###2
ytest.hat2 = fitted(kknn(measured$intensity~ .,measured,truth, kernel=kernels,k=2)) 
ytest.hat6 = fitted(kknn(measured$intensity~ .,measured,truth, kernel=kernels,k=6)) 
ytest.hat12 = fitted(kknn(measured$intensity~ .,measured,truth, kernel=kernels,k=12)) 
ytest.hat25 = fitted(kknn(measured$intensity~ .,measured,truth, kernel=kernels,k=25)) 
truth$intensity_truth <- truth$intensity
truth <- truth[,-c(1,2)]
ms <- rbind(data.frame(intensity=measured$intensity, MZ=measured$MZ, t=as.factor(1)), data.frame(intensity=truth$intensity_truth, MZ=measured$MZ, t=as.factor(0)))
ms2 = rbind(data.frame(intensity=ytest.hat2, MZ=measured$MZ, t=as.factor(2)), ms)
ms6 = rbind(data.frame(intensity=ytest.hat6, MZ=measured$MZ, t=as.factor(2)), ms)
ms12 = rbind(data.frame(intensity=ytest.hat12, MZ=measured$MZ, t=as.factor(2)), ms)
ms25 = rbind(data.frame(intensity=ytest.hat25, MZ=measured$MZ, t=as.factor(2)), ms)
ggplot(vd, aes(x=v, y=y)) + geom_point(aes(color=t))

plot2 <- plot(ms2$MZ,ms2$intensity,col=ms2$t)
plot6 <- plot(ms6$MZ,ms6$intensity,col=ms6$t)
plot12 <- plot(ms12$MZ,ms12$intensity,col=ms12$t)
plot25 <- plot(ms25$MZ,ms25$intensity,col=ms25$t)

p2 <- ggplot(ms2, aes(x = MZ, y = intensity, color = t, shape = t, linetype = t)) +
  geom_point() + 
  theme_bw() 
p2
p6 <- ggplot(ms6, aes(x = MZ, y = intensity, color = t, shape = t, linetype = t)) +
geom_point() + 
  theme_bw() 
p6
p12 <- ggplot(ms12, aes(x = MZ, y = intensity, color = t, shape = t, linetype = t)) +
geom_point() + 
  theme_bw() 
p12
p25 <- ggplot(ms25, aes(x = MZ, y = intensity, color = t, shape = t, linetype = t)) +
geom_point() + 
  theme_bw() 
p25


###3
#It can be known from RMSE, when the value of K is small, the complexity (capacity) of the model is high, the training error will be reduced, and the generalization ability will be weakened; When the value of K is large, the complexity of the model is low, the training error will increase, and the generalization ability will be improved to a certain extent
###4
#Knn can provide a smooth, low-noise background level estimation and an accurate estimation of peak height.As can be seen from the picture, the predicted value has a peak just where there is a peak
###5
knn = train.kknn(measured$intensity~ ., data = measured, kmax=25, kernel=kernels)
truth <- read_csv("ms.truth.2022.csv")
k = knn$best.parameters$k
k
ytest.hatbest = fitted(kknn(measured$intensity~ .,measured,truth, kernel = knn$best.parameters$kernel, k = knn$best.parameters$k)) 
#The best k is 5
###6
output <- sqrt(mean((ytest.hat[[5]]-truth$intensity)^2))
output
#SD=3.66
###7
ytest.hatbest
summary(ytest.hatbest)
which.max(ytest.hatbest)
#MZ value is 8818

###8

mean(ytest.hat == truth$intensity)*100
