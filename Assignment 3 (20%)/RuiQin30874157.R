#Q1
setwd("D:/Documents/Collection/3-SEM_2/FIT2086/Assignment 3 (20%)")
housing <- read.csv("housing.2023.csv")
model <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat, data = housing)
summary(model)

final_model <- step(model, direction = "both", k = log(nrow(housing)))
summary(final_model)

medv = 34.054337 - 0.115818*0.04741 + 0.018561*0 - 0.011274*11.93 + 4.163521*0 - 16.722652*0.573 + 4.501521*6.03 + 0.001457*80.8 - 1.163294*2.505 + 0.291680*1 - 0.012387*273 - 0.960017*21 - 0.480698*7.88
medv


predicted_price <- predict(model, data.frame(
    crim = 0.04741,
    zn = 0,
    indus = 11.93,
    chas = 0,
    nox = 0.573,
    rm = 6.03,
    age = 80.8,
    dis = 2.505,
    rad = 1,
    tax = 273,
    ptratio = 21,
    lstat = 7.88
  ), interval = "confidence", level = 0.95)
print(predicted_price)


interaction_model <- lm(medv ~ chas + nox + rm * dis + ptratio + lstat, data = housing)
summary(interaction_model)

housing$rm_dis_interaction <- housing$rm * housing$dis
interaction_model <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat + rm_dis_interaction, data = housing)
summary(interaction_model)


#Q2
library(pROC)
library(rpart)
library(tree)
source("my.prediction.stats.R")
source("wrappers.R")
heart.train <- read.csv("heart.train.2023.csv")

#Q2.1
cv_heart <- learn.tree.cv(HD ~ ., data = heart.train, nfolds=10, m=5000)
best_tree <- cv_heart$best.tree
best_tree
plot(cv_heart$best.tree)
text(cv_heart$best.tree,pretty=12)

#Q2.3
heart_tree <- rpart(HD~., heart.train)
heart_tree
plot(heart_tree)
text(heart_tree, pretty=12)

#Q2.5
logistic_model <- glm(as.factor(heart.train$HD)~., data = heart.train, family = binomial)
stepwise_model  <- step(logistic_model, direction="both", k=log(nrow(heart.train)), trace = 0)
summary(stepwise_model)

#Q2.6
stepwise_model$coefficients

#Q2.7
heart.test <- read.csv('heart.test.2023.csv')
my.pred.stats(predict(stepwise_model, heart.test, type='response'), as.factor(heart.test$HD))
my.pred.stats(predict(best_tree, heart.test)[,2], as.factor(heart.test$HD))

#Q2.8
patient_69 <- heart.test[69,]

#tree model
predicted_prob_tree <- predict(best_tree, patient_69)
predicted_prob_tree
predicted_prob_tree[2]/predicted_prob_tree[1]

#logistic regression model
odds_logistic <- predict(stepwise_model, patient_69, type = "response")
odds_logistic
odds_logistic / (1 - odds_logistic)

#Q2.9
library(boot)
boot.auc <- function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = patient_69
  rv = predict(fit, target, type="response")
  return(rv)
  
}

bs_logic <- boot(data=heart.train, statistic=boot.auc, R=5000, formula=as.factor(HD) ~ CP + THALACH + OLDPEAK + 
                  CA + THAL)
boot.ci(bs_logic, conf=0.95, type="bca")


#Q3.1

library(kknn)
library(ggplot2)

ms.truth <- read.csv("ms.truth.2023.csv")
ms.measured <- read.csv("ms.measured.2023.csv")

mse_values <- numeric(25)

for (k in 1:25) {

  knn_model <- kknn(intensity ~ MZ, train = ms.measured, test = ms.truth, k = k, kernel = "optimal")
  
  predictions <- predict(knn_model)
  
  mse <- mean((predictions - ms.truth$intensity)^2)
  
  mse_values[k] <- mse
}

mse_values

plot(1:25, mse_values, type = "b", xlab = "k", ylab = "Mean Squared Error", main = "Plot of Mean Squared Error vs. k")

#Q3.2
spectra_plot <- function(k) {
  
  knn_model <- kknn(intensity ~ MZ, train = ms.measured, test = ms.truth, k = k, kernel = "optimal")
  
  
  predictions <- predict(knn_model)
  
  
  plot_data <- data.frame(MZ = ms.truth$MZ, True_Intensity = ms.truth$intensity, Predicted_Intensity = predictions, Measured_Intensity = ms.measured$intensity)
  
  
  ggplot(plot_data, aes(x = MZ)) +
    geom_line(aes(y = True_Intensity, color = "True Spectrum"), size = 1) +
    geom_point(aes(y = True_Intensity, color = "True Spectrum"), size = 1, alpha = 0.5, shape = 16) +
    geom_line(aes(y = Predicted_Intensity, color = "Estimated Spectrum"), size = 1) +
    geom_point(aes(y = Predicted_Intensity, color = "Estimated Spectrum"), size = 1, shape = 17) +
    geom_point(aes(y = Measured_Intensity, color = "Training Data"), size = 1, alpha = 0.5, shape = 16) +
    labs(title = paste("k =", k), x = "MZ", y = "Intensity", color = "Legend") +
    scale_color_manual(values = c("True Spectrum" = "blue", "Estimated Spectrum" = "red", "Training Data" = "black"))
}

spectra_plot(2)
spectra_plot(5)
spectra_plot(10)
spectra_plot(25)

#Q3.3
spectra_plot(6)
spectra_plot(7)

#Q3.5
cv_results <- train.kknn(intensity ~ MZ, data = ms.measured, kmax=25, kernel = "optimal")
best_k <- cv_results$best.parameters$k
best_k

mse_values <- numeric(25)

for (k in 1:25) {
  knn_model <- kknn(intensity ~ MZ, train = ms.measured, test = ms.truth, k = k, kernel = "optimal")
  predictions <- predict(knn_model)
  mse <- mean((predictions - ms.truth$intensity)^2)
  mse_values[k] <- mse
}
mse_values
min_mse_k <- which.min(mse_values)
min_mse_k


#Q3.6
ytest.hat <- fitted(kknn(intensity ~ .,ms.measured, ms.truth, kernel = "optimal", k = 6))
sd(ms.measured$intensity - ytest.hat)

#Q3.7
ms.truth$MZ[which.max(ytest.hat)]


#Q3.8
knn_estimate <- function(data, indices, mz_value, k) {
  d <- data[indices,]
  knn_model <- kknn(intensity ~ MZ, train = d, test = data.frame(MZ = mz_value), k = k, kernel = "optimal")
  return(predict(knn_model))
}

bootstrap_ci <- function(k) {
  boot_results <- boot(ms.measured, knn_estimate, R = 5000, mz_value = 7963.3, k = k)
  ci <- boot.ci(boot_results, type = "basic", conf = 0.95)
  return(ci)
}

bootstrap_ci(3)
bootstrap_ci(6)
bootstrap_ci(20)




