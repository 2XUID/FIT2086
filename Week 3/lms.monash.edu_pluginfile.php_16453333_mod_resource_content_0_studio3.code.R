####################################################################################
#	Script-file:   studio3.code.R
#	Project:       FIT2086 - Studio 3 
#
# Purpose:  	   Solutions for questions in Studio 3
####################################################################################


# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

cat("Question 3\n")

# 3.1
my_estimates <- function(X)
{
  n = length(X)
  
  retval = list()
  
  # Calculate the sample mean
  retval$mu_ml = sum(X)/n
  
  # Calculate the squared deviations around the mean
  e2 = (X - retval$m)^2
  
  # Calculate the two estimates of variance
  retval$var_ml = sum(e2)/n
  retval$var_u  = sum(e2)/(n-1)
  
  return(retval)
}

# 3.2
train <- read.csv("train.csv")

est = my_estimates(train$heights)
est$mu_ml
est$var_ml
est$var_u
sqrt(est$var_ml)
sqrt(est$var_u)

# 3.3 Recreate the Figure
plot(x=train$heights, y=rep(0,10), ylim=c(0,6), xlim=c(1.4,2), 
     ylab="p(heights)", xlab="Heights")
xv = seq(from=1.4, to=2, length.out=100)
lines(xv, dnorm(xv, est$mu_ml, sqrt(est$var_ml)), lwd=2.5, col="red")
lines(xv, dnorm(xv, est$mu_ml, sqrt(est$var_u)), lwd=2.5, col="blue")

# Tip: lty = 0 means no line, pch = "o" means a circle, pch = "" means no symbol
legend(x=1.75,y=6,c("Samples","ML Estimate","Unbiased Estimate"), lty=c(0,1,1), 
       pch=c("o","",""), col=c("black","red","blue"), lwd=c(1,2.5,2.5))

# 3.4
test <- read.csv("test.csv")

cat("P(X > 1.7m) =", 1-pnorm(1.7,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    1-pnorm(1.7,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights>1.7), "(empirical)\n")

cat("P(X < 1.5m) =", pnorm(1.5,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    pnorm(1.5,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights<1.5), "(empirical)\n")

cat("P(X > 1.6m & X < 1.75m) =", pnorm(1.75,est$mu_ml,sqrt(est$var_ml)) - pnorm(1.6,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    pnorm(1.75,est$mu_ml,sqrt(est$var_u))-pnorm(1.6,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights > 1.6 & test$heights < 1.75), "(empirical)\n")

# 3.5
norm_negloglike <- function(y,mu,sigma)
{
  n = length(y)
  return( (n/2)*log(2*pi*sigma^2) + 1/2/sigma^2*sum((y-mu)^2) )
}
cat("Neg-log-likelihood (unbiased):", norm_negloglike(test$heights, est$mu_ml, sqrt(est$var_ml)))
cat("; Neg-log-likelihood (ML):", norm_negloglike(test$heights, est$mu_ml, sqrt(est$var_u)),'\n')

