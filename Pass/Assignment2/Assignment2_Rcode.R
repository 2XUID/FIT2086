setwd("/Users/ljw/Documents/CS/Semester 3/FIT2086/Assignment2")
daily_covid_aug1to7 = read.csv("daily.covid.aug1to7.csv")
daily_covid_aud8to14 = read.csv("daily.covid.aug8to14.csv")

#Question 1
#1
#mean 
mean(daily_covid_aug1to7$daily.covid.cases)
#variance
var(daily_covid_aug1to7$daily.covid.cases)
#t value
qt(p = 1-0.05/2, df = 7-1)

#2.
#mean
mean(daily_covid_aud8to14$daily.covid.cases)
#variance
var(daily_covid_aud8to14$daily.covid.cases)

#3.
#p-value
2*pnorm(-abs(-2.826))

#Question 2
#1. 
#For y values
y_vector <- c(0:25)

#probability mass function
negative_binomial_probability <- function(y_array,v,r) {
  result <- c()
  for (y in y_array) {
    p <- choose(y+r-1,y)*(r^r)*((exp(v)+r)^(-r-y))*(exp(y*v))
    result <- append(result,p)
  }
  return(result)
}

#Plot
plot(y_vector,negative_binomial_probability(y_vector,0,1), type="o", col="blue", pch="o", 
     xlab="y", ylab="p(y|v,r)", main="Negative binomial probability mass function for values from 0 to 25")

points(y_vector, negative_binomial_probability(y_vector,1,2),  col="red", pch="*")
lines(y_vector, negative_binomial_probability(y_vector,1,2), col="red")

points(y_vector, negative_binomial_probability(y_vector,1.5,2),  col="green", pch="+")
lines(y_vector, negative_binomial_probability(y_vector,1.5,2), col="green")

# Add legend
legend(1, 95, legend=c("Line 1", "Line 2"),col=c("red", "blue"), lty=1:2, cex=0.8)

legend(x = "topright", # Position
       legend = c("(v=0,r=1)", "(v=1,r=2)", "(v=1.5,r=2)"), # Legend texts
       col = c("blue", "red","green"), # Line colors
       lwd = 2)  # Line width


#Question 3
#2
2*pnorm(-7.23)
1-pnorm(7.23)
pnorm(7.23)

#3
binom.test(x=176,n=240,p=0.5)

#4
2*pnorm(-abs(-3.911))



