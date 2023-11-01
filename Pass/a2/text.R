setwd("D:/2022S1/2086/a2")


data1 <- read.csv("daily.covid.aug1to7.csv", header = TRUE)
mean(data1$daily.covid.cases)
var(data1$daily.covid.cases)
length(data1$daily.covid.cases)
qt(1-0.05/2,7-1)

data2 <- read.csv("daily.covid.aug8to14.csv", header = TRUE)
mean(data2$daily.covid.cases)
var(data2$daily.covid.cases)
length(data2$daily.covid.cases)

Binomial <-function(v,r){
  Y = c()
  for (y in 0:25){
    m = choose(y+r-1,y)*(r^r)*(exp(v)+r)^(-r-y)*exp(y*v)
    Y = c(Y,m)
  }
  return (Y)
}

y1 = Binomial(0,1)
y2 = Binomial(1,2)
y3 = Binomial(1.5,2)


x = 0:25
plot(x,y1,type="b",xlab = "y",ylab = "P(y|v,r)",main="Negative binomial probability mass function", col = "red")
lines(x,y2,type = "b", col = "green")
lines(x,y3,type = "b", col = "blue")

legend(x = "topright",
       legend = c("(v = 0, r = 1)","(v = 1, r = 2)","(v = 1.5, r = 2)"),
       fill = c("red","green","blue"))


"""
> a<- 176/240-1/2
> b<- (1/2*(1-1/2)/240)^(1/2)
> a/b
[1] 7.229569
> 2*pnorm(-abs(a/b))
[1] 4.845296e-13
> b<-((1/2)*(1-1/2)/240))^(1/2)
> b<-(((1/2)*(1-1/2)/240))^(1/2)
> 2*pnorm(-abs(a/b))
[1] 4.845296e-13
> binom.test(x=176,n=240,p = 1/2)

Exact binomial test

data:  176 and 240
number of successes = 176, number of trials = 240, p-value =
  2.854e-13
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
  0.6726355 0.7881673
sample estimates:
  probability of success 
0.7333333 


> prop.test(x=c(176,64),n=c(240,240))

2-sample test for equality of proportions with continuity
correction

data:  c(176, 64) out of c(240, 240)
X-squared = 102.67, df = 1, p-value < 2.2e-16
alternative hypothesis: two.sided
95 percent confidence interval:
  0.3833789 0.5499545
sample estimates:
  prop 1    prop 2 
0.7333333 0.2666667 

"""