setwd("D:/Documents/Collection/3-SEM_2/FIT2086/Assignment 1 (10%)")



df <- read.csv("covid.2023.csv")
# Question 1
library(MASS)
fit <- fitdistr(df$Days, "Poisson")
estimated_lambda <- fit$estimate["lambda"]
print(estimated_lambda)



# Question 2A
lambda <- 15.556
probability_10_or_less <- ppois(10, lambda)
print(probability_10_or_less)



#Question 2B
freq_df <- data.frame(table(df$Days))
top_values <- head(freq_df[order(-freq_df$Freq), ], 3)$Var1

ggplot(df, aes(x = factor(Days))) +
  geom_bar(stat = "count") +
  geom_bar(data = subset(df, Days %in% top_values), 
           aes(x = factor(Days)), 
           stat = "count", fill = "red") +
  scale_x_discrete(labels = function(x) 
    ifelse(x %in% top_values, paste(x, "\n(Top)", sep = ""), as.character(x))) +
  labs(x = "Days", y = "Frequency")



# Question 2C
# Function to calculate Poisson probability base on PMF
poisson_prob_range <- function(lambda_f, k_min, k_max) {
  probabilities <- numeric(k_max - k_min + 1)
  for (k in k_min:k_max) {
    probabilities[k - k_min + 1] <- (lambda_f^k * exp(-lambda_f)) / factorial(k)
  }
  return(sum(probabilities))
}
# Total rate parameter for five individuals, Range is 60 to 80 days
probability <- poisson_prob_range(5 * lambda, 60, 80)
print(probability)



# Question 2D
prob_after_14 <- 1 - ppois(13, lambda)
probability_three_or_more <- sum(dbinom(3:5, 5, prob_after_14))
print(probability_three_or_more)



# Question 3
ggplot() +
  geom_line(aes(x = df$Days, y = dpois(df$Days, lambda)), color = "blue", size = 3) +
  labs(title = "Poisson Distribution Of Days",
       x = "Days",
       y = "Probability") +
  scale_x_continuous(breaks = seq(min(df$Days), max(df$Days), by = 1)) +
  theme_minimal()



ggplot(df, aes(x = factor(Days))) +
  geom_bar(stat = "count") +
  geom_bar(data = subset(df, Days %in% top_values), 
           aes(x = factor(Days)), 
           stat = "count", fill = "red") +
  geom_line(aes(x = df$Days, 
                y = 500*dpois(df$Days, lambda)), 
            color = "blue", size = 2) +
  scale_x_discrete(labels = function(x) 
    ifelse(x %in% top_values, paste(x, "\n(Top)", sep = ""), as.character(x))) +
  labs(title = "Poisson Distribution and Frequency Distribution",
       x = "Days",
       y = "Frequency / Probability")



