#Salary is measured in thousands of dollars changed to dollars
model1 <- lm(log(salary) ~ log(sales), data = df)
coef(model1)
df$salary_dollars <- df$salary * 1000
model2 <- lm(log(salary_dollars) ~ log(sales), data = df)
coef(model2)
coef(model2) - coef(model1)
(Intercept)  log(sales)

#A log-log regression. By about what percent does y change when x falls by 2%?
beta1 <- 0.27
x_change <- -2

y_change <- beta1 * x_change
round(y_change, 2)

#By about what percent does predicted salary change when return on equity rises by 5 points?
library(wooldridge)
data("ceosal1")

model <- lm(log(salary) ~ roe, data = ceosal1)
summary(model)
beta1 <- coef(model)["roe"]

percent_change <- 100 * beta1 * 5

round(percent_change, 2)

#Under SLR1-5 sample sd
x <- c(1, 2, 4, 5, 6)
sigma <- 5

SST_x <- sum((x - mean(x))^2)

sd_beta1 <- sigma / sqrt(SST_x)

round(sd_beta1, 2)

# a sample regression estimate the error of variance
SSR <- 756
n <- 31

sigma_squared <- SSR / (n - 2)

round(sigma_squared, 2)

#A simple regression find the standard error
n <- 8
SSR <- 457
SST_x <- 13

sigma_hat <- sqrt(SSR / (n - 2))

se_beta1 <- sigma_hat / sqrt(SST_x)

round(se_beta1, 2)

#regression of y on x what is the new slope
beta1 <- 0.14
divisor <- 1000

new_slope <- beta1 * divisor

round(new_slope, 2)