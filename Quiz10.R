# A regression loghaty with changes in x1 and x2, what percent does predicted y change
100 (b1hat * deltaX1 + b2hat * deltaX2)

# What percent does wage change, experience and tenure changes, holding education constant
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b1 <- reg$coefficients[1]
b2 <- reg$coefficients[2]
b3 <- reg$coefficients[3]
b4 <- reg$coefficients[4]

100 * (b3*2 + b4*1)

# Using logwage, what is the predicted hourly wage 
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b1 <- reg$coefficients[1]
b2 <- reg$coefficients[2]
b3 <- reg$coefficients[3]
b4 <- reg$coefficients[4]

exp(b1 + b2 * 12 + b3 * 10 + b4 * 3)

#Q4

# A MLR including sample means, what is the sample mean of y
b0 <- 
b1 <- 
b2 <- 
xbar1 <- 
xbar2 <- 

ybar <- b0 + b1 * xbar1 + b2 * xbar2

#Q7

#Q9

# Formula 3.23, given the SLR MLR and slope, find the multiple regression slope
b1tilda <- 
b2hat <-
sigma1tilda <- 

b1hat <- b1tilda - b2hat * sigma1tilda

#





