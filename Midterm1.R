
#**Always remember to create a vector! x <- c()




# ** PROBLEM SET 7 ** #

# Set up in R
library(wooldridge)
data("wage1")
data("bwght")

# Number of observations
nraw(wage1) 

# Sample mean of hourly wage
mean(wage1$wage)

#Sample standard deviation of years of educ.
sd(wage1$educ)

# Run an OLS Regression of wage on education
reg <- lm(wage ~ educ, data = wage1)
# Save the slope as b1
b1 <- reg$coefficients[2]

#Save the intercept from the previous line
b0 <- reg$coefficients[1]

#What is the predicted hourly wage at educ = 14?
wagehat <- b0 + b1 * educ
wagehat <- b0 + b1 * 14

# How much does predicted hourly wage change when educ rises bby 2?
wagehat <- b1 * deltaeduc
wagehat <- b1 * 2

# What is the sum of squared residuals?
SSR <- sum(reg$residuals^2)
SSR

#SST
SST <- sum((wage1$wage - mean(wage1$wage))^2)

# R^2 for the regression of wage on education
R2 <- 1 - SSR / SST

# Check the answer
summary(reg)$r.squared

#New data set
reg2 <- lm(bwght ~ cigs, data = bwght)

# Save the slope as b1
b1 <- reg2$coefficients[2]

#Save the intercept from the previous line
b0 <- reg2$coefficients[1]

# By how many ounces does predicted birth weight change when the mother smokes 5 more cigarettes per day?
bwghthat <- b1 * 5



#mean of values  (Xbar and Ybar)
mean()
#sum of values
sum()
#SAMPLE variance     (how far are numbers in sample spread around the mean)
var()

x <- c()
var(x)
# SAMPLE covariance
x <- c()
y <- c()
cov(x,y)

#standard deviation    (back to regular units)
sd()

x <- c()
sd(x)
#correlation        always falls between -1 and 1
cor(x,y)


#covariance   NOT SAMPLE COVARIANCE
cov(X,Y)

x <- c(2,0,5)        # x across pairs (0,0) (0,1) (1,0) (1,1)
y <- c(1,3,2)        # y across those same pairs
p <- c(1/3, 1/3, 1/3)        # probability of each pair
EX <-  sum(x * p)  
EY <-  sum(y * p)
EXY <- sum(x * y * p) 
CovXY <- EXY-EX*EY     # Cov = E[XY] - E[X]*E[Y]    



#percentage change
((new - old) / old) * 100
#percentage point change
new_ - old_


# Multiple Linear Regression Model
y <- b0 + b1 * X1 + b2 * X2
    # y = dependent variable
    # X1 and X2 = 1st and 2nd independent variable
    # b0 = intercept, predicted y, when X1 and X2  = 0
    # b1 = effect of X1 on y, when holding X2 constant
    # b2 = effect of X2 on y, when holding X1 constant
deltaY <- b1 * X1 + b2 * X2 
                    # only plug in independent variables not the intercept


# Sumple Linear Regression Equations
y <- b0 + b1 * X1
# Change in y
deltaY <- b1 * X1   # don't include b0 b/c it's the intercept and it doesn't effect change
# Finding the predicted y at a given x value, also given b0, b1
y <- b0 + b1 * X1
                    # include b0 b/c it's the intercept and starting point to find the total y
    


# Expected values
x <- c(6, 8, 3)
y <- c(1, 2, 3)
p <- c(.2, .3, .5)
EX <- sum(x * p)
EY <- sum(y * p)

# Expected values w/ x^2 (EX2)
x <- c(1, 2, 3)
p <- c(.5, .5, .5)
EX2 <- sum(x^2 * p)

# Expected value of XY
EXY <- sum(x * y * p)

# Covariance
covXY <- EXY - EX * EY

# Variance of a random variable X
EX2 <- sum(x^2 * p)
varX <- EX2 - EX^2

# Conditional value of Y    (what is the average value of Y for people/observations at certain value of x)
E[Y | X = x]


#Given Var(X) = 6, a = 2, find Var(X) = a^2 * X + b
#plug numbers into 
a^2 * var(x)


# When given cov(x,y) and sd() 
cov_xy <- 2
sd_x <- 3
sd_y <- 4

cor_xy <- cov_xy / (sd_x * sd_y)


#OLS Slope
#**b1 = cov(X,Y)/var()
# Enter the data
x <- c(8, 8, 3)
y <- c(4, 3, 4)

# Find the means
x_bar <- mean(x)
y_bar <- mean(y)

# Calculate the numerator
numerator <- sum((x - x_bar) * (y - y_bar))

# Calculate the denominator
denominator <- sum((x - x_bar)^2)

# Calculate the OLS slope
b1 <- numerator / denominator



#Find OLS Intercept
# Enter the data
x <- c(7, 4, 4)
y <- c(2, 1, 0)

# Find the OLS slope
b1 <- cov(x, y) / var(x)

# Find the intercept
b0 <- mean(y) - b1 * mean(x)



************************
#prediction w/ OLS (find the predicted value of y_hat at a given x)
x = c(2, 4, 8)
y = c(11, 4, 0)
reg = lm(y~x)
b0 = reg$coefficients[1]
b1 = reg$coefficients[2]
b0 + b1 * x


#Fit the OLS line (predictions with OLS line) 
x <- c(1, 1, 2)
y <- c(3, 2, 9)

b1 = cov(x,y) / var(x)
b0 = mean(y) - mean(x) * b1
b0 - b1 * x

********************
  
#Find the OLS residual of a fitted line
# Given values
x <- 8
y <- 19

# Calculate the predicted y
y_hat <- -3 + (8/10) * x
y_hat
# Calculate the residual
u_hat <- y - y_hat


  
# With a fitted line E[colGPS | hsGPA] = 11/10 + 4/10 * hsGPA, find expected colGPA w/ hsGPA 31/10
hsGPA <- 31/10
colGPA <- 11/10 + (4/10) * hsGPA

# Even given the function wage = b0 + b1 * educ + U, to find change in wage only use
wage <- b1 * deltaeduc


# Find SSR with a fitted line
# Enter the data
x <- c(9, 10, 9)
y <- c(9, 12, 11)
# Find predicted values
y_hat <- 4 + (7/10) * x
y_hat
# Find the residuals
residuals <- y - y_hat
residuals
# Now square the residuals
SSR <- sum(residuals^2)

#Regressions
SST = SSE + SSR


# R^2
SST <- 276
SSR <- 34

R2 <- 1 - SSR/SST



#find SSR with sample data
x = c(5,10,4)
y = c(9,13,9)
b0 = 0
b1 = 4/10
fitted_vals = b0 + b1 * x
resids = y - fitted_vals
sum(resids^2)






#Probability

#X and Y are independent, then Cov(X,Y) = 0 b/c E[X^2] - E[X] * E[Y] = 0

#Var of Random Variables = E[X^2] -(E[x])^2

#covariance is unitless

# E[Y|X=x] is the average of Y

# X and Y are independent Var(X,Y)= ?  Var(X,Y) = Var(X+Var(Y) + 2Cov(X,Y)

# Cov(X,Y) = E[X,Y] - E












