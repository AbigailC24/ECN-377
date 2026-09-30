#Find the OLS residual of a fitted line
# Given values
x <- 8
y <- 19

# Calculate the predicted y
y_hat <- -3 + (8/10) * x
y_hat
# Calculate the residual
u_hat <- y - y_hat

# Round to hundredths
round(u_hat, 2)




#prediction w/ OLS
x = c()
y = c()
reg = lm(y~x)
b0 = reg$coefficients[1]
b1 = reg$coefficients[2]
b0 + b1 * x



#find SSR with sample data
x = c(5,10,4)
y = c(9,13,9)
b0 = 0
b1 = 4/10
fitted_vals = b0 + b1 * x
resids = y - fitted_vals
sum(resids^2)


#b1 = cov(X,Y)/var()
#OLS Slope
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
beta1 <- numerator / denominator

# Round to hundredths
round(beta1, 2)







#**Always remember to create a vector! x = c()

#mean of values
mean()
#sum of values
sum()
#SAMPLE variance
var()
#standard deviation
sd()
#correlation
cor(x,y)
#covariance
cov()

xj <- c(0,0,1,1)        # x across pairs (0,0) (0,1) (1,0) (1,1)
yj <- c(0,1,0,1)        # y across those same pairs
pj <- rep(1/4,4)        # probability of each pair
EXY <- sum(xj*yj*pj) 
EX= sum(xj*pj)  # E[XY]                 -- same idea, weight xj*yj
EY = sum(yj*pj)
CovXY <- EXY-EX*XY     # Cov = E[XY] - E[X]*E[Y]    -- independent, so expect 0
  

#Probability

#X, E[X]    (X*probability) + (X*probability)
x = c(1,2) p = c(1/2,1/2)   sum(x*p)

#X^2, E[X^2]    (X^2 * probability) +  (X^2 * probability)
x = c(1,2,) p = (.5,.5) sum(x^2*p)

#X and Y are independent, then Cov(X,Y) = 0 b/c E[X^2] -E[X] * E[Y] = 0

#Var of Random Variables = E[X^2] -(E[x])^2

#covariance is unitless

# E[Y|X=x] is the average of Y

# X and Y are independent Var(X,Y)= ?  Var(X,Y) = Var(X+Var(Y) + 2Cov(X,Y)

# Cov(X,Y) = E[X,Y] - E

#OLS Slpoe
  # Enter the data
  x <- c(5, 6, 8)
  y <- c(9, 12, 12)

  # Find the means
  xbar <- mean(x)
  ybar <- mean(y)
  
  # Calculate the numerator
  numerator <- sum((x - xbar) * (y - ybar))

  # Calculate the denominator
  denominator <- sum((x - xbar)^2)

  # OLS slope
  b1 <- numerator / denominator

  # Round to hundredths
  round(b1, 2)
  
#Find OLS Intercept
  # Enter the data
  x <- c(7, 4, 4)
  y <- c(2, 1, 0)
  
  # Find the OLS slope
  b1 <- cov(x, y) / var(x)
  
  # Find the intercept
  b0 <- mean(y) - b1 * mean(x)
  
  # Round to hundredths
  round(b0, 2)

    
#Fit the OLS line (predictions with OLS line)
x <- c(4, 8, 2)
y <- c(4, 2, 2)

b1 = cov(x,y) / var(x)
b0 = mean(y) - mean(x) * b1
b0 - b1 * 10
  
#Joint Probability Distribution
  x <- c(5, 4, 3)
  y <- c(5, 1, 2)
  p <- c(0.2, 0.3, 0.5)
  
  # Expected values
  EX <- sum(x * p)
  EY <- sum(y * p)
  
  # Expected value of XY
  EXY <- sum(x * y * p)
  
  # Covariance
  covXY <- EXY - EX * EY
  
  round(covXY, 2)
  
  
#Population Regression Slope
  x <- c(3, 1, 6)
  y <- c(7, 0, 0)
  p <- c(0.2, 0.3, 0.5)
  
  # Expected values
  EX <- sum(x * p)
  EY <- sum(y * p)
  
  # Covariance
  EXY <- sum(x * y * p)
  covXY <- EXY - EX * EY
  
  # Variance of X
  EX2 <- sum(x^2 * p)
  varX <- EX2 - EX^2
  
  # Population regression slope
  beta1 <- covXY / varX
  
  round(beta1, 2)
  
#Calculate Sample Covariance from data
  x <- c(6, 3, 1)
  y <- c(0, 4, 1)
  
  cov(x, y)
  
