#Level-Log: 1% increase in X → β₁/100 unit change in Y.
#Log-Level: 1-unit increase in X → approximately β₁ × 100% change in Y.
#Log-Log: 1% increase in X → approximately β₁% change in Y.
#Q1 a regression gives testscorehat <- 12 + 8log(income) , what does the 8 mean?
  #A 1% rise in income is associated with a rise of about 0.08 points in the predicted test score.

#Q2 log-level regression: log(y) = .9 +.07x what % does y change, x falls by 3
  #  b0=.9 b1=.07       %changeinY = 100 * b1 * changeinX ==> 100 * .07 * -3

#Q3 run the log-log regression
lm(log(bwght) ~ log(faminc), data = bwght)
  # b1=log(faminc) so %changeinY = b1 * changeinX ==> .02 * 20

#Q4 Suppose you could draw 1,000 different random samples from the same population and compute in each one. If OLS is unbiased (SLR1–SLR4 hold)
  #The average of the 1,000 estimates is close to b1

#Q6 which changes would make var(b1hat) larger?
  #X values bunched more tightly together, smaller SSTx, a smaller sample, larger error variance

#Q7 Find the standard error of a simple regression
n <- 14
sigma <- 4.6
Sx <- 0.5

SSTx <- (n - 1) * Sx^2

SE <- sigma / sqrt(SSTx)

#Q9 (sd(b1) / sqrt(amount times as large))
round(0.65 / sqrt(8), 2)

#Q12 find prdeicted y value at given X1 and X2 (just plug in)
yhat = b0 + b1X1 + b2X2

#Q13 a multiple regression gives yhat = b0 + b1X1 - b2X2
b1 <-2.9
b2 <- -1.1
chnginX1 <- 4
chnginX2 <- 2
chnginY <- b1*chnginX1 + b2*chnginX2

#Q14 multiply each coefficient by their change and add together
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
wage <- reg$coefficients[1]
educ <- reg$coefficients[2]
exper <- reg$coefficients[3]
tenure <- reg$coefficients[4]
wage + educ*14 + exper*10 + tenure*5

#Q15 if only one variable is changed how much does wage change
tenure*3
