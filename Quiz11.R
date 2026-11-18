#Q1
Under MLR1–MLR6 (bjhat-bj)/sd(bjhat) has a N (0,1)  distribution, but (bjhat - bj)/se(bjhat) has a 
t distribution. Why?    The standard error replaces the unknown with the estimate, which varies from sample to sample.

#Q3 Given a MLR how many degrees of freedom
n <- 53
K <- 3
df <- n - K - 1
df

#Q4 An estimate is bjaht = .81 with se(bjhat) = .41 Find the t-statistic for Hzero

t <- 0.81 / 0.41

#Q6 For a test of Hzero:Bj = 0 against H1 : Bj > 0 at the 5% level with df = 29. Find critical value
because Bj > 0
qt(0.95, df = 29)

#Q7 test of Hzero:Bj = 0 against H1 : Bj > 0 at the 5% level with df = 25 and c = 1.71. t = 1.52 what do you conclude?
Fail to reject Hzero because t < c

#Q8 For a test of Hzero:Bj = 0 against H1 : Bj < 0 at the 5% level with df = 29. Find critical value
because Bj < 0
qt(0.05, df = 29)

#Q9 test of Hzero:Bj = 0 against H1 : Bj < 0 at the 5% level with df = 30 and c = -1.70. t = 1.52. Two different samples t = -2.1 and t = -1.2
Reject Hzero when t = -2.1, fail to reject when t = -1.2

#Q14 test H0 : Bcigs = 0 against H1 : Bcigs < 0 at 1% level. Find the t-statistic for cigs.
reg <- lm(bwght ~ cigs + faminc, data = bwght)
summary(reg)$coefficients[2, 3]

#Q16 What can you conclude at the 1% level
Reject h0 because t < c, more cigarettes per day are associated with a lower birth weight, holding family income fixed.
reg <- lm(bwght ~ cigs + faminc, data = bwght)
t <- summary(reg)$coefficients["cigs", "t value"]
c <- qt(0.01, df.residual(reg))

#Q17 what should the slope be? .129 is the current slope
round(0.129 * 100, 2)

#Q20 find the predicted house price for a 2,000-square-foot house with 3 bedrooms
reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
prediction <- predict(reg, newdata = data.frame(sqrft = 2000, bdrms = 3))
exp(prediction)
round(exp(predict(reg, newdata = data.frame(sqrft = 2000, bdrms = 3))), 2)


