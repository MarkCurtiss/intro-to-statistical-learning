# 6.

# a) Perform polynomial regression to predict wage using age.  Use cross-validation to select the optimal
# degree d for the polynomial.  What degree was chosen, and how does this compare to the results of hypothesis
# testing using ANOVA?  Make a plot of the resulting polynomial fit to the data.

library(ISLR)

set.seed(1)
error_rates <- rep(0,6)

for (i in 1:6) {
    train <- sample(dim(Wage)[1], 1500)
    wage_by_age.fit <- lm(wage~poly(age,i,raw=TRUE), data=Wage[train,])
    wage_by_age.predictions <- predict(wage_by_age.fit, newdata=Wage)
    error_rates[i] <- mean((wage_by_age.predictions - Wage$age)^2)
}
error_rates

# [1] 4753.035 4941.360 4853.229 4678.132 4807.466 4907.984
# Degree 4 was chosen (the quartic polynomial)


fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age,2), data=Wage)
fit.3 <- lm(wage~poly(age,3), data=Wage)
fit.4 <- lm(wage~poly(age,4), data=Wage)
fit.5 <- lm(wage~poly(age,5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

## Model 1: wage ~ age
## Model 2: wage ~ poly(age, 2)
## Model 3: wage ~ poly(age, 3)
## Model 4: wage ~ poly(age, 4)
## Model 5: wage ~ poly(age, 5)
##   Res.Df     RSS Df Sum of Sq        F    Pr(>F)
## 1   2998 5022216
## 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
## 3   2996 4777674  1     15756   9.8888  0.001679 **
## 4   2995 4771604  1      6070   3.8098  0.051046 .
## 5   2994 4770322  1      1283   0.8050  0.369682

# Degree 4 was chosen.

# b) Fit a step function to predict wage using age, and perform cross-validation to choose the optimal number
# of cuts.  Make a plot of the fit obtained.

num_cuts_to_try <- 10
error_rates_by_cut <- rep(0, num_cuts_to_try)

for (i in 2:num_cuts_to_try) {
        paste("now trying ", i)
    train <- sample(dim(Wage)[1], 1500)
    wage_by_age.fit <- lm(wage~cut(age,i), data=Wage[train,])
    wage_by_age.predictions <- predict(wage_by_age.fit, newdata=Wage)
    error_rates_by_cut[i] <- mean((wage_by_age.predictions - Wage$age)^2)
}
error_rates_by_cut
# [1]    0.000 4826.944 4765.125 4585.585 4823.932 4790.949 4948.497 4945.938
# [9] 4852.716 4885.233
min(error_rates_by_cut[-1])
## [1] 4585.585
# 3 cuts is the coolest number of cuts

# 7. The Wage data set contains a number of other features not explored in this chapter, such as marital status
# (maritl), job class (jobclass), and others.  Explore the relationship between some of these other predictors
# and wage, and use non-linear fitting techniques in order to fit flexible models to the data.  Create plots
# of the results obtained, and write a summary of your findings.

library(splines)
install.packages('gam')
install.packages('akima')
library(gam)
library(akima)

gam.lr <- gam(I(wage>250)~s(year,df=3)+s(age,df=5), family=binomial, data=Wage)
plot(gam.lr, se=TRUE)
