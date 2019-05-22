# 6.

# a) Perform polynomial regression to predict wage using age.  Use cross-validation to select the optimal
# degree d for the polynomial.  What degree was chosen, and how does this compare to the results of hypothesis
# testing using ANOVA?  Make a plot of the resulting polynomial fit to the data.

library(ISLR)
library(boot)

set.seed(1)
error_rates <- rep(0,6)

for (i in 1:6) {
    wage_by_age.fit <- glm(wage~poly(age,i,raw=TRUE), data=Wage)
    error_rates[i] <- cv.glm(Wage, wage_by_age.fit, K=10)$delta[1]
}
error_rates

# [1] 1675.882 1602.562 1597.223 1595.184 1599.612 1596.589
# Degree 4 was chosen (the quartic polynomial)


best.fit <- glm(wage~poly(age,4,raw=TRUE),data=Wage)
sorted_ages <- sort(unique(Wage$age))
predictions <- predict(best.fit, newdata=data.frame(age=sorted_ages))
plot(Wage$age, Wage$wage)
lines(sorted_ages, predictions, col="red")


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
    Wage$age.cut <- cut(Wage$age, i)
    wage_by_age.fit <- glm(wage~age.cut, data=Wage)
    error_rates_by_cut[i] <- cv.glm(Wage, wage_by_age.fit, K=10)$delta[1]
}
error_rates_by_cut
 [1]    0.000 1735.051 1682.455 1635.132 1633.602 1622.005 1611.415 1602.668
 [9] 1609.955 1603.193
min(error_rates_by_cut[-1])
# [1] 1602.668
# 8 is the coolest number of cuts


# 7. The Wage data set contains a number of other features not explored in this chapter, such as marital status
# (maritl), job class (jobclass), and others.  Explore the relationship between some of these other predictors
# and wage, and use non-linear fitting techniques in order to fit flexible models to the data.  Create plots
# of the results obtained, and write a summary of your findings.

library(splines)
install.packages('gam')
install.packages('akima')
library(gam)
library(akima)
gam.fit <- gam(wage~maritl+jobclass+s(age,4), data=Wage)
plot.Gam(gam.fit)

# 8. Fit some of the non-linear models investigated in this chapter to the Auto data set.  Is there evidence
# for non-linear relationships in this data set?  Create some informative plots to justify your answer.
spline.fit <- glm(mpg~s(horsepower,4)+s(weight,4)+s(displacement,4),data=Auto)
error_rate <- cv.glm(Auto, spline.fit, K=10)$delta[1]
plot(spline.fit)

lm.fit <- lm(mpg~horsepower,data=Auto)
spline.fit <- glm(mpg~ns(horsepower,4), data=Auto)
lm.line <- data.frame(Auto$horsepower, predict(lm.fit, newdata=Auto))
spline.line <- data.frame(Auto$horsepower, predict(spline.fit, newdata=Auto))
plot(Auto$horsepower, Auto$mpg)
lines(sort(Auto$horsepower), sort(predict(lm.fit, newdata=Auto)), col="blue")
abline(lm.fit, col="blue")
abline(spline.fit, col="red")
lines(
        sort(predict(spline.fit, newdata=Auto)),
       sort(Auto$horsepower),
        col="red"
)
lines(c(0,0), c(80,160), lwd=8, col="red")
abline(spline.line, col="red")
# ha ha why can't I graph this spline??!

# 9. This question uses the variables dis (the weighted mean of distances to five Boston employment centers)
# and nox (nitrogen oxides concentration in parts per 10 million) from the Boston data. We will treat dis as
# the predictor and nox as the response.
# a) Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.

library(MASS)
poly.fit <- glm(nox~poly(dis, 3, raw=TRUE), data=Boston)
summary(poly.fit)

plot(Boston$dis, Boston$nox)
x <- seq(min(Boston$dis), max(Boston$dis))
y <- predict(poly.fit, newdata=data.frame(dis=x))

lines(x,y,col="red")
## x <- sort(sample(Boston$dis, 100))
## y <- predict(poly.fit, newdata=data.frame(dis=x))
## lines(x,y, col="red")

# b) Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report
# the associated residual sum of squares.

set.seed(1)
error_rates <- rep(0,10)

for (i in 1:10) {
    nox_by_dis.fit <- glm(nox~poly(dis, i, raw=TRUE), data=Boston)
    plot(Boston$dis, Boston$nox, main=paste("polynomial fit of degree",i))
    x <- seq(min(Boston$dis), max(Boston$dis))
    y <- predict(nox_by_dis.fit, newdata=data.frame(dis=x))
    lines(x,y,col="red")
    ## readline(prompt=paste("polynomial fit of degree", i))
    Sys.sleep(2)
    error_rates[i] <- mean((nox_by_dis.fit$fitted.values - Boston$nox)^2)
}
error_rates
