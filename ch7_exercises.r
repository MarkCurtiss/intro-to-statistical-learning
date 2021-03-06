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
par(mfrow=c(3,1))
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
sorted_horses <-       sort(Auto$horsepower, decreasing=TRUE)
lines(
   Auto$horsepower,
  predict(spline.fit, newdata=data.frame(horsepower=sorted_horses)),
   col="red"
)
lines(c(0,0), c(80,160), lwd=8, col="red")
plot(spline.fit)
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
    error_rates[i] <- sum((nox_by_dis.fit$fitted.values - Boston$nox)^2)
}
error_rates
min(error_rates)
# R is frustrating.  Why can't I just get the RSS off of the fit's coefficients
# like I can with other models.  Is it because I used glm() instead of lm()?
# Is it because I fit a polynomial?

# c) Perform cross-validation or another approach to select the opti mal degree for the polynomial,
# and explain your results.
set.seed(1)
cv.error_rates <- rep(0, 10)

for (i in 1:10) {
    nox_by_dis.fit <- glm(nox~poly(dis, i, raw=TRUE), data=Boston)
    cv.error_rates[i] = cv.glm(Boston, nox_by_dis.fit, K=10)$delta[1]
}
cv.error_rates
min(cv.error_rates)
# huh.  This picked the quartic polynomial.  It must provide the right balance between
# bias and variance.

# d)  Use the bs() function to fit a regression spline to predict nox using dis.
# Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.
library(splines)
spline.fit <- glm(nox~bs(dis,degree=4,df=4),data=Boston)
# Eyeballing "dis", 4 knots looked reasonable..
summary(spline.fit)

plot(Boston$dis, Boston$nox)
x <- seq(min(Boston$dis), max(Boston$dis))
y <- predict(spline.fit, newdata=data.frame(dis=x))
lines(x,y,col="red")

# e) Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits and report
# the resulting RSS. Describe the results obtained.
set.seed(1)
spline.error_rates <- rep(0,10)

for (i in 1:10) {
    spline.fit <- glm(nox~bs(dis,degree=i,df=i),data=Boston)

    plot(Boston$dis, Boston$nox, main=paste("regression spline of degree",i))
    x <- seq(min(Boston$dis), max(Boston$dis))
    y <- predict(spline.fit, newdata=data.frame(dis=x))
    lines(x,y,col="red")
    Sys.sleep(2)

    spline.error_rates[i] <- sum((spline.fit$fitted.values - Boston$nox)^2)
}
spline.error_rates
min(spline.error_rates)
# the 10-degree spline again !


# f) Perform cross-validation or another approach in order to select the best degrees of freedom for a
# regression spline on this data. Describe your results.
set.seed(1)
cv.spline.error_rates <- rep(0, 10)

for (i in 1:10) {
    spline.fit <- glm(nox~bs(dis,degree=i,df=i),data=Boston)
    cv.spline.error_rates[i] = cv.glm(Boston, spline.fit, K=10)$delta[1]
}
cv.spline.error_rates
min(cv.spline.error_rates)
# This picked a cubic spline (although it also emitted a bunch of warnings)
# 10.This question relates to the College data set.
# a) Split the data into a training set and a test set. Using out-of-state tuition as the response
# and the other variables as the predictors, perform forward stepwise selection on the training set
# in order to identify a satisfactory model that uses just a subset of the predictors.
train <- sample(nrow(College)/2)
test <- (-train)

library(leaps)
regfit.fwd <- regsubsets(Outstate~., data=College[test,], nvmax=18, method="forward")
summary(regfit.fwd)

par(mfrow=c(2,2))
plot(regfit.fwd, scale="r2")
plot(regfit.fwd, scale="adjr2")
plot(regfit.fwd, scale="Cp")
plot(regfit.fwd, scale="bic")
coef(regfit.fwd, 10)
names(coef(regfit.fwd, 10))
##  [1] "(Intercept)" "PrivateYes"  "Top10perc"   "Room.Board"  "Books"
##  [6] "PhD"         "Terminal"    "S.F.Ratio"   "perc.alumni" "Expend"
## [11] "Grad.Rate"

# b) Fit a GAM on the training data, using out-of-state tuition as the response and the features selected in
# the previous step as the predictors. Plot the results, and explain your findings.
library(gam)
gam.fit <- gam(Outstate~Private+Top10perc+Room.Board+Books+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate, data=College[train,])

par(mfrow=c(2,5))
plot.Gam(gam.fit)
# Ha ha.  I can't explain anything about these plots.  Why are they all straight lines ?

# c) Evaluate the model obtained on the test set, and explain the results obtained.
predictions <- predict(gam.fit, newdata=College[test,])
(mean(predictions - College[test,]$Outstate)^2)
# I guess it is not good!
