# 1.
# The null hypothesis is that there is no relationship between TV,
# radio, newspaper, and sales.
# We can conclude that there is a relationship between TV and radio
# advertising and sales.  We can also conclude that there is no
# relatioship between newspaper advertising and sales.

# 2.
# KNN regression is attempting to predict a quantitative response.  KNN
# classification is attemping to predict a qualitatives response.

# 3.
# a)
# For a fixed value of IQ and GPA, females earn more on average than males
# provided GPA is high enough.
# b)
salary=function(gpa,iq,gender){
 return ((50 + (20*gpa) + (0.07*iq) + (35*gender) + (0.01*gpa*iq) + (-10*gpa*gender)) * 1000)
}
salary(gpa=4.0,iq=110,gender=1)
# 137100
# c) False.  It isn't the coefficient of the interaction effect that indicates
# significance, it is the t-statistic and p-value.

# 4.
# a)
# I'd expect the cubic regression to have a lower training RSS than the
# linear regression as it can more closely fit the training data.
# b)
# I'd expect the linear regression to have a lower test RSS, as it won't
# have overfit the training data.
# c)
# I again expect the cubic regression to more tightly fit the training
# data and thus have a lower training RSS.
# d)
# I don't think there's enough information to tell, since we don't know
# whether the true relationship is closer to linear or cubic.

# 5. *TODO*  How am I supposed to make a y-sub-i-tilde
# 6. *TODO*
# 7. My study group is skipping this one.

# 8.
# a)
Auto <- read.csv("~/Documents/intro_to_statistical_learning/Auto.csv", header=T, na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)
regression <- lm(mpg~horsepower, Auto)
summary(regression)
# i. Yes
# ii. Very strong - the p-value is tiny and the coefficient is -24.49
# iii. Negative

predict(regression, data.frame(horsepower=c(98)), interval="confidence")
predict(regression, data.frame(horsepower=c(98)), interval="prediction")

# iv. 24.46708 is the predicted mpg, with an interval of (23.97308, 24.96108) and
#     a prediction interval of (14.8094, 34.12476)
# b)
plot(horsepower, mpg)
abline(regression, lwd=3, col="red")

# c) The 'u' shape in the residuals vs fitted graph indicates non-linearity in the data.
#    The studentized residuals show that we have 3 outliers.

# 9)
# a)
plot(Auto)
# b)
cor(Auto[,1:8])
##                     mpg  cylinders displacement horsepower     weight
## mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442
## cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273
## displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944
## horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377
## weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000
## acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392
## year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199
## origin        0.5652088 -0.5689316   -0.6145351 -0.4551715 -0.5850054
##              acceleration       year     origin
## mpg             0.4233285  0.5805410  0.5652088
## cylinders      -0.5046834 -0.3456474 -0.5689316
## displacement   -0.5438005 -0.3698552 -0.6145351
## horsepower     -0.6891955 -0.4163615 -0.4551715
## weight         -0.4168392 -0.3091199 -0.5850054
## acceleration    1.0000000  0.2903161  0.2127458
## year            0.2903161  1.0000000  0.1815277
## origin          0.2127458  0.1815277  1.0000000
# c)
fit = lm(Auto$mpg~Auto$cylinders+
        Auto$displacement+
        Auto$horsepower+
        Auto$weight+
        Auto$acceleration+
        Auto$year+
        Auto$origin)
summary(fit)
# i. There is a relationship between the predictors and response.  The F-statistic is
# 252.
# ii. Weight, year, and origin.
# iii. Every increase in year adds another 3/4 mpg to fuel economy.
# d)
plot(fit)
plot(predict(fit), rstudent(fit))
# The slight u-shape in the residuals vs fitted plot suggests mild non-linearity in the data.
# There's one observation with very high leverage.
# There are a couple of points that are outliers: |rstudent| > 3 (in one case, 4!)
#
# e)
interact_fit = lm(mpg~cylinders*horsepower+
        displacement*weight+
        acceleration+
        year+
        origin)
summary(interact_fit)
# This netted me an F-statistic of 273.3 ; the interaction between cylinders and horsepower is
# statistically significant.
# f)
nonlinear_fit = lm(mpg~cylinders*horsepower+
        displacement*weight+
        acceleration+
        I(year^2)+
        origin)
summary(nonlinear_fit)
# Changing year to year^2 increase my F-statistic to 276.9 ; it also improved my R^2
# from .8215 to .8671

# 10.
# a)
library(ISLR)
attach(Carseats)
carseat.regression <- lm(Sales~Price+Urban+US)

# b)
summary(carseat.regression)
# Increases in price decrease sales.
# Less carseats are sold in urban areas.
# More carseats are sold in the USA.
# c)
# Sales = 13.043469 - 0.054459*Price - 0.021916*Urban[Yes] + 1.200573*US[Yes]
# d)
# Price and US[Yes] both have p-values close to 0 and we can reject the null
# hypothesis for them.
# e)
carseat.limited_regression <- lm(Sales~Price+US)
summary(carseat.limited_regression)
# f)
# They both fit it pretty closely with RSEs of 2.47
# g) Price
confint(carseat.limited_regression)
# Price  : (-0.06475984, -0.04419543)
# US[Yes]: (0.69151957, 1.70776632)

#h)
plot(carseat.limited_regression)
# I do see a high-leverage observation.
# I also see two studentized residuals close to 3.

#11.
# a)
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
no_intercept <- lm(y~x+0)
summary(no_intercept)
# The coefficient estimate is 1.9939, standard error is 0.1065, t-value is
# 18.73 and the p-value is <2e-16.  The high t-value and low p-value lead me
# to reject the null hypothesis.

# b)
another_non_intercept <- lm(x~y+0)
summary(another_non_intercept)
# The coefficient is 0.39111, standard error is 0.02089, t-value is
# 18.73 and the p-value is <2e-16.  The high t-value and low p-value lead me
# to reject the null hypothesis.

# c) The coefficients and p-values are exactly the same.
# d) *TODO*
# e) *TODO*
# f)
x_on_y <- lm(y~x)
y_on_x <- lm(x~y)
summary(x_on_y)
summary(y_on_x)
# t-value = 18.556

# 12.
# a) When there's a perfectly 1:1 relationship between the two.
# b)
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
x_on_y <- lm(y~x+0)
y_on_x <- lm(x~y+0)
summary(x_on_y)
summary(y_on_x)
# coefficient for x_on_y = 1.9939, coefficient for y_on_x = 0.39111

#c)
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
x_on_y <- lm(y~x+0)
y_on_x <- lm(x~y+0)
summary(x_on_y)
summary(y_on_x)
# coefficient for x_on_y = coefficient for y_on_x = -0.02148

#13.
# a)
set.seed(1)
x <- rnorm(100,mean=0,sd=1)

# b)
eps <- rnorm(100, mean=0, sd=0.25)

# c)
y <- -1 + (.5*x) + eps
length(y)
# 100
# B0 = -1, B1 = 0.5

# d)
plot(x,y)
# Most observations cluster around 0 which makes sense given the mean was set to 0 for both
# X and Y.

# e)
lm.fit <- lm(y~x)
summary(lm.fit)
# B^0 and B^1 are really close! -1.00942 and 0.49973, respectively

# f)
plot(x,y)
abline(lm.fit, lwd=3, col='red')
legend(x="top", legend="Simulated X vs Y with linear regression")

# g)
quad.fit <- lm(y~x+I(x^2))
plot(x,y)
abline(quad.fit, lwd=3, col="red")
# No.  Not only does the fit look the same, but the RSE and R^2 values are nearly the same.

# h)
set.seed(1)
x <- rnorm(100,mean=0,sd=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- -1 + (.5*x) + eps
lm.fit <- lm(y~x)
quad.fit <- lm(y~x+I(x^2))
# The linear regressions fit the data even more closely, with R^2 values of 0.9565.
# The t-value and p-values for the quadratic fit don't look great either.

# i)
set.seed(1)
x <- rnorm(100,mean=0,sd=1)
eps <- rnorm(100, mean=0, sd=10)
y <- -1 + (.5*x) + eps
lm.fit <- lm(y~x)
quad.fit <- lm(y~x+I(x^2))
plot(x,y)
abline(lm.fit, col="red", lwd=3)
abline(quad.fit, col="blue", lwd=3)
summary(lm.fit)
summary(quad.fit)
anova(lm.fit, quad.fit)
# The quadratic fit does a better job fitting this noisier data.
# The RSE is lower and the R^2 is an order of magnitude larger.

# j)
set.seed(1)
x <- rnorm(100,mean=0,sd=1)
eps <- rnorm(100, mean=0, sd=0.25)
y <- -1 + (.5*x) + eps
lm.fit <- lm(y~x)
confint(lm.fit)
#                  2.5 %     97.5 %
# (Intercept) -1.0575402 -0.9613061
# x            0.4462897  0.5531801

set.seed(1)
x <- rnorm(100,mean=0,sd=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- -1 + (.5*x) + eps
lm.fit <- lm(y~x)
confint(lm.fit)
#                  2.5 %     97.5 %
# (Intercept) -1.0230161 -0.9845224
# x            0.4785159  0.5212720

set.seed(1)
x <- rnorm(100,mean=0,sd=1)
eps <- rnorm(100, mean=0, sd=10)
y <- -1 + (.5*x) + eps
lm.fit <- lm(y~x)
confint(lm.fit)
#                 2.5 %    97.5 %
# (Intercept) -3.301607 0.5477552
# x           -1.648412 2.6272040

# 14.
# a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100)
# B0 = 2, B1 = 2, B2 = .03

# b) x2 is ~ .5*x1
plot(x1, x2)

# c)
lm.fit <- lm(y~x1+x2)
summary(lm.fit)
# B^0 = 1.9144, B^1 = 1.9604, B^2 = 0.5485
# B^0 and B^1 are close to the true predictors but B^2 is off.
# I can maybe reject the null hypothesis that B1 = 0 due to the p.value of 0.0146.
# I cannot reject the null hypothesis that B2 = 0.

# d)
lm.fit <- lm(y~x1)
summary(lm.fit)
# This is a much better fit.  I can reject the null hypothesis B1 = 0 due ot the
# tiny p-value.

# e)
lm.fit <- lm(y~x2)
summary(lm.fit)
# I can reject the null hypothesis that B1 = 0 due to the tiny p-value.

# f) No, the results demonstrate that the predictors are collinear.

# g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y,6)
lm.fit <- lm(y~x1+x2)
plot(lm.fit)
hatvalues(lm.fit)[100] / mean(hatvalues(lm.fit))
# This is not an outlier or a high-leverage point in this model, based on the plot
# and also the fact that our predicted value isn't much greater than the average
# prediction.
lm.fit <- lm(y~x1)
plot(lm.fit)
tail(hatvalues(lm.fit), 1) / mean(hatvalues(lm.fit))
# This is an outlier in this model due to it being a > 3 studentized residual.
# It's not quite large enough to be a high-leverage point, though.
lm.fit <- lm(y~x2)
plot(lm.fit)
tail(hatvalues(lm.fit), 1) / mean(hatvalues(lm.fit))
# This is a high leverage point in this model - it's 4.372822 times greater than
# the average hatvalue!

# 15.
# a)
library(MASS)
lm_pvalue <- function (x) {
   pvalue <- summary(x)$coefficients[2,4]
   pvalue
}

is_significant <- function(linear_model) {
     lm_pvalue(linear_model) < .05
}

predictors <- names(Boston)[2:14]
lapply(predictors, function(x) { paste(x, 'affects crime? ',
        is_significant(lm(crim~Boston[[x]], data=Boston))) }
)

# [[1]]
# [1] "zn affects crime?  TRUE"
# [[2]]
# [1] "indus affects crime?  TRUE"
# [[3]]
# [1] "chas affects crime?  FALSE"
# [[4]]
# [1] "nox affects crime?  TRUE"
# [[5]]
# [1] "rm affects crime?  TRUE"
# [[6]]
# [1] "age affects crime?  TRUE"
# [[7]]
# [1] "dis affects crime?  TRUE"
# [[8]]
# [1] "rad affects crime?  TRUE"
# [[9]]
# [1] "tax affects crime?  TRUE"
# [[10]]
# [1] "ptratio affects crime?  TRUE"
# [[11]]
# [1] "black affects crime?  TRUE"
# [[12]]
# [1] "lstat affects crime?  TRUE"
# [[13]]
# [1] "medv affects crime?  TRUE"


# Everything but 'chas' predicts crime!

# b)
all_lm <- lm(crim~.,data=Boston)
summary(all_lm)$coefficients[,4] < 0.05

# (Intercept)          zn       indus        chas         nox          rm
#        TRUE        TRUE       FALSE       FALSE       FALSE       FALSE

#         age         dis         rad         tax     ptratio       black
#       FALSE        TRUE        TRUE       FALSE       FALSE        TRUE

#       lstat        medv
#       FALSE        TRUE

# We can reject the null hypothesis for 'zn', 'dis', 'rad', 'black', and 'medv'.
# If we restrict our p-value to 1%, we can reject the null hypothesis for
# (Intercept)          zn       indus        chas         nox          rm
#       FALSE       FALSE       FALSE       FALSE       FALSE       FALSE

#         age         dis         rad         tax     ptratio       black
#       FALSE        TRUE        TRUE       FALSE       FALSE       FALSE

#       lstat        medv
#       FALSE        TRUE
# 'dis', 'rad', and 'medv'

# c)
univariate_coefficients <- c()
predictors <- names(Boston)[2:14]
for(predictor in predictors) {
    model <- as.formula(paste('crim~', predictor))
    univariate_coefficients <- c(univariate_coefficients,
                                coefficients(lm(model))[2])
}

all_lm <- lm(crim~.,data=Boston)
multivariate_coefficients <- coefficients(all_lm)[-1]
plot(univariate_coefficients, multivariate_coefficients)

# d)
has_nonlinear_association <- function(response, predictor) {
    linear_model <- as.formula(paste(response, '~', predictor))
    cubic_model <- as.formula(paste(response, '~ poly(', predictor, ',3)'))
    comparison <- anova(lm(linear_model), lm(cubic_model))
    comparison$'F'[2] > 10 && comparison$'Pr(>F)'[2] < .01
}

predictors <- c(names(Boston)[2:3], names(Boston)[5:14])
for(predictor in predictors) {
    if(has_nonlinear_association('crim', predictor)) {
        message(predictor, " has a non-linear association with crime")
    } else {
        message(predictor, " does not have a non-linear association with crime")
    }
}

# zn does not have a non-linear association with crime
# indus has a non-linear association with crime
# nox has a non-linear association with crime
# rm does not have a non-linear association with crime
# age has a non-linear association with crime
# dis has a non-linear association with crime
# rad does not have a non-linear association with crime
# tax has a non-linear association with crime
# ptratio does not have a non-linear association with crime
# black does not have a non-linear association with crime
# lstat does not have a non-linear association with crime
# medv has a non-linear association with crime
