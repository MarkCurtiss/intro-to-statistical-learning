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

# 5.??  How am I supposed to make a y-sub-i-tilde

# 6. ??

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


