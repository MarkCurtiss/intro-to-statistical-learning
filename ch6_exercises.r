install.packages('leaps')
library('leaps')

predict.regsubsets <- function(object,newdata,id,...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[,xvars]%*%coefi
}

install.packages('glmnet')
library(glmnet)

install.packages('pls')
library(pls)

# 8.
# a)
set.seed(1)
X <- rnorm(100)
noise <- rnorm(100)
# b)
Y <- 1+2*X+3*X^2+3*X^3+noise
# c)
data.full <- data.frame(X, Y)
regfit <- regsubsets(Y~poly(X,10), data.full, nvmax=10)
par(mfrow=c(2,2))
plot(regfit, scale="adjr2")
plot(regfit, scale="Cp")
plot(regfit, scale="bic")
coef(regfit,3)
# (Intercept) poly(X, 10)1 poly(X, 10)2 poly(X, 10)3
#    4.243527    86.765815    41.923436    45.183672

# d)
regfit.fwd <- regsubsets(Y~poly(X,10), data.full, nvmax=10)

plot(regfit.fwd, scale="adjr2")
plot(regfit.fwd, scale="Cp")
plot(regfit.fwd, scale="bic")
coef(regfit.fwd, 3)
graphics.off()
## (Intercept) poly(X, 10)1 poly(X, 10)2 poly(X, 10)3
##    4.243527    86.765815    41.923436    45.183672

regfit.back <- regsubsets(Y~poly(X,10), data.full, nvmax=10)
par(mfrow=c(2,2))
plot(regfit.back, scale="adjr2")
plot(regfit.back, scale="Cp")
plot(regfit.back, scale="bic")
coef(regfit.back, 3)
graphics.off()
## (Intercept) poly(X, 10)1 poly(X, 10)2 poly(X, 10)3
##    4.243527    86.765815    41.923436    45.183672

# e)
set.seed(1)
x_data <- poly(X,10,simple=TRUE)

lasso.model <- glmnet(x_data, Y, alpha=1)
plot(lasso.model)

cv.model <- cv.glmnet(x_data, Y, alpha=1)
cv.model$lambda.min
# [1] 0.03266677
plot(cv.model)
lasso.predictions <- predict(lasso.model, type="coefficients", s=cv.model$lambda.min)
lasso.predictions
# The resulting model has 7 variables with the following coefficient estimates:
## 11 x 1 sparse Matrix of class "dgCMatrix"
##                        1
## (Intercept)  4.243526722
## 1           86.439147362
## 2           41.596767988
## 3           44.857004182
## 4            0.930427281
## 5            1.153520639
## 6            .
## 7           -0.003098054
## 8            .
## 9            .
## 10          -0.624561786

# f)
new_Y <- 1+7*(X^7)+noise
new_data <- data.frame(X, new_Y)
new_regfit <- regsubsets(Y~X, new_data, nvmax=10)

par(mfrow=c(2,2))
plot(new_regfit, scale="adjr2")
plot(new_regfit, scale="Cp")
plot(new_regfit, scale="bic")
coef(new_regfit, 3)
 ## (Intercept) poly(X, 10, raw = TRUE)1 poly(X, 10, raw = TRUE)2
 ## 1.061507                 1.975280                 2.876209

graphics.off()

set.seed(1)
lasso.model <- glmnet(x_data, new_Y, alpha=1)
cv.model <- cv.glmnet(x_data, new_Y, alpha=1)
cv.model$lambda.min
## [1] 7.507068
lasso.predictions <- predict(lasso.model, type="coefficients", s=cv.model$lambda.min)
lasso.predictions
## 11 x 1 sparse Matrix of class "dgCMatrix"
##                      1
## (Intercept)   30.50469
## 1           2273.53630
## 2            898.90729
## 3           3026.85923
## 4            494.73254
## 5           1165.73243
## 6             34.15447
## 7            127.60532
## 8              .
## 9              .
## 10             .

# 9.
# a)
library(ISLR)

set.seed(1)
training_college <- sample(c(TRUE, FALSE), nrow(College), TRUE)
test_college <- (!training_college)

# b)
lm.fit <- lm(Apps~.,data=College[training_college,])
