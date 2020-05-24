library(HistData)
library(caret)
library(tidyverse)

set.seed(1983)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  dplyr::select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son)
# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

#use predict with de fitted model
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#comprehesion Check linear Regression
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#question 1
set.seed(1, sample.kind="Rounding") 
RMSE<-replicate(n,{
  y<-dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit<-lm(y~x,data=train_set)
  y_hat<-predict(fit,test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(RMSE)
sd(RMSE)

#Question 2
set.seed(1, sample.kind="Rounding") 
n<-c(100, 500, 1000, 5000, 10000)
RMSE_func<-function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE<-replicate(100,{
    y<-dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit<-lm(y~x,data=train_set)
    y_hat<-predict(fit,test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}
map(n,RMSE_func)

#question 3
set.seed(1) 
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  RMSE<-replicate(100,{
    y<-dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit<-lm(y~x,data=train_set)
    y_hat<-predict(fit,test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
RMSE_func(100)


#question 6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
reg_1<-lm(y~x_1,train_set)
yhat<-predict(reg_1,test_set)
sqrt(mean((yhat - test_set$y)^2))

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
reg_2<-lm(y~x_2,train_set)
yhat<-predict(reg_2,test_set)
sqrt(mean((yhat - test_set$y)^2))

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
reg_3<-lm(y~x_1+x_2,train_set)
yhat<-predict(reg_3,test_set)
sqrt(mean((yhat - test_set$y)^2))
