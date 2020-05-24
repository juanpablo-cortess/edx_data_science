library(dslabs)
library(tidyverse)
library(caret)

data("heights")
# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#assessment
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#use type to predict sex by guessing 
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

sensitivity(data = y_hat,reference = y)
confusionMatrix(data = y_hat, reference = y)

#assesment part 2
library(caret)
library(broom)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
train$Species <- factor(train$Species)

tidy_iris<-iris %>% gather(key = caract,value = length,-Species)
tidy_iris %>% ggplot(aes(x=Species,y=length,fill=Species)) + 
  geom_bar(stat = 'identity') + facet_wrap('caract')

# examine the accuracy-cutoffs
#petal.length
cutoff <- seq(range(iris$Petal.Length)[1],range(iris$Petal.Length)[2],0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
test$Species <- factor(test$Species)
mean(y_hat == test$Species)

resume<-data.frame(cutoff,accuracy)
resume %>% 
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point(stat = 'identity') + labs(title = 'accuracy petal length')
caret::confusionMatrix(y_hat,test$Species)

#petal.width
cutoff <- seq(range(iris$Petal.Width)[1],range(iris$Petal.Width)[2],0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
test$Species <- factor(test$Species)
mean(y_hat == test$Species)

resume<-data.frame(cutoff,accuracy)
resume %>% 
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point(stat = 'identity') + labs(title = 'accuracy petal width')
caret::confusionMatrix(y_hat,test$Species)

#sepal.length
cutoff <- seq(range(iris$Sepal.Length)[1],range(iris$Sepal.Length)[2],0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
test$Species <- factor(test$Species)
mean(y_hat == test$Species)

resume<-data.frame(cutoff,accuracy)
resume %>% 
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point(stat = 'identity') + labs(title = 'accuracy petal width')
caret::confusionMatrix(y_hat,test$Species)

#sepal.width
cutoff <- seq(range(iris$Sepal.Width)[1],range(iris$Sepal.Width)[2],0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
test$Species <- factor(test$Species)
mean(y_hat == test$Species)

resume<-data.frame(cutoff,accuracy)
resume %>% 
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point(stat = 'identity') + labs(title = 'accuracy petal width')
caret::confusionMatrix(y_hat,test$Species)


#petal.width and length
cutoff <- seq(range(iris$Petal.Width)[1],range(iris$Petal.Width)[2],0.1)
cutoff2 <- seq(range(iris$Petal.Length)[1],range(iris$Petal.Length)[2],0.1)
best_cutoff_pl<-4.7
for (i in cutoff2){}
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x|train$Petal.Length>best_cutoff_pl, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Width > best_cutoff|train$Petal.Length>best_cutoff_pl, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
test$Species <- factor(test$Species)
mean(y_hat == test$Species)

resume<-data.frame(cutoff,accuracy)
resume %>% 
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point(stat = 'identity') + labs(title = 'accuracy petal width')
caret::confusionMatrix(y_hat,test$Species)
