high.train <- read.csv("loan_high_train.csv")
high.test <- read.csv("loan_high_test.csv")
low.train <- read.csv("loan_low_train.csv")
low.test <- read.csv("loan_low_test.csv")
med.train <- read.csv("loan_med_train.csv")
med.test <- read.csv("loan_med_test.csv")
all.train <- read.csv("loan_all_train.csv")
all.test <- read.csv("loan_all_test.csv")


## select threshold
library(ggplot2)
logistic_th <- function(train, test){
  fullmod <- glm(loan_status~., train, family = binomial)
  backwards <- step(fullmod, trace = 0)
  lg_pre <- predict(backwards, test, type = "response")
  threshold <- seq(0, 0.5, by = 0.01)
  accuracy <- rep(NA, length(threshold))
  recall <- rep(NA, length(threshold))
  count <- 1
  for (t in threshold){
    log_pred <- ifelse(lg_pre > t, 1, 0)
    log_pred <- factor(log_pred, levels = 0:1)
    conf <- table(log_pred, test$loan_status)
    accuracy[count] <- (conf[1,1]+conf[2,2])/sum(conf)
    recall[count] <- conf[2,2]/(conf[1,2]+conf[2,2])
    count <- count + 1
  }
  metric <- data.frame(threshold, accuracy, recall)
  p <- ggplot(metric, aes(threshold)) + 
    geom_line(aes(y = accuracy, colour = "accuracy")) + 
    geom_line(aes(y = recall, colour = "recall"))
  print(p)
  return(metric)
}


logistic_th(low.train, low.test)
logistic_th(high.train, high.test)
logistic_th(med.train, med.test)
logistic_th(all.train, all.test)



## importance
high.mod <- glm(loan_status~., high.train, family = binomial)
high.re <- step(high.mod, trace = 0)
summary(high.re)

med.mod <- glm(loan_status~., med.train, family = binomial)
med.re <- step(med.mod, trace = 0)
summary(med.re)

low.mod <- glm(loan_status~., low.train, family = binomial)
low.re <- step(low.mod, trace = 0)
summary(low.re)

all.mod <- glm(loan_status~., all.train, family = binomial)
all.re <- step(all.mod, trace = 0)
summary(all.re)

