



L3train<- read.csv("./Datasets/loan_high_train.csv")
L3test <- read.csv("./Datasets/loan_high_test.csv")
L1train <- read.csv("./Datasets/loan_low_train.csv")
L1test <- read.csv("./Datasets/loan_low_test.csv")
L2train <- read.csv("./Datasets/loan_med_train.csv")
L2test <- read.csv("./Datasets/loan_med_test.csv")
Ltrain <- read.csv("./Datasets/loan_all_train.csv")
Ltest <- read.csv("./Datasets/loan_all_test.csv")




library(caret)
  

xgb_class <- function(data_train, data_test, para) {
  result <- c()
  acc <- c()
  rec <- c()
  output <- data.frame()
  library(foreach)
  library(doParallel)
  
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  #.combine = "cbind", .multicombine = TRUE,
  result <- foreach(i = 1:nrow(para), .combine = "rbind", .packages = c("xgboost"),  .verbose = TRUE) %dopar% {
      dtrain <- xgb.DMatrix(data.matrix(subset(data_train, select = -c(loan_status))), label = data_train[, "loan_status"])
      cv <- xgb.cv(data = dtrain, nrounds = 5, nfold = 5, metrics = list("error"),
                  max_depth = para[i,]$depth, eta = para[i,]$eta, min_child_weight = para[i,]$min_child_weight, 
                  objective = "binary:logistic", gamma = para[i,]$gamma, verbose = TRUE)
      
      result[i] <- sum(1-cv$evaluation_log[,2])/5
      
  }
   stopCluster(cl)
   best <- para[order(result[,1], decreasing = T)[1],]
   best$mean_accuracy <- max(result)
  
   return(best)
}

cf <- function(L3, L3train, L3test, from, to, by){
  fit <- c()
  acc <- c()
  rec <- c()
  pred <- c()
  itera <- seq(from, to, by = by)
  trn <- as.factor(L3train[, "loan_status"])
  tet <- as.factor(L3test[,"loan_status"])
  library(foreach)
  library(doParallel)
  
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  ite <- foreach(i = 1:length(itera), .combine = "rbind", .packages = c("xgboost"),  .verbose = TRUE) %dopar% {

  fit <- xgboost(data = data.matrix(subset(L3train, select = -c(loan_status))),
               label = L3train[, "loan_status"], max_depth = L3$depth, eta = L3$eta,
                                                             min_child_weight = L3$min_child_weight,
                                                             objective = "binary:logistic", gamma = L3$gamma, nrounds = itera[i])
  pred <- predict(fit, data.matrix(subset(L3test, select = -c(loan_status))))
  pred <- ifelse(pred >= .5, 1, 0 )
  acc[i] <- mean(as.factor(pred) == tet)
  conf <- table(pred, L3test$loan_status)
  rec[i] <- conf[2,2]/(conf[1,2] + conf[2,2])
   data.frame(itera[i], acc[i], rec[i])
  

  }
  
  return(ite)
}




para3 <- expand.grid(max_depth = c(8,9,10), eta = c(0.3,0.4,0.2), min_child_weight = c(10,9,8), gamma = c(0,1,5,10)) 
names(para3) <- c("depth", "eta", "min_child_weight","gamma")

para2 <- expand.grid(max_depth = c(9,10), eta = c(0.37,0.4,0.42), min_child_weight = c(6,7,8), gamma = c(0.25,1,.5)) 
names(para2) <- c("depth", "eta", "min_child_weight","gamma")

para1 <- expand.grid(max_depth = c(9,10), eta = c(0.37,0.4,0.42), min_child_weight = c(6,7,8), gamma = c(0.3,.2)) 
names(para1) <- c("depth", "eta", "min_child_weight","gamma")

para <- expand.grid(max_depth = c(9,10), eta = c(0.37,0.42), min_child_weight = c(10,9,8), gamma = c(0.6,1,1.3)) 
names(para) <- c("depth", "eta", "min_child_weight","gamma")


L3 <- xgb_class(L3train, L3test, para3)
L3
L3_ite <- cf(L3, L3train, L3test, 1, 100, 5)
L3_ite

L2 <- xgb_class(L2train, L2test, para2)
L2
L2_ite <- cf(L2, L2train, L2test, 10, 500,10)
L2_ite


L1 <- xgb_class(L1train, L1test, para1)
L1
L1_ite <- cf(L1, L1train, L1test, 1, 320, 5)
L1_ite

L <- xgb_class(Ltrain, Ltest, para)
L
L_ite <- cf(L, Ltrain, Ltest, 10, 800, 50)
L_ite

library(ggplot2)
library(tidyr)
par(mfrow=c(2,2))
L3_data <-
  data.frame(
    Accuracy3 = L3_ite[,2],
    Recall3 = L3_ite[,3],
    iter = L3_ite[,1])
  
L3_data %>%
    gather(High,rate, Accuracy3, Recall3) %>%
    ggplot(aes(x=iter, y=rate, colour = High)) +
    geom_line()


L2_data <-
  data.frame(
    Accuracy2 = L2_ite[,2],
    Recall2 = L2_ite[,3],
    iter = L2_ite[,1])
  
L2_data %>%
    gather(Middle,rate, Accuracy2, Recall2) %>%
    ggplot(aes(x=iter, y=rate, colour = Middle)) +
    geom_line()

L1_data <-
  data.frame(
    Accuracy1 = L1_ite[,2],
    Recall1 = L1_ite[,3],
    iter = L1_ite[,1])
  
L1_data %>%
    gather(Low,rate, Accuracy1, Recall1) %>%
    ggplot(aes(x=iter, y=rate, colour = Low)) +
    geom_line()

L_data <-
  data.frame(
    Accuracy = L_ite[,2],
    Recall = L_ite[,3],
    iter = L_ite[,1])
  
L_data %>%
    gather(All,rate, Accuracy, Recall) %>%
    ggplot(aes(x=iter, y=rate, colour = All)) +
    geom_line()






































































































































