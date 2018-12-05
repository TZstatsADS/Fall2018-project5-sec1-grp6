library(rpart)
library(rpart.plot)

loan.low.train <- read.csv("./Datasets/loan_low_train.csv")
loan.low.test <- read.csv("./Datasets/loan_low_test.csv")
## Low data
loan.low.test$loan_status <- as.factor(loan.low.test$loan_status)
loan.low.train$loan_status <- as.factor(loan.low.train$loan_status)

model.rpart <- rpart(factor(loan_status) ~., data = loan.low.train,method = "class", control=rpart.control(minsplit=20, maxdepth=8))

printcp(model.rpart)
plotcp(model.rpart)
cp1 <- model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"]
model.rpart1 <- rpart(factor(loan_status) ~., data = loan.low.train,
                      control=rpart.control(minsplit=20, maxdepth=6,cp=0.000013))

prp(model.rpart1, cex = 0.6)


Ltree.pre <- predict(model.rpart1, loan.low.test, type = "class")
t1 <- table(Ltree.pre, loan.low.test$loan_status)
t1
acc1 <- sum(loan.low.test$loan_status==Ltree.pre)/length(Ltree.pre)
a <- t1[4]/(t1[4]+t1[3])

## All data
loan.all.train <- read.csv("./Datasets/loan_all_train.csv")
loan.all.test <- read.csv("./Datasets/loan_all_test.csv")

## All data
loan.all.test$loan_status <- as.factor(loan.all.test$loan_status)
loan.all.train$loan_status <- as.factor(loan.all.train$loan_status)

model.rpart2 <- rpart(factor(loan_status) ~., data = loan.all.train,method = "class", control=rpart.control(minsplit=20, maxdepth=8))
printcp(model.rpart2)
plotcp(model.rpart2)

cp1 <- model.rpart2$cptable[which.min(model.rpart2$cptable[,"xerror"]),"CP"]

model.rpart3 <- rpart(factor(loan_status) ~., data = loan.all.train,
                      control=rpart.control(minsplit=20, maxdepth=6,cp=0.00024))
prp(model.rpart3, cex = 0.6)
L1tree.pre <- predict(model.rpart3, loan.all.test, type = "class")
t2 <- table(L1tree.pre, loan.all.test$loan_status)
t2
acc2 <- sum(loan.all.test$loan_status==L1tree.pre)/length(L1tree.pre)
b <- t2[4]/(t2[4]+t2[3])


## Med data
loan.med.train <- read.csv("./Datasets/loan_med_train.csv")
loan.med.test <- read.csv("./Datasets/loan_med_test.csv")

## Med data
loan.med.test$loan_status <- as.factor(loan.med.test$loan_status)
loan.med.train$loan_status <- as.factor(loan.med.train$loan_status)

model.rpart <- rpart(factor(loan_status) ~., data = loan.med.train,method = "class", control=rpart.control(minsplit=20, maxdepth=8))
printcp(model.rpart)
plotcp(model.rpart)

cp1 <- model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"]
model.rpart1 <- rpart(factor(loan_status) ~., data = loan.med.train,
                      control=rpart.control(minsplit=20, maxdepth=6,cp=0.0005))
prp(model.rpart1, cex = 0.6)

L2tree.pre <- predict(model.rpart1, loan.med.test, type = "class")
t3 <- table(L2tree.pre, loan.med.test$loan_status)
t3
acc3 <- sum(loan.med.test$loan_status==L2tree.pre)/length(L2tree.pre)
c <- t3[4]/(t3[4]+t3[3])


## High data
loan.high.train <- read.csv("./Datasets/loan_high_train.csv")
loan.high.test <- read.csv("./Datasets/loan_high_test.csv")

## High data
loan.high.test$loan_status <- as.factor(loan.high.test$loan_status)
loan.high.train$loan_status <- as.factor(loan.high.train$loan_status)

model.rpart <- rpart(factor(loan_status) ~., data = loan.high.train,method = "class", control=rpart.control(minsplit=20, maxdepth=8))
printcp(model.rpart)
plotcp(model.rpart)

cp1 <- model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"]
model.rpart1 <- rpart(factor(loan_status) ~., data = loan.high.train,
                      control=rpart.control(minsplit=20, maxdepth=6,cp=0.001))
prp(model.rpart1, cex = 0.6)
L3tree.pre <- predict(model.rpart1, loan.high.test, type = "class")
t4 <- table(L3tree.pre, loan.high.test$loan_status)
t4
acc4 <- sum(loan.high.test$loan_status==L3tree.pre)/length(L3tree.pre)
d <- t4[4]/(t4[4]+t4[3])


