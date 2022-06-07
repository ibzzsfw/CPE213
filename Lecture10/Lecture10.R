library(tidyverse)
library(corrplot)
library(caret)

credit <- read.csv("./credit-approval.csv")
names(credit)
names(credit)[names(credit) == "b"] <- "A1"
names(credit)[names(credit) == "X30.83"] <- "A2"
names(credit)[names(credit) == "X0"] <- "A3"
names(credit)[names(credit) == "u"] <- "A4"
names(credit)[names(credit) == "g"] <- "A5"
names(credit)[names(credit) == "w"] <- "A6"
names(credit)[names(credit) == "v"] <- "A7"
names(credit)[names(credit) == "X1.25"] <- "A8"
names(credit)[names(credit) == "t"] <- "A9"
names(credit)[names(credit) == "t.1"] <- "A10"
names(credit)[names(credit) == "X01"] <- "A11"
names(credit)[names(credit) == "f"] <- "A12"
names(credit)[names(credit) == "g.1"] <- "A13"
names(credit)[names(credit) == "X00202"] <- "A14"
names(credit)[names(credit) == "X0.1"] <- "A15"
names(credit)[names(credit) == "X."] <- "A16"

credit$A1 <- as.factor(credit$A1)
credit$A4 <- as.factor(credit$A4)
credit$A5 <- as.factor(credit$A5)
credit$A6 <- as.factor(credit$A6)
credit$A7 <- as.factor(credit$A7)
credit$A9 <- as.factor(credit$A9)
credit$A10 <- as.factor(credit$A10)
credit$A12 <- as.factor(credit$A12)
credit$A13 <- as.factor(credit$A13)
credit$A14 <- as.factor(credit$A14)
credit$A16 <- as.factor(credit$A16)

credit[credit == "?"] <- NA

creditCleaned <- drop_na(credit)

n <- nrow(creditCleaned)
table (creditCleaned$A16)
train_id <- sample(1: n, size = 0.7*n)
train <- creditCleaned[train_id,] 
test <- creditCleaned[-train_id,]

model1 <- glm(A16 ~., data = train, family = binomial) 
summary(model1)
predict1 <- predict(model1, train, type="response")
factor1 <- factor(ifelse(predict1>(295/n),"+","-"))
confusionMatrix (factor1, train$A16, positive = "+", mode = "prec_recall")

model2 <- glm(A16 ~ A1+A4+A5+A6+A7+A9+A10+A12+A13, data = train, family = binomial) 
summary(model2)
predict2 <- predict(model2, train, type="response")
factor2 <- factor(ifelse(predict2>(295/n),"+","-"))
confusionMatrix (factor2, train$A16, positive = "+", mode = "prec_recall")

predict_test <- predict(model2, newdata = test, type = 'response') 
factor_test <- factor(ifelse(predict_test>0.45,'+','-'))
factor_test
confusionMatrix (factor_test, test$A16, positive = '+', mode = 'prec_recall')


