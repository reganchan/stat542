data(Caravan)

train <- within(Caravan[-c(1:1000),], rm(Purchase))
train$y <- Caravan[-c(1:1000),]$Purchase=="Yes"
test <- within(Caravan[1:1000,], rm(Purchase))
test$y <- Caravan[c(1:1000),]$Purchase=="Yes"

library(MASS)
model <- glm(y~., family=binomial, data=train)
pred <- predict(model, test, type="response")
sum(pred[test$y] <= 0.25)
sum(pred[!test$y] >= 0.25)
library(pROC)
roc(test$y, pred)

aic <- stepAIC(glm(y~1, family=binomial, data=train), direction="forward", scope=formula(model))
pred_aic <- predict(aic, test, type="response")
sum(pred_aic[!test$y] > 0.25)
sum(pred_aic[test$y] < 0.25)
roc(test$y, pred_aic)

k <- log(nrow(train)-1000)
bic <- stepAIC(glm(y~1, family=binomial, data=train), direction="forward", scope=formula(model), k=k)
pred_bic <- predict(bic, test, type="response")
sum(pred_bic[!test$y] > 0.25)
sum(pred_bic[test$y] < 0.25)
roc(test$y, pred_bic)

library(glmnet)
l1 <- glmnet(as.matrix(within(train, rm(y))), train$y, family=c("binomial"), alpha=1, lambda = 0.004)
pred_l1 <- predict(l1, as.matrix(within(test, rm(y))), type="response")
sum(pred_l1[!test$y] > 0.25)
sum(pred_l1[test$y] < 0.25)
roc(test$y, pred_l1)
