data(spam)
train <- spam[-c(1:100,1901:1960),]
test  <- spam[c(1:100,1901:1960),]
train$spam <- as.factor(train$spam=="spam")
test$spam  <- as.factor(test$spam=="spam")

model <- svm(spam~., train, kernel='linear', cost=1)
nrow(model$SV)
sum(model$fitted != train$spam)
sum(predict(model, test) != test$spam)

model <- svm(spam~., train, kernel='linear', cost=10)
nrow(model$SV)
sum(model$fitted != train$spam)
sum(predict(model, test) != test$spam)

model <- svm(spam~., train, kernel='linear', cost=50)
nrow(model$SV)
sum(model$fitted != train$spam)
sum(predict(model, test) != test$spam)

model <- svm(spam~., train, kernel='radial', cost=1)
nrow(model$SV)
err.train <- sum(model$fitted != train$spam)
err.test  <- sum(predict(model, test) != test$spam)

model <- svm(spam~., train, kernel='radial', cost=10)
nrow(model$SV)
err.train <- sum(model$fitted != train$spam)
err.test  <- sum(predict(model, test) != test$spam)

model <- svm(spam~., train, kernel='radial', cost=50)
nrow(model$SV)
err.train <- sum(model$fitted != train$spam)
err.test  <- sum(predict(model, test) != test$spam)
