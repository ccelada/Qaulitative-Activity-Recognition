setwd("~/ownCloud/Practical Machine Learning/Week 3/Project")
library(caret)
data <- read.csv("pml-training.csv", stringsAsFactors = FALSE)
assignment <- read.csv("pml-testing.csv", stringsAsFactors = FALSE)
data2 <- data[data$new_window == "no",]

set.seed(1512)
inTrain <- createDataPartition(data2$classe, p=0.75, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]


nzv <- nearZeroVar(training[training$new_window=="no",], saveMetrics = TRUE)
training_clean <- training[,!nzv$zeroVar]
training_clean <- training_clean[,7:59]

classess <- rep("",53)
for(i in 1:53){
    classess[i] <- class(training_clean[,i])
}

training_clean$classe <- as.factor(training_clean$classe)
set.seed(1276)
Model1 <- train(classe~., data=training_clean, method="rf",trControl = trainControl(method = "cv", number = 5,verboseIter = TRUE))
## Model1<- randomForest(classe~., data=training_clean)

Model1
Model1$finalModel


predicted <- predict(Model1, testing)
testing$classe <- as.factor(testing$classe)
sum(predicted == testing$classe)/length(predicted)
1-sum(predicted == testing$classe)/length(predicted)

## Assignment

pred_assignment <- predict(Model1, assignment)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(pred_assignment)




