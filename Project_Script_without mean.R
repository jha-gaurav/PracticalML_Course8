library(caret)
library(ggplot2)
library(randomForest)
library(corrplot)
library(rpart)

set.seed(1000)
#Step 1: Get the data

mainDir <- getwd()
subDir <- "outputDirectory"

if (!file.exists(subDir)){
        dir.create(file.path(mainDir, subDir))
}

if(!file.exists("outputDirectory/training.csv")){
        fileURL_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileURL_train, destfile = "outputDirectory/training.csv")
        
}

if(!file.exists("outputDirectory/test.csv")){
        fileURL_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileURL_test, destfile = "outputDirectory/test.csv")
        
}

df_train <- read.csv("outputDirectory/training.csv", stringsAsFactors = FALSE)
df_test <- read.csv("outputDirectory/test.csv", stringsAsFactors = FALSE)

df_train <- df_train[, -c(1, 3:7)]
df_train$user_name <- as.factor(df_train$user_name)
df_train$classe <- as.factor(df_train$classe)
str(df_train)
#Create training and test datasets from the training datasets.

for(i in 1:ncol(df_train)){
        
        if(class(df_train[,i]) == "character"){
                
                df_train[,i] <- as.numeric(df_train[,i])
        }
}

num_na <- sapply(1:ncol(df_train), function(x) sum(is.na(df_train[,x])))

df_train_new <- df_train[, num_na == 0]
str(df_train_new)



inTrain <- createDataPartition(y = df_train_new$classe, p = 0.75, list = FALSE)

tempTrain <- df_train_new[inTrain,]
tempTest <- df_train_new[-inTrain,]

table(tempTrain$classe)


#Create a new dataframe with user name and classe variables.
#The other features will be added subsequently to this dataframe

model1 <- train(classe ~., data = tempTrain, method = "rpart")

#model2 <- train(classe ~., data = tempTrain, method = "rf")

model3 <- train(classe ~., data = tempTrain, method = "gbm", verbose = FALSE)


confusionMatrix(predict(model1, newdata = tempTest), tempTest$classe)

#confusionMatrix(predict(model2, newdata = tempTest), tempTest$classe)

confusionMatrix(predict(model3, newdata = tempTest), tempTest$classe)


predict(model3, newdata = df_test)