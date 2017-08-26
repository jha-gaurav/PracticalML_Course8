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

#Create training and test datasets from the training datasets.

inTrain <- createDataPartition(y = df_train$classe, p = 0.75, list = FALSE)

tempTrain <- df_train[inTrain,]
tempTest <- df_train[-inTrain,]


#Create a new dataframe with user name and classe variables.
#The other features will be added subsequently to this dataframe

new.train <- cbind(tempTrain$user_name, tempTrain$classe)
new.test <- cbind(tempTest$user_name, tempTest$classe)
new.final.test <- df_test$user_name


colnames(new.train) <- colnames(new.test) <- c("user_name", "classe")


#The input data contains data from four categories, i.e. belt, arm, forearm and dumbbell.
#We will analyse each class one by one.

temp_belt_df <- tempTrain[,grep("belt", colnames(tempTrain))]
temp_arm_df <- tempTrain[,grep("_arm", colnames(tempTrain))]
temp_forearm_df <- tempTrain[,grep("forearm", colnames(tempTrain))]
temp_dumbbell_df <- tempTrain[,grep("dumbbell", colnames(tempTrain))]


temp_belt_df.test <- tempTest[,grep("belt", colnames(tempTest))]
temp_arm_df.test <- tempTest[,grep("_arm", colnames(tempTest))]
temp_forearm_df.test <- tempTest[,grep("forearm", colnames(tempTest))]
temp_dumbbell_df.test <- tempTest[,grep("dumbbell", colnames(tempTest))]
str(temp_belt_df)


belt_df.test <- df_test[,grep("belt", colnames(df_test))]
arm_df.test <- df_test[,grep("_arm", colnames(df_test))]
forearm_df.test <- df_test[,grep("forearm", colnames(df_test))]
dumbbell_df.test <- df_test[,grep("dumbbell", colnames(df_test))]
#Let's analyse the belt data first

str(temp_belt_df)

sapply(1:ncol(temp_belt_df), function(x) sum(is.na(df_train[,x])))
#The fields kurtosis, skewness, max_yaw, min_yaw and amplitude_yaw are characters 
#and have lot of missing values. We will convert these fields to numeric as they contain numbers.

for(i in 1:ncol(temp_belt_df)){
        
        if(class(temp_belt_df[,i]) == "character"){
                
                temp_belt_df[,i] <- as.numeric(temp_belt_df[,i])
                temp_belt_df.test[,i] <- as.numeric(temp_belt_df.test[,i])
                belt_df.test[,i] <- as.numeric(belt_df.test[,i])
        }
        if(class(temp_arm_df[,i]) == "character"){
                temp_arm_df[,i] <- as.numeric(temp_arm_df[,i])
                temp_arm_df.test[,i] <- as.numeric(temp_arm_df.test[,i])
                arm_df.test[,i] <- as.numeric(arm_df.test[,i])
        }
        
        if(class(temp_forearm_df[,i]) == "character"){
                temp_forearm_df[,i] <- as.numeric(temp_forearm_df[,i])
                temp_forearm_df.test[,i] <- as.numeric(temp_forearm_df.test[,i])
                forearm_df.test[,i] <- as.numeric(forearm_df.test[,i])
        }
        
        if(class(temp_dumbbell_df[,i]) == "character"){
                temp_dumbbell_df[,i] <- as.numeric(temp_dumbbell_df[,i])
                temp_dumbbell_df.test[,i] <- as.numeric(temp_dumbbell_df.test[,i])
                dumbbell_df.test[,i] <- as.numeric(dumbbell_df.test[,i])
                
        }
        
        
}

#Calculate mean of all columns



for(i in 1:ncol(temp_belt_df)){
        
        m1 <- mean(temp_belt_df[,i], na.rm = TRUE)
        temp_belt_df[is.na(temp_belt_df[,i]), i] <- m1
        temp_belt_df.test[is.na(temp_belt_df.test[,i]), i] <- m1
        belt_df.test[is.na(belt_df.test[,i]), i] <- m1
        
        m1 <- mean(temp_arm_df[,i], na.rm = TRUE)
        temp_arm_df[is.na(temp_arm_df[,i]), i] <- m1
        temp_arm_df.test[is.na(temp_arm_df.test[,i]), i] <- m1
        arm_df.test[is.na(arm_df.test[,i]), i] <- m1
        
        m1 <- mean(temp_forearm_df[,i], na.rm = TRUE)
        temp_forearm_df[is.na(temp_forearm_df[,i]), i] <- m1
        temp_forearm_df.test[is.na(temp_forearm_df.test[,i]), i] <- m1
        forearm_df.test[is.na(forearm_df.test[,i]), i] <- m1
        
        m1 <- mean(temp_dumbbell_df[,i], na.rm = TRUE)
        temp_dumbbell_df[is.na(temp_dumbbell_df[,i]), i] <- m1
        temp_dumbbell_df.test[is.na(temp_dumbbell_df.test[,i]), i] <- m1
        dumbbell_df.test[is.na(dumbbell_df.test[,i]), i] <- m1
}

str(temp_belt_df)

#The fields kurtosis_yaw_belt and skewness_yaw_belt have NaN values. Will drop them.

temp_belt_df$kurtosis_yaw_belt <- NULL
temp_belt_df$skewness_yaw_belt <- NULL


str(temp_arm_df)

#All features for arm are good.

str(temp_forearm_df)

#The fields kurtosis_yaw_forearm and skewness_yaw_forearm have NaN values. Will drop them.

temp_forearm_df$kurtosis_yaw_forearm <- NULL
temp_forearm_df$skewness_yaw_forearm <- NULL

str(temp_dumbbell_df)

#The fields kurtosis_yaw_dumbbell and skewness_yaw_dumbbell have NaN values. Will drop them.

temp_dumbbell_df$kurtosis_yaw_dumbbell <- NULL
temp_dumbbell_df$skewness_yaw_dumbbell <- NULL

#Create a cleat training dataset

final_train <- cbind(new.train, temp_belt_df, temp_arm_df, temp_forearm_df, temp_dumbbell_df)
final_test <- cbind(new.test, temp_belt_df.test, temp_arm_df.test, temp_forearm_df.test, temp_dumbbell_df.test)

str(final_train)

#Create a model

model1 <- train(classe ~., data = final_train, method = "rpart")

model2 <- train(classe ~., data = final_train, method = "rf")

#predict(model1, newdata = final_test)

confusionMatrix(predict(model1, newdata = final_test), final_test$classe)

confusionMatrix(predict(model2, newdata = final_test), final_test$classe)

sapply(1:ncol(df_test), function(x) sum(is.na(df_test[,x])))

test_df <- cbind(new.final.test, belt_df.test, arm_df.test, forearm_df.test, dumbbell_df.test)
colnames_temp <- c(colnames(belt_df.test), colnames(arm_df.test), colnames(forearm_df.test), colnames(dumbbell_df.test))
colnames(test_df) <- c("user_name", colnames_temp)

#predict(model1, newdata = test_df)