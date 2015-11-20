---
title: "Practical Machine Learning Project"
author: "Yanan_Zhao"
date: "November 18, 2015"
---

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har>

## Getting Data

The training data for this project are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://groupware.les.inf.puc-rio.br/har>.


```r
setInternet2(TRUE)
if (!file.exists("training.csv")) 
  {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = 'training.csv')
}

traindata <- read.csv("training.csv",na.strings = c("NA", ""),header = TRUE)

if (!file.exists("testing.csv")) 
  {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = 'testing.csv')
  }
testdata <- read.csv("testing.csv",na.strings = c("NA", ""),header = TRUE)
```

## Data cleaning and Preprocessing

Most of the variables in the training data set are not useful and some of them are near zero variance.


```r
library('caret')
nzv <- nearZeroVar(traindata, saveMetrics= TRUE)
```

After carefully reading the literature explaining what these variables are, I decide to keep the following parameters as variables, which are:
-roll_belt, pitch_belt, yaw_belt, roll_arm, pitch_arm, yaw_arm, roll_forearm, pitch_forearm, yaw_forearm, roll_dumbbell, pitch_dumbbell, yaw_dumbbell, classe


```r
library(dplyr)
traindata_sle <- select(traindata, roll_belt, pitch_belt, yaw_belt, roll_arm, pitch_arm, yaw_arm, roll_forearm, pitch_forearm, yaw_forearm, roll_dumbbell, pitch_dumbbell, yaw_dumbbell, classe)
```

## Data analysis

First, let's load the corresponding library.


```r
library(rpart)
```

Second, to do to data training, I am planning to use 10 fold cross validation for the training data set. I employeed random tree as the training method.


```r
set.seed(123)
folds <- createFolds(y=traindata_sle$classe,k=10,list=TRUE,returnTrain=FALSE)
err.vect <- rep(0, times = 10)
for(i in 1:10) {
  test_subset <- traindata_sle[folds[[i]],]
  train_subset <- traindata_sle[-folds[[i]],]
  modFit <- train(classe~., method = "rf", data = train_subset)
  testPC <- predict(modFit, test_subset)
  err.vect[i] <- confusionMatrix(test_subset$classe, testPC)$overall[1]
  print(paste("Accuracy for fold", i, ":", err.vect[i]))
}
```

```
## [1] "Accuracy for fold 1 : 0.989296636085627"
## [1] "Accuracy for fold 2 : 0.993880673125956"
## [1] "Accuracy for fold 3 : 0.989811512990321"
## [1] "Accuracy for fold 4 : 0.987767584097859"
## [1] "Accuracy for fold 5 : 0.989816700610998"
## [1] "Accuracy for fold 6 : 0.991335372069317"
## [1] "Accuracy for fold 7 : 0.990316004077472"
## [1] "Accuracy for fold 8 : 0.992860785313616"
## [1] "Accuracy for fold 9 : 0.986245542536933"
## [1] "Accuracy for fold 10 : 0.990825688073395"
```

```r
print(paste("Average Accuracy:", mean(err.vect)))
```

```
## [1] "Average Accuracy: 0.990215649898149"
```

As shown from the previous cross validation results, I get pretty good accuracy for random tree method. The average accuracy is 99%. So, the expected out of sample error should be around 1%.

## Prediction of testing data

First let's subset the variables from the testing dataset.

```r
testdata_sle <- select(testdata, roll_belt, pitch_belt, yaw_belt, roll_arm, pitch_arm, yaw_arm, roll_forearm, pitch_forearm, yaw_forearm, roll_dumbbell, pitch_dumbbell, yaw_dumbbell)
```

Next, I will apply the previous build model to do predictions.


```r
Pre_test <- predict(modFit, testdata_sle)
```

The code uploading the prediction results are as following.


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(Pre_test)
```
