### Course Project for Data Extraction and Cleaning

rm(list = ls())

## Features

setwd("~/Documents/coursera3/UCI HAR Dataset")
features = read.table(file = "features.txt", header = FALSE)
names(features) = c("id","features")
names = as.vector(features$features)

## train data set

setwd("~/Documents/coursera3/UCI HAR Dataset/train")

subTrain = read.table(file = "subject_train.txt", header = FALSE)
names(subTrain)[1] = "subject"

xTrain = read.table(file = "X_train.txt",header = FALSE)

yTrain = read.table(file = "y_train.txt",header = FALSE)
names(yTrain)[1] = "activity"

xTrain = cbind(subTrain,yTrain,xTrain)

## test data set

setwd("~/Documents/coursera3/UCI HAR Dataset/test")

subTest = read.table(file = "subject_test.txt", header = FALSE)
names(subTest)[1] = "subject"

yTest = read.csv(file = "y_test.txt",header = FALSE)
names(yTest)[1] = "activity"

xTest = read.table(file = "X_test.txt", header = FALSE)
xTest = cbind(subTest,yTest,xTest)

###Merging test and train
finalData = rbind(xTrain,xTest)
finalData = finalData[order(finalData$subject),]

#give column names
for(i in 3:563){
  pos1 = i - 1
  colnames(finalData)[i] = names[pos1]
}
 
# take only variables of interest
Interest = finalData[,c((grep("mean()|std()",colnames(finalData))))]

sign = NULL
for(i in 1:ncol(Interest)){
  if(!(grepl("meanFreq()",colnames(Interest)[i]))){
    sign = c(sign,i)
  }
}
final = Interest[,sign]

#compile them together
final = cbind(finalData$activity,finalData$subject,final)
names(final)[1] = "activity"
names(final)[2] = "subject"

### Activity descriptions

for(i in 1:nrow(final)){
  if(final$activity[i] == 1){
    final$activity[i] = "WALKING"
  } else if(final$activity[i] == 2){
    final$activity[i] = "WALKING_UPSTAIRS"
  } else if(final$activity[i] == 3){
    final$activity[i] = "WALKING_DOWNSTAIRS"
  } else if(final$activity[i] == 4){
    final$activity[i] = "SITTING"
  } else if(final$activity[i] == 5){
    final$activity[i] = "STANDING"
  } else{
    final$activity[i] = "LAYING"
  }
}

#### get tidy data set

tidyData = aggregate(.~ activity + subject, data = final, mean)

write.table(tidyData,file = "tidyData.txt",row.name=FALSE)























 