---
title: "CodeBook.md"
author: "Sean Stoner"
date: "October 25, 2015"
output: html_document
---

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# **Getting and Cleaning Data Course Project**

## **Instructions Provided**

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

 You should create one R script called run_analysis.R that does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Good luck!

## **The Dataset**

The data used in this example comes from 30 volunteers who were within an age range of 19-48 years.  Each person performed six activities from walking upstairs to laying and their 3-axial linear acceleration and 3-axial angular velocity were measured using their Samsung Galaxy S II smartphones.  70% of the volunteers went into the training data and 30% into the test data.  For more information and to access the data, visit http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## The Script

First, we needed to download the data.

```{r}
## This section of code downloads the file and puts it in a folder in my specified path

filesPath <- "C:/Users/Sean/Documents/R Programming/data/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

## This section unzips the file
unzip(zipfile="./data/Dataset.zip",exdir="./data")
```

Next, we load the packages that we will need to use in our script.

```{r}
## This section loads the required packages
library(dplyr)
library(data.table)
library(tidyr)
```

There are two files for each section that we want to read data from:
* Train data
  + Subject - train/subject_train.txt
  + Activity - train/X_train.txt
  + Feature/Data Files - train/y_train.txt
* Test Data
  + Subject - test/subject_train.txt
  + Activity - test/X_train.txt
  + Feature/Data Files - test/X_train.txt
  
We will also be using the features.txt file and activity_labels.txt file

Let's load the first files.

```{r}
## This section reads the necessary training and test data from the files and stores them in R

filesPath <- "C:/Users/Sean/Documents/R Programming/data/UCI HAR Dataset"
# Reads the subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Reads the activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read the features data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))
```

Now that we have most of our data loaded into data tables in R, we can begin with our assigned tasks.

## 1. Merges the training and the test sets to create one data set.

```{r}
# This section combines the train and test data sets for both Activity and Subject files using rbind and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

# This section combines the features data training and test files
dataTable <- rbind(dataTrain, dataTest)

# This section names the variables according to feature in the features.txt file e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

# This section sets column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# This merges columns between the subject and activity data and the features data
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)
```

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

```{r}
# This section of the code reads "features.txt" and extracts only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

# This code takes only the measurements for the mean and standard deviation and adds "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 
```

## 3. Uses descriptive activity names to name the activities in the data set

```{r}
# This section of code enters the name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# This section creates dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))
```

## 4. Appropriately labels the data set with descriptive variable names. 

```{r}
names(dataTable)<-gsub("std()", "StdDev", names(dataTable))
names(dataTable)<-gsub("mean()", "Mean", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
```

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```{r}
write.table(dataTable, file = "tidydata.txt", row.name=FALSE)
```

The tidy data set is a set of variables for each activity and each subject.  This means there are 10299 instaces split into 180 groups (due to the 30 subjects and 6 activities they performed).  The data table has mean variables, std dev variables, subject, activity names and activity numbers.  The first row of the data set has names for each column.