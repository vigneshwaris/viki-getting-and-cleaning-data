#A full description is available at the site where the data was obtained:
  
 # http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#The data is available at:
  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#installing the libraries
install.packages("data.table")
library(data.table)
library(dplyr)

#Reading the supporting meta data
featureNames <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
activityLabels <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", header = FALSE)
#Formatting the training datasets and testing data sets

#Reading the training data
subjectTrain <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Read test data
subjectTest <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Part 1 - Merge the training and the test sets to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#Naming the columns
colnames(features) <- t(featureNames[2])
#Merge the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement
#Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
#Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
#We create extractedData with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns.

extractedData <- completeData[,requiredColumns]
dim(extractedData)
#Part 3 - Uses descriptive activity names to name the activities in the data set
#The activity field in extractedData is originally of numeric type. We need to change its type to character so that it can accept activity names. The activity names are taken from metadata activityLabels.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
#We need to factor the activity variable, once the activity names are updated.

extractedData$Activity <- as.factor(extractedData$Activity)
#Part 4 - Appropriately labels the data set with descriptive variable names
names(extractedData)
#Editing the names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Here are the names of the variables in extractedData after they are edited
names(extractedData)


#Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
install.packages("data.table")
library(data.table)
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#Create tidy Data
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
