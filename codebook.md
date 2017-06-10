THIS CODE BOOK HAS:
1. R CODES
2. ALL VARIABLE NAMES
3. VARIABLE STRUCTURE


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL R CODES
library(dplyr)
library(data.table)
library(tidyr)
# QUESTION 1 _ merge the datsets
getwd()
if(!file.exists("./data")){dir.create("./data")}
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="./data/Dataset.zip")
unzip(zipfile="./data/Dataset.zip",exdir="./data")
# I will first read the training and testing dataset of each type
# I will then convert them into dataframes for consistency
# Also, I will confirm whether their dimensions are same for merging
# subject_train/test
subject_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/subject_train.txt"))
subject_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/subject_test.txt"))
head(subject_train)
head(subject_test)
dim(subject_train)
dim(subject_test)
# subject_train and subject_test have each one variable (V1, integer type) and can be merged

# x_train/ test
data_x_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/X_train.txt"))
data_x_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/X_test.txt"))
head(data_x_train)
head(data_x_test)
dim(data_x_train)
dim(data_x_test)
# data_x_train and data_X_test both have 561 similar variables (V1, V2, etc.) and can be merged
# activity_y_train/ test
activity_y_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/y_train.txt"))
activity_y_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/y_test.txt"))
head(activity_y_train)
head(activity_y_test)
dim(activity_y_train)
dim(activity_y_test)
# activity_y_train and activity_y_test have each one variable (V1, integer type) and can be merged
# I will start the merging of the datasets.
# subject i.e. y dataset using rbind
merged_subject_rbind <- rbind(subject_train, subject_test)
dim(merged_subject_rbind)
head(merged_subject_rbind)
setnames(merged_subject_rbind, "V1", "subject")
head(merged_subject_rbind)
# activity dataset using rbind
merged_activity_rbind <- rbind(activity_y_train, activity_y_test)
dim(merged_activity_rbind)
head(merged_activity_rbind)
setnames(merged_activity_rbind, "V1", "activity_number")
head(merged_activity_rbind)
# x dataset using rbind
merged_dataset_rbind <- rbind(data_x_train, data_x_test)
dim(merged_dataset_rbind)
head(merged_dataset_rbind)
# merged _dataset_rbind has 561 variables. 
# I will first read/tidy the the other look up tables such as activity_labels.txt and features.txt
# features.txt
data_features <- tbl_df(read.table("./data/UCI HAR Dataset/features.txt"))
head(data_features)
setnames(data_features, names(data_features), c("feature_number", "feature_name"))
head(data_features)
colnames(merged_dataset_rbind) <- data_features$feature_name
head(merged_dataset_rbind)
# activity_labels.txt
activity_label <- tbl_df(read.table("./data/UCI HAR Dataset/activity_labels.txt"))
head(activity_label)
setnames(activity_label, names(activity_label), c("activity_number", "activity_name"))
head(activity_label)
# the datasets have been provides with name labels in place of V1, V2, etc.
# Each of these has 10299 rows and 1, 1, 561 variables. These can be finally merged.
# I will combine them using two instances of cbind (as there are 3 tables)
# First cbind two and then another cbind for the third
final_merged_dataset <- cbind(merged_subject_rbind, merged_activity_rbind)
head(final_merged_dataset)
final_merged_dataset <- cbind(final_merged_dataset, merged_dataset_rbind)
# The resulting dataset has columns such as subject, activity_number,  tBodyAcc-mean()-X, etc
# Question 2 - Extract measurements on Mean and SD
Q2_extract_mean_SD <- grep("mean\\(\\)|std\\(\\)", data_features$feature_name, value=TRUE)
dim(Q2_extract_mean_SD)
head(Q2_extract_mean_SD)
#Question 3 - Name label the activities
# We have activity number in the final dataset
# We have activity labels and activity numbers in activity_label
# Finally we need to bring the activity labels in the final dataset
# i.e. we will create another variable in the final dataset that has the labels
# This is achieved by a merge by activity number brought from object activity_label
final_merged_dataset <- merge(activity_label, final_merged_dataset, by = "activity_number", all.x=TRUE)
head(final_merged_dataset)
final_merged_dataset$activity_name <- as.character(final_merged_dataset$activity_name)
head(final_merged_dataset$activity_name)
# Q4 - we now need to see the structure of the final dataset
str(final_merged_dataset)
# The final data set does not spells out the variable names
# such as "Acc" in tBodyAcc-mean()-X means "accelerometer"
# The names command gives us info on all variable names
names(final_merged_dataset)
# From these names, we need to replace charecters such "Acc" by "accelerometer"
# i.e. we substitute "Acc" with "accelerometer"
names(final_merged_dataset)<-gsub("std()", "SD", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("mean()", "MEAN", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("^t", "time", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("^f", "frequency", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("Acc", "Accelerometer", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("Gyro", "Gyroscope", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("Mag", "Magnitude", names(final_merged_dataset))
names(final_merged_dataset)<-gsub("BodyBody", "Body", names(final_merged_dataset))
# look at the final dataset to confirm the changes
str(final_merged_dataset)
# Changes are confirmed (for example:  timeBodyAccelerometerJerk-min()-X)
# finally we create the tidy dataset with average for each activity/ subject
# Question 5
tidy_final_dataset <- aggregate(. ~ subject - activity_name, data = final_merged_dataset, mean)
write.table(tidy_final_dataset, "tidy_final_dataset.txt", row.name=FALSE)
codebook_finaldata <- names(tidy_final_dataset)
write.table(codebook_finaldata, "codebook_finaldata.txt", row.name=FALSE)
str(final_merged_dataset)


---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL VARIABLE NAMES
"x"
"subject"
"activity_name"
"activity_number"
"timeBodyAccelerometer-MEAN()-X"
"timeBodyAccelerometer-MEAN()-Y"
"timeBodyAccelerometer-MEAN()-Z"
"timeBodyAccelerometer-SD()-X"
"timeBodyAccelerometer-SD()-Y"
"timeBodyAccelerometer-SD()-Z"
"timeBodyAccelerometer-mad()-X"
"timeBodyAccelerometer-mad()-Y"
"timeBodyAccelerometer-mad()-Z"
"timeBodyAccelerometer-max()-X"
"timeBodyAccelerometer-max()-Y"
"timeBodyAccelerometer-max()-Z"
"timeBodyAccelerometer-min()-X"
"timeBodyAccelerometer-min()-Y"
"timeBodyAccelerometer-min()-Z"
"timeBodyAccelerometer-sma()"
"timeBodyAccelerometer-energy()-X"
"timeBodyAccelerometer-energy()-Y"
"timeBodyAccelerometer-energy()-Z"
"timeBodyAccelerometer-iqr()-X"
"timeBodyAccelerometer-iqr()-Y"
"timeBodyAccelerometer-iqr()-Z"
"timeBodyAccelerometer-entropy()-X"
"timeBodyAccelerometer-entropy()-Y"
"timeBodyAccelerometer-entropy()-Z"
"timeBodyAccelerometer-arCoeff()-X,1"
"timeBodyAccelerometer-arCoeff()-X,2"
"timeBodyAccelerometer-arCoeff()-X,3"
"timeBodyAccelerometer-arCoeff()-X,4"
"timeBodyAccelerometer-arCoeff()-Y,1"
"timeBodyAccelerometer-arCoeff()-Y,2"
"timeBodyAccelerometer-arCoeff()-Y,3"
"timeBodyAccelerometer-arCoeff()-Y,4"
"timeBodyAccelerometer-arCoeff()-Z,1"
"timeBodyAccelerometer-arCoeff()-Z,2"
"timeBodyAccelerometer-arCoeff()-Z,3"
"timeBodyAccelerometer-arCoeff()-Z,4"
"timeBodyAccelerometer-correlation()-X,Y"
"timeBodyAccelerometer-correlation()-X,Z"
"timeBodyAccelerometer-correlation()-Y,Z"
"timeGravityAccelerometer-MEAN()-X"
"timeGravityAccelerometer-MEAN()-Y"
"timeGravityAccelerometer-MEAN()-Z"
"timeGravityAccelerometer-SD()-X"
"timeGravityAccelerometer-SD()-Y"
"timeGravityAccelerometer-SD()-Z"
"timeGravityAccelerometer-mad()-X"
"timeGravityAccelerometer-mad()-Y"
"timeGravityAccelerometer-mad()-Z"
"timeGravityAccelerometer-max()-X"
"timeGravityAccelerometer-max()-Y"
"timeGravityAccelerometer-max()-Z"
"timeGravityAccelerometer-min()-X"
"timeGravityAccelerometer-min()-Y"
"timeGravityAccelerometer-min()-Z"
"timeGravityAccelerometer-sma()"
"timeGravityAccelerometer-energy()-X"
"timeGravityAccelerometer-energy()-Y"
"timeGravityAccelerometer-energy()-Z"
"timeGravityAccelerometer-iqr()-X"
"timeGravityAccelerometer-iqr()-Y"
"timeGravityAccelerometer-iqr()-Z"
"timeGravityAccelerometer-entropy()-X"
"timeGravityAccelerometer-entropy()-Y"
"timeGravityAccelerometer-entropy()-Z"
"timeGravityAccelerometer-arCoeff()-X,1"
"timeGravityAccelerometer-arCoeff()-X,2"
"timeGravityAccelerometer-arCoeff()-X,3"
"timeGravityAccelerometer-arCoeff()-X,4"
"timeGravityAccelerometer-arCoeff()-Y,1"
"timeGravityAccelerometer-arCoeff()-Y,2"
"timeGravityAccelerometer-arCoeff()-Y,3"
"timeGravityAccelerometer-arCoeff()-Y,4"
"timeGravityAccelerometer-arCoeff()-Z,1"
"timeGravityAccelerometer-arCoeff()-Z,2"
"timeGravityAccelerometer-arCoeff()-Z,3"
"timeGravityAccelerometer-arCoeff()-Z,4"
"timeGravityAccelerometer-correlation()-X,Y"
"timeGravityAccelerometer-correlation()-X,Z"
"timeGravityAccelerometer-correlation()-Y,Z"
"timeBodyAccelerometerJerk-MEAN()-X"
"timeBodyAccelerometerJerk-MEAN()-Y"
"timeBodyAccelerometerJerk-MEAN()-Z"
"timeBodyAccelerometerJerk-SD()-X"
"timeBodyAccelerometerJerk-SD()-Y"
"timeBodyAccelerometerJerk-SD()-Z"
"timeBodyAccelerometerJerk-mad()-X"
"timeBodyAccelerometerJerk-mad()-Y"
"timeBodyAccelerometerJerk-mad()-Z"
"timeBodyAccelerometerJerk-max()-X"
"timeBodyAccelerometerJerk-max()-Y"
"timeBodyAccelerometerJerk-max()-Z"
"timeBodyAccelerometerJerk-min()-X"
"timeBodyAccelerometerJerk-min()-Y"
"timeBodyAccelerometerJerk-min()-Z"
"timeBodyAccelerometerJerk-sma()"
"timeBodyAccelerometerJerk-energy()-X"
"timeBodyAccelerometerJerk-energy()-Y"
"timeBodyAccelerometerJerk-energy()-Z"
"timeBodyAccelerometerJerk-iqr()-X"
"timeBodyAccelerometerJerk-iqr()-Y"
"timeBodyAccelerometerJerk-iqr()-Z"
"timeBodyAccelerometerJerk-entropy()-X"
"timeBodyAccelerometerJerk-entropy()-Y"
"timeBodyAccelerometerJerk-entropy()-Z"
"timeBodyAccelerometerJerk-arCoeff()-X,1"
"timeBodyAccelerometerJerk-arCoeff()-X,2"
"timeBodyAccelerometerJerk-arCoeff()-X,3"
"timeBodyAccelerometerJerk-arCoeff()-X,4"
"timeBodyAccelerometerJerk-arCoeff()-Y,1"
"timeBodyAccelerometerJerk-arCoeff()-Y,2"
"timeBodyAccelerometerJerk-arCoeff()-Y,3"
"timeBodyAccelerometerJerk-arCoeff()-Y,4"
"timeBodyAccelerometerJerk-arCoeff()-Z,1"
"timeBodyAccelerometerJerk-arCoeff()-Z,2"
"timeBodyAccelerometerJerk-arCoeff()-Z,3"
"timeBodyAccelerometerJerk-arCoeff()-Z,4"
"timeBodyAccelerometerJerk-correlation()-X,Y"
"timeBodyAccelerometerJerk-correlation()-X,Z"
"timeBodyAccelerometerJerk-correlation()-Y,Z"
"timeBodyGyroscope-MEAN()-X"
"timeBodyGyroscope-MEAN()-Y"
"timeBodyGyroscope-MEAN()-Z"
"timeBodyGyroscope-SD()-X"
"timeBodyGyroscope-SD()-Y"
"timeBodyGyroscope-SD()-Z"
"timeBodyGyroscope-mad()-X"
"timeBodyGyroscope-mad()-Y"
"timeBodyGyroscope-mad()-Z"
"timeBodyGyroscope-max()-X"
"timeBodyGyroscope-max()-Y"
"timeBodyGyroscope-max()-Z"
"timeBodyGyroscope-min()-X"
"timeBodyGyroscope-min()-Y"
"timeBodyGyroscope-min()-Z"
"timeBodyGyroscope-sma()"
"timeBodyGyroscope-energy()-X"
"timeBodyGyroscope-energy()-Y"
"timeBodyGyroscope-energy()-Z"
"timeBodyGyroscope-iqr()-X"
"timeBodyGyroscope-iqr()-Y"
"timeBodyGyroscope-iqr()-Z"
"timeBodyGyroscope-entropy()-X"
"timeBodyGyroscope-entropy()-Y"
"timeBodyGyroscope-entropy()-Z"
"timeBodyGyroscope-arCoeff()-X,1"
"timeBodyGyroscope-arCoeff()-X,2"
"timeBodyGyroscope-arCoeff()-X,3"
"timeBodyGyroscope-arCoeff()-X,4"
"timeBodyGyroscope-arCoeff()-Y,1"
"timeBodyGyroscope-arCoeff()-Y,2"
"timeBodyGyroscope-arCoeff()-Y,3"
"timeBodyGyroscope-arCoeff()-Y,4"
"timeBodyGyroscope-arCoeff()-Z,1"
"timeBodyGyroscope-arCoeff()-Z,2"
"timeBodyGyroscope-arCoeff()-Z,3"
"timeBodyGyroscope-arCoeff()-Z,4"
"timeBodyGyroscope-correlation()-X,Y"
"timeBodyGyroscope-correlation()-X,Z"
"timeBodyGyroscope-correlation()-Y,Z"
"timeBodyGyroscopeJerk-MEAN()-X"
"timeBodyGyroscopeJerk-MEAN()-Y"
"timeBodyGyroscopeJerk-MEAN()-Z"
"timeBodyGyroscopeJerk-SD()-X"
"timeBodyGyroscopeJerk-SD()-Y"
"timeBodyGyroscopeJerk-SD()-Z"
"timeBodyGyroscopeJerk-mad()-X"
"timeBodyGyroscopeJerk-mad()-Y"
"timeBodyGyroscopeJerk-mad()-Z"
"timeBodyGyroscopeJerk-max()-X"
"timeBodyGyroscopeJerk-max()-Y"
"timeBodyGyroscopeJerk-max()-Z"
"timeBodyGyroscopeJerk-min()-X"
"timeBodyGyroscopeJerk-min()-Y"
"timeBodyGyroscopeJerk-min()-Z"
"timeBodyGyroscopeJerk-sma()"
"timeBodyGyroscopeJerk-energy()-X"
"timeBodyGyroscopeJerk-energy()-Y"
"timeBodyGyroscopeJerk-energy()-Z"
"timeBodyGyroscopeJerk-iqr()-X"
"timeBodyGyroscopeJerk-iqr()-Y"
"timeBodyGyroscopeJerk-iqr()-Z"
"timeBodyGyroscopeJerk-entropy()-X"
"timeBodyGyroscopeJerk-entropy()-Y"
"timeBodyGyroscopeJerk-entropy()-Z"
"timeBodyGyroscopeJerk-arCoeff()-X,1"
"timeBodyGyroscopeJerk-arCoeff()-X,2"
"timeBodyGyroscopeJerk-arCoeff()-X,3"
"timeBodyGyroscopeJerk-arCoeff()-X,4"
"timeBodyGyroscopeJerk-arCoeff()-Y,1"
"timeBodyGyroscopeJerk-arCoeff()-Y,2"
"timeBodyGyroscopeJerk-arCoeff()-Y,3"
"timeBodyGyroscopeJerk-arCoeff()-Y,4"
"timeBodyGyroscopeJerk-arCoeff()-Z,1"
"timeBodyGyroscopeJerk-arCoeff()-Z,2"
"timeBodyGyroscopeJerk-arCoeff()-Z,3"
"timeBodyGyroscopeJerk-arCoeff()-Z,4"
"timeBodyGyroscopeJerk-correlation()-X,Y"
"timeBodyGyroscopeJerk-correlation()-X,Z"
"timeBodyGyroscopeJerk-correlation()-Y,Z"
"timeBodyAccelerometerMagnitude-MEAN()"
"timeBodyAccelerometerMagnitude-SD()"
"timeBodyAccelerometerMagnitude-mad()"
"timeBodyAccelerometerMagnitude-max()"
"timeBodyAccelerometerMagnitude-min()"
"timeBodyAccelerometerMagnitude-sma()"
"timeBodyAccelerometerMagnitude-energy()"
"timeBodyAccelerometerMagnitude-iqr()"
"timeBodyAccelerometerMagnitude-entropy()"
"timeBodyAccelerometerMagnitude-arCoeff()1"
"timeBodyAccelerometerMagnitude-arCoeff()2"
"timeBodyAccelerometerMagnitude-arCoeff()3"
"timeBodyAccelerometerMagnitude-arCoeff()4"
"timeGravityAccelerometerMagnitude-MEAN()"
"timeGravityAccelerometerMagnitude-SD()"
"timeGravityAccelerometerMagnitude-mad()"
"timeGravityAccelerometerMagnitude-max()"
"timeGravityAccelerometerMagnitude-min()"
"timeGravityAccelerometerMagnitude-sma()"
"timeGravityAccelerometerMagnitude-energy()"
"timeGravityAccelerometerMagnitude-iqr()"
"timeGravityAccelerometerMagnitude-entropy()"
"timeGravityAccelerometerMagnitude-arCoeff()1"
"timeGravityAccelerometerMagnitude-arCoeff()2"
"timeGravityAccelerometerMagnitude-arCoeff()3"
"timeGravityAccelerometerMagnitude-arCoeff()4"
"timeBodyAccelerometerJerkMagnitude-MEAN()"
"timeBodyAccelerometerJerkMagnitude-SD()"
"timeBodyAccelerometerJerkMagnitude-mad()"
"timeBodyAccelerometerJerkMagnitude-max()"
"timeBodyAccelerometerJerkMagnitude-min()"
"timeBodyAccelerometerJerkMagnitude-sma()"
"timeBodyAccelerometerJerkMagnitude-energy()"
"timeBodyAccelerometerJerkMagnitude-iqr()"
"timeBodyAccelerometerJerkMagnitude-entropy()"
"timeBodyAccelerometerJerkMagnitude-arCoeff()1"
"timeBodyAccelerometerJerkMagnitude-arCoeff()2"
"timeBodyAccelerometerJerkMagnitude-arCoeff()3"
"timeBodyAccelerometerJerkMagnitude-arCoeff()4"
"timeBodyGyroscopeMagnitude-MEAN()"
"timeBodyGyroscopeMagnitude-SD()"
"timeBodyGyroscopeMagnitude-mad()"
"timeBodyGyroscopeMagnitude-max()"
"timeBodyGyroscopeMagnitude-min()"
"timeBodyGyroscopeMagnitude-sma()"
"timeBodyGyroscopeMagnitude-energy()"
"timeBodyGyroscopeMagnitude-iqr()"
"timeBodyGyroscopeMagnitude-entropy()"
"timeBodyGyroscopeMagnitude-arCoeff()1"
"timeBodyGyroscopeMagnitude-arCoeff()2"
"timeBodyGyroscopeMagnitude-arCoeff()3"
"timeBodyGyroscopeMagnitude-arCoeff()4"
"timeBodyGyroscopeJerkMagnitude-MEAN()"
"timeBodyGyroscopeJerkMagnitude-SD()"
"timeBodyGyroscopeJerkMagnitude-mad()"
"timeBodyGyroscopeJerkMagnitude-max()"
"timeBodyGyroscopeJerkMagnitude-min()"
"timeBodyGyroscopeJerkMagnitude-sma()"
"timeBodyGyroscopeJerkMagnitude-energy()"
"timeBodyGyroscopeJerkMagnitude-iqr()"
"timeBodyGyroscopeJerkMagnitude-entropy()"
"timeBodyGyroscopeJerkMagnitude-arCoeff()1"
"timeBodyGyroscopeJerkMagnitude-arCoeff()2"
"timeBodyGyroscopeJerkMagnitude-arCoeff()3"
"timeBodyGyroscopeJerkMagnitude-arCoeff()4"
"frequencyBodyAccelerometer-MEAN()-X"
"frequencyBodyAccelerometer-MEAN()-Y"
"frequencyBodyAccelerometer-MEAN()-Z"
"frequencyBodyAccelerometer-SD()-X"
"frequencyBodyAccelerometer-SD()-Y"
"frequencyBodyAccelerometer-SD()-Z"
"frequencyBodyAccelerometer-mad()-X"
"frequencyBodyAccelerometer-mad()-Y"
"frequencyBodyAccelerometer-mad()-Z"
"frequencyBodyAccelerometer-max()-X"
"frequencyBodyAccelerometer-max()-Y"
"frequencyBodyAccelerometer-max()-Z"
"frequencyBodyAccelerometer-min()-X"
"frequencyBodyAccelerometer-min()-Y"
"frequencyBodyAccelerometer-min()-Z"
"frequencyBodyAccelerometer-sma()"
"frequencyBodyAccelerometer-energy()-X"
"frequencyBodyAccelerometer-energy()-Y"
"frequencyBodyAccelerometer-energy()-Z"
"frequencyBodyAccelerometer-iqr()-X"
"frequencyBodyAccelerometer-iqr()-Y"
"frequencyBodyAccelerometer-iqr()-Z"
"frequencyBodyAccelerometer-entropy()-X"
"frequencyBodyAccelerometer-entropy()-Y"
"frequencyBodyAccelerometer-entropy()-Z"
"frequencyBodyAccelerometer-maxInds-X"
"frequencyBodyAccelerometer-maxInds-Y"
"frequencyBodyAccelerometer-maxInds-Z"
"frequencyBodyAccelerometer-MEANFreq()-X"
"frequencyBodyAccelerometer-MEANFreq()-Y"
"frequencyBodyAccelerometer-MEANFreq()-Z"
"frequencyBodyAccelerometer-skewness()-X"
"frequencyBodyAccelerometer-kurtosis()-X"
"frequencyBodyAccelerometer-skewness()-Y"
"frequencyBodyAccelerometer-kurtosis()-Y"
"frequencyBodyAccelerometer-skewness()-Z"
"frequencyBodyAccelerometer-kurtosis()-Z"
"frequencyBodyAccelerometer-bandsEnergy()-1,8"
"frequencyBodyAccelerometer-bandsEnergy()-9,16"
"frequencyBodyAccelerometer-bandsEnergy()-17,24"
"frequencyBodyAccelerometer-bandsEnergy()-25,32"
"frequencyBodyAccelerometer-bandsEnergy()-33,40"
"frequencyBodyAccelerometer-bandsEnergy()-41,48"
"frequencyBodyAccelerometer-bandsEnergy()-49,56"
"frequencyBodyAccelerometer-bandsEnergy()-57,64"
"frequencyBodyAccelerometer-bandsEnergy()-1,16"
"frequencyBodyAccelerometer-bandsEnergy()-17,32"
"frequencyBodyAccelerometer-bandsEnergy()-33,48"
"frequencyBodyAccelerometer-bandsEnergy()-49,64"
"frequencyBodyAccelerometer-bandsEnergy()-1,24"
"frequencyBodyAccelerometer-bandsEnergy()-25,48"
"frequencyBodyAccelerometer-bandsEnergy()-1,8.1"
"frequencyBodyAccelerometer-bandsEnergy()-9,16.1"
"frequencyBodyAccelerometer-bandsEnergy()-17,24.1"
"frequencyBodyAccelerometer-bandsEnergy()-25,32.1"
"frequencyBodyAccelerometer-bandsEnergy()-33,40.1"
"frequencyBodyAccelerometer-bandsEnergy()-41,48.1"
"frequencyBodyAccelerometer-bandsEnergy()-49,56.1"
"frequencyBodyAccelerometer-bandsEnergy()-57,64.1"
"frequencyBodyAccelerometer-bandsEnergy()-1,16.1"
"frequencyBodyAccelerometer-bandsEnergy()-17,32.1"
"frequencyBodyAccelerometer-bandsEnergy()-33,48.1"
"frequencyBodyAccelerometer-bandsEnergy()-49,64.1"
"frequencyBodyAccelerometer-bandsEnergy()-1,24.1"
"frequencyBodyAccelerometer-bandsEnergy()-25,48.1"
"frequencyBodyAccelerometer-bandsEnergy()-1,8.2"
"frequencyBodyAccelerometer-bandsEnergy()-9,16.2"
"frequencyBodyAccelerometer-bandsEnergy()-17,24.2"
"frequencyBodyAccelerometer-bandsEnergy()-25,32.2"
"frequencyBodyAccelerometer-bandsEnergy()-33,40.2"
"frequencyBodyAccelerometer-bandsEnergy()-41,48.2"
"frequencyBodyAccelerometer-bandsEnergy()-49,56.2"
"frequencyBodyAccelerometer-bandsEnergy()-57,64.2"
"frequencyBodyAccelerometer-bandsEnergy()-1,16.2"
"frequencyBodyAccelerometer-bandsEnergy()-17,32.2"
"frequencyBodyAccelerometer-bandsEnergy()-33,48.2"
"frequencyBodyAccelerometer-bandsEnergy()-49,64.2"
"frequencyBodyAccelerometer-bandsEnergy()-1,24.2"
"frequencyBodyAccelerometer-bandsEnergy()-25,48.2"
"frequencyBodyAccelerometerJerk-MEAN()-X"
"frequencyBodyAccelerometerJerk-MEAN()-Y"
"frequencyBodyAccelerometerJerk-MEAN()-Z"
"frequencyBodyAccelerometerJerk-SD()-X"
"frequencyBodyAccelerometerJerk-SD()-Y"
"frequencyBodyAccelerometerJerk-SD()-Z"
"frequencyBodyAccelerometerJerk-mad()-X"
"frequencyBodyAccelerometerJerk-mad()-Y"
"frequencyBodyAccelerometerJerk-mad()-Z"
"frequencyBodyAccelerometerJerk-max()-X"
"frequencyBodyAccelerometerJerk-max()-Y"
"frequencyBodyAccelerometerJerk-max()-Z"
"frequencyBodyAccelerometerJerk-min()-X"
"frequencyBodyAccelerometerJerk-min()-Y"
"frequencyBodyAccelerometerJerk-min()-Z"
"frequencyBodyAccelerometerJerk-sma()"
"frequencyBodyAccelerometerJerk-energy()-X"
"frequencyBodyAccelerometerJerk-energy()-Y"
"frequencyBodyAccelerometerJerk-energy()-Z"
"frequencyBodyAccelerometerJerk-iqr()-X"
"frequencyBodyAccelerometerJerk-iqr()-Y"
"frequencyBodyAccelerometerJerk-iqr()-Z"
"frequencyBodyAccelerometerJerk-entropy()-X"
"frequencyBodyAccelerometerJerk-entropy()-Y"
"frequencyBodyAccelerometerJerk-entropy()-Z"
"frequencyBodyAccelerometerJerk-maxInds-X"
"frequencyBodyAccelerometerJerk-maxInds-Y"
"frequencyBodyAccelerometerJerk-maxInds-Z"
"frequencyBodyAccelerometerJerk-MEANFreq()-X"
"frequencyBodyAccelerometerJerk-MEANFreq()-Y"
"frequencyBodyAccelerometerJerk-MEANFreq()-Z"
"frequencyBodyAccelerometerJerk-skewness()-X"
"frequencyBodyAccelerometerJerk-kurtosis()-X"
"frequencyBodyAccelerometerJerk-skewness()-Y"
"frequencyBodyAccelerometerJerk-kurtosis()-Y"
"frequencyBodyAccelerometerJerk-skewness()-Z"
"frequencyBodyAccelerometerJerk-kurtosis()-Z"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,8"
"frequencyBodyAccelerometerJerk-bandsEnergy()-9,16"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,24"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,32"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,40"
"frequencyBodyAccelerometerJerk-bandsEnergy()-41,48"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,56"
"frequencyBodyAccelerometerJerk-bandsEnergy()-57,64"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,16"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,32"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,48"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,64"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,24"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,48"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,8.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-9,16.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,24.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,32.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,40.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-41,48.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,56.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-57,64.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,16.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,32.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,48.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,64.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,24.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,48.1"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,8.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-9,16.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,24.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,32.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,40.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-41,48.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,56.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-57,64.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,16.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-17,32.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-33,48.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-49,64.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-1,24.2"
"frequencyBodyAccelerometerJerk-bandsEnergy()-25,48.2"
"frequencyBodyGyroscope-MEAN()-X"
"frequencyBodyGyroscope-MEAN()-Y"
"frequencyBodyGyroscope-MEAN()-Z"
"frequencyBodyGyroscope-SD()-X"
"frequencyBodyGyroscope-SD()-Y"
"frequencyBodyGyroscope-SD()-Z"
"frequencyBodyGyroscope-mad()-X"
"frequencyBodyGyroscope-mad()-Y"
"frequencyBodyGyroscope-mad()-Z"
"frequencyBodyGyroscope-max()-X"
"frequencyBodyGyroscope-max()-Y"
"frequencyBodyGyroscope-max()-Z"
"frequencyBodyGyroscope-min()-X"
"frequencyBodyGyroscope-min()-Y"
"frequencyBodyGyroscope-min()-Z"
"frequencyBodyGyroscope-sma()"
"frequencyBodyGyroscope-energy()-X"
"frequencyBodyGyroscope-energy()-Y"
"frequencyBodyGyroscope-energy()-Z"
"frequencyBodyGyroscope-iqr()-X"
"frequencyBodyGyroscope-iqr()-Y"
"frequencyBodyGyroscope-iqr()-Z"
"frequencyBodyGyroscope-entropy()-X"
"frequencyBodyGyroscope-entropy()-Y"
"frequencyBodyGyroscope-entropy()-Z"
"frequencyBodyGyroscope-maxInds-X"
"frequencyBodyGyroscope-maxInds-Y"
"frequencyBodyGyroscope-maxInds-Z"
"frequencyBodyGyroscope-MEANFreq()-X"
"frequencyBodyGyroscope-MEANFreq()-Y"
"frequencyBodyGyroscope-MEANFreq()-Z"
"frequencyBodyGyroscope-skewness()-X"
"frequencyBodyGyroscope-kurtosis()-X"
"frequencyBodyGyroscope-skewness()-Y"
"frequencyBodyGyroscope-kurtosis()-Y"
"frequencyBodyGyroscope-skewness()-Z"
"frequencyBodyGyroscope-kurtosis()-Z"
"frequencyBodyGyroscope-bandsEnergy()-1,8"
"frequencyBodyGyroscope-bandsEnergy()-9,16"
"frequencyBodyGyroscope-bandsEnergy()-17,24"
"frequencyBodyGyroscope-bandsEnergy()-25,32"
"frequencyBodyGyroscope-bandsEnergy()-33,40"
"frequencyBodyGyroscope-bandsEnergy()-41,48"
"frequencyBodyGyroscope-bandsEnergy()-49,56"
"frequencyBodyGyroscope-bandsEnergy()-57,64"
"frequencyBodyGyroscope-bandsEnergy()-1,16"
"frequencyBodyGyroscope-bandsEnergy()-17,32"
"frequencyBodyGyroscope-bandsEnergy()-33,48"
"frequencyBodyGyroscope-bandsEnergy()-49,64"
"frequencyBodyGyroscope-bandsEnergy()-1,24"
"frequencyBodyGyroscope-bandsEnergy()-25,48"
"frequencyBodyGyroscope-bandsEnergy()-1,8.1"
"frequencyBodyGyroscope-bandsEnergy()-9,16.1"
"frequencyBodyGyroscope-bandsEnergy()-17,24.1"
"frequencyBodyGyroscope-bandsEnergy()-25,32.1"
"frequencyBodyGyroscope-bandsEnergy()-33,40.1"
"frequencyBodyGyroscope-bandsEnergy()-41,48.1"
"frequencyBodyGyroscope-bandsEnergy()-49,56.1"
"frequencyBodyGyroscope-bandsEnergy()-57,64.1"
"frequencyBodyGyroscope-bandsEnergy()-1,16.1"
"frequencyBodyGyroscope-bandsEnergy()-17,32.1"
"frequencyBodyGyroscope-bandsEnergy()-33,48.1"
"frequencyBodyGyroscope-bandsEnergy()-49,64.1"
"frequencyBodyGyroscope-bandsEnergy()-1,24.1"
"frequencyBodyGyroscope-bandsEnergy()-25,48.1"
"frequencyBodyGyroscope-bandsEnergy()-1,8.2"
"frequencyBodyGyroscope-bandsEnergy()-9,16.2"
"frequencyBodyGyroscope-bandsEnergy()-17,24.2"
"frequencyBodyGyroscope-bandsEnergy()-25,32.2"
"frequencyBodyGyroscope-bandsEnergy()-33,40.2"
"frequencyBodyGyroscope-bandsEnergy()-41,48.2"
"frequencyBodyGyroscope-bandsEnergy()-49,56.2"
"frequencyBodyGyroscope-bandsEnergy()-57,64.2"
"frequencyBodyGyroscope-bandsEnergy()-1,16.2"
"frequencyBodyGyroscope-bandsEnergy()-17,32.2"
"frequencyBodyGyroscope-bandsEnergy()-33,48.2"
"frequencyBodyGyroscope-bandsEnergy()-49,64.2"
"frequencyBodyGyroscope-bandsEnergy()-1,24.2"
"frequencyBodyGyroscope-bandsEnergy()-25,48.2"
"frequencyBodyAccelerometerMagnitude-MEAN()"
"frequencyBodyAccelerometerMagnitude-SD()"
"frequencyBodyAccelerometerMagnitude-mad()"
"frequencyBodyAccelerometerMagnitude-max()"
"frequencyBodyAccelerometerMagnitude-min()"
"frequencyBodyAccelerometerMagnitude-sma()"
"frequencyBodyAccelerometerMagnitude-energy()"
"frequencyBodyAccelerometerMagnitude-iqr()"
"frequencyBodyAccelerometerMagnitude-entropy()"
"frequencyBodyAccelerometerMagnitude-maxInds"
"frequencyBodyAccelerometerMagnitude-MEANFreq()"
"frequencyBodyAccelerometerMagnitude-skewness()"
"frequencyBodyAccelerometerMagnitude-kurtosis()"
"frequencyBodyAccelerometerJerkMagnitude-MEAN()"
"frequencyBodyAccelerometerJerkMagnitude-SD()"
"frequencyBodyAccelerometerJerkMagnitude-mad()"
"frequencyBodyAccelerometerJerkMagnitude-max()"
"frequencyBodyAccelerometerJerkMagnitude-min()"
"frequencyBodyAccelerometerJerkMagnitude-sma()"
"frequencyBodyAccelerometerJerkMagnitude-energy()"
"frequencyBodyAccelerometerJerkMagnitude-iqr()"
"frequencyBodyAccelerometerJerkMagnitude-entropy()"
"frequencyBodyAccelerometerJerkMagnitude-maxInds"
"frequencyBodyAccelerometerJerkMagnitude-MEANFreq()"
"frequencyBodyAccelerometerJerkMagnitude-skewness()"
"frequencyBodyAccelerometerJerkMagnitude-kurtosis()"
"frequencyBodyGyroscopeMagnitude-MEAN()"
"frequencyBodyGyroscopeMagnitude-SD()"
"frequencyBodyGyroscopeMagnitude-mad()"
"frequencyBodyGyroscopeMagnitude-max()"
"frequencyBodyGyroscopeMagnitude-min()"
"frequencyBodyGyroscopeMagnitude-sma()"
"frequencyBodyGyroscopeMagnitude-energy()"
"frequencyBodyGyroscopeMagnitude-iqr()"
"frequencyBodyGyroscopeMagnitude-entropy()"
"frequencyBodyGyroscopeMagnitude-maxInds"
"frequencyBodyGyroscopeMagnitude-MEANFreq()"
"frequencyBodyGyroscopeMagnitude-skewness()"
"frequencyBodyGyroscopeMagnitude-kurtosis()"
"frequencyBodyGyroscopeJerkMagnitude-MEAN()"
"frequencyBodyGyroscopeJerkMagnitude-SD()"
"frequencyBodyGyroscopeJerkMagnitude-mad()"
"frequencyBodyGyroscopeJerkMagnitude-max()"
"frequencyBodyGyroscopeJerkMagnitude-min()"
"frequencyBodyGyroscopeJerkMagnitude-sma()"
"frequencyBodyGyroscopeJerkMagnitude-energy()"
"frequencyBodyGyroscopeJerkMagnitude-iqr()"
"frequencyBodyGyroscopeJerkMagnitude-entropy()"
"frequencyBodyGyroscopeJerkMagnitude-maxInds"
"frequencyBodyGyroscopeJerkMagnitude-MEANFreq()"
"frequencyBodyGyroscopeJerkMagnitude-skewness()"
"frequencyBodyGyroscopeJerkMagnitude-kurtosis()"
"angle(tBodyAccelerometerMean,gravity)"
"angle(tBodyAccelerometerJerkMean),gravityMean)"
"angle(tBodyGyroscopeMean,gravityMean)"
"angle(tBodyGyroscopeJerkMean,gravityMean)"
"angle(X,gravityMean)"
"angle(Y,gravityMean)"
"angle(Z,gravityMean)"
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
FINAL DATASET VARIABLE INFO
'data.frame':	10299 obs. of  564 variables:
 $ activity_number                                     : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity_name                                       : chr  "WALKING" "WALKING" "WALKING" "WALKING" ...
 $ subject                                             : int  28 28 28 28 28 28 28 28 28 28 ...
 $ timeBodyAccelerometer-MEAN()-X                      : num  0.308 0.168 0.343 0.31 0.17 ...
 $ timeBodyAccelerometer-MEAN()-Y                      : num  -0.00617 -0.01862 -0.02722 -0.04363 -0.00886 ...
 $ timeBodyAccelerometer-MEAN()-Z                      : num  -0.13 -0.0547 -0.0515 -0.1233 -0.1231 ...
 $ timeBodyAccelerometer-SD()-X                        : num  -0.201 -0.248 -0.177 -0.127 -0.216 ...
 $ timeBodyAccelerometer-SD()-Y                        : num  -0.0439 0.035 0.0165 0.0864 0.0251 ...
 $ timeBodyAccelerometer-SD()-Z                        : num  -0.243 -0.268 -0.22 -0.205 -0.211 ...
 $ timeBodyAccelerometer-mad()-X                       : num  -0.23 -0.282 -0.196 -0.14 -0.254 ...
 $ timeBodyAccelerometer-mad()-Y                       : num  -0.0871 0.0222 0.0489 0.0639 -0.014 ...
 $ timeBodyAccelerometer-mad()-Z                       : num  -0.226 -0.265 -0.224 -0.221 -0.228 ...
 $ timeBodyAccelerometer-max()-X                       : num  -0.1326 -0.1326 0.0701 0.0701 -0.1614 ...
 $ timeBodyAccelerometer-max()-Y                       : num  -0.0566 -0.0566 -0.00318 -0.00318 -0.06173 ...
 $ timeBodyAccelerometer-max()-Z                       : num  -0.301 -0.301 -0.295 -0.295 -0.187 ...
 $ timeBodyAccelerometer-min()-X                       : num  0.1363 0.1363 0.1979 0.0844 0.0844 ...
 $ timeBodyAccelerometer-min()-Y                       : num  -0.0597 -0.0597 0.0729 -0.1844 -0.1844 ...
 $ timeBodyAccelerometer-min()-Z                       : num  0.382 0.382 0.346 0.315 0.315 ...
 $ timeBodyAccelerometer-sma()                         : num  -0.10136 -0.09724 -0.04022 -0.00927 -0.08958 ...
 $ timeBodyAccelerometer-energy()-X                    : num  -0.68 -0.714 -0.659 -0.618 -0.689 ...
 $ timeBodyAccelerometer-energy()-Y                    : num  -0.823 -0.793 -0.8 -0.771 -0.797 ...
 $ timeBodyAccelerometer-energy()-Z                    : num  -0.74 -0.753 -0.72 -0.714 -0.718 ...
 $ timeBodyAccelerometer-iqr()-X                       : num  -0.264 -0.317 -0.257 -0.255 -0.355 ...
 $ timeBodyAccelerometer-iqr()-Y                       : num  -0.3205 -0.1755 -0.0894 -0.085 -0.2752 ...
 $ timeBodyAccelerometer-iqr()-Z                       : num  -0.297 -0.298 -0.266 -0.393 -0.387 ...
 $ timeBodyAccelerometer-entropy()-X                   : num  0.478 0.308 0.373 0.392 0.334 ...
 $ timeBodyAccelerometer-entropy()-Y                   : num  0.332 0.302 0.228 0.189 0.245 ...
 $ timeBodyAccelerometer-entropy()-Z                   : num  0.239 0.326 0.388 0.338 0.375 ...
 $ timeBodyAccelerometer-arCoeff()-X,1                 : num  -0.45 -0.503 -0.51 -0.513 -0.362 ...
 $ timeBodyAccelerometer-arCoeff()-X,2                 : num  0.361 0.385 0.401 0.436 0.292 ...
 $ timeBodyAccelerometer-arCoeff()-X,3                 : num  -0.218 -0.197 -0.236 -0.267 -0.177 ...
 $ timeBodyAccelerometer-arCoeff()-X,4                 : num  0.243 0.218 0.255 0.222 0.221 ...
 $ timeBodyAccelerometer-arCoeff()-Y,1                 : num  -0.213 -0.168 -0.183 -0.338 -0.337 ...
 $ timeBodyAccelerometer-arCoeff()-Y,2                 : num  0.0595 0.0239 0.1138 0.3226 0.2928 ...
 $ timeBodyAccelerometer-arCoeff()-Y,3                 : num  0.5106 0.5854 0.3255 0.0302 0.1194 ...
 $ timeBodyAccelerometer-arCoeff()-Y,4                 : num  -0.42804 -0.513987 -0.22056 -0.000458 -0.141109 ...
 $ timeBodyAccelerometer-arCoeff()-Z,1                 : num  -0.0544 -0.1693 0.0128 0.0766 -0.0874 ...
 $ timeBodyAccelerometer-arCoeff()-Z,2                 : num  0.000834 0.098063 0.028932 0.047574 0.151388 ...
 $ timeBodyAccelerometer-arCoeff()-Z,3                 : num  0.184433 0.167167 0.000246 -0.057909 -0.07799 ...
 $ timeBodyAccelerometer-arCoeff()-Z,4                 : num  -0.0415 -0.1365 0.0935 0.1455 0.1038 ...
 $ timeBodyAccelerometer-correlation()-X,Y             : num  -0.0874 -0.0671 0.0444 -0.0181 -0.0976 ...
 $ timeBodyAccelerometer-correlation()-X,Z             : num  -0.069 -0.1019 -0.0505 -0.0269 -0.1111 ...
 $ timeBodyAccelerometer-correlation()-Y,Z             : num  0.324 0.262 0.308 0.515 0.523 ...
 $ timeGravityAccelerometer-MEAN()-X                   : num  0.934 0.936 0.936 0.942 0.938 ...
 $ timeGravityAccelerometer-MEAN()-Y                   : num  -0.235 -0.234 -0.233 -0.226 -0.229 ...
 $ timeGravityAccelerometer-MEAN()-Z                   : num  -0.15 -0.142 -0.128 -0.124 -0.129 ...
 $ timeGravityAccelerometer-SD()-X                     : num  -0.972 -0.974 -0.97 -0.975 -0.971 ...
 $ timeGravityAccelerometer-SD()-Y                     : num  -0.979 -0.983 -0.974 -0.975 -0.968 ...
 $ timeGravityAccelerometer-SD()-Z                     : num  -0.985 -0.939 -0.935 -0.98 -0.962 ...
 $ timeGravityAccelerometer-mad()-X                    : num  -0.972 -0.974 -0.97 -0.977 -0.972 ...
 $ timeGravityAccelerometer-mad()-Y                    : num  -0.981 -0.984 -0.979 -0.976 -0.967 ...
 $ timeGravityAccelerometer-mad()-Z                    : num  -0.986 -0.943 -0.935 -0.98 -0.963 ...
 $ timeGravityAccelerometer-max()-X                    : num  0.87 0.87 0.874 0.876 0.876 ...
 $ timeGravityAccelerometer-max()-Y                    : num  -0.249 -0.249 -0.241 -0.24 -0.24 ...
 $ timeGravityAccelerometer-max()-Z                    : num  -0.153 -0.13 -0.127 -0.127 -0.132 ...
 $ timeGravityAccelerometer-min()-X                    : num  0.945 0.946 0.946 0.951 0.95 ...
 $ timeGravityAccelerometer-min()-Y                    : num  -0.213 -0.21 -0.21 -0.205 -0.207 ...
 $ timeGravityAccelerometer-min()-Z                    : num  -0.151 -0.148 -0.144 -0.125 -0.137 ...
 $ timeGravityAccelerometer-sma()                      : num  -0.0524 -0.0732 -0.1142 -0.1328 -0.117 ...
 $ timeGravityAccelerometer-energy()-X                 : num  0.824 0.827 0.827 0.843 0.834 ...
 $ timeGravityAccelerometer-energy()-Y                 : num  -0.909 -0.91 -0.911 -0.916 -0.914 ...
 $ timeGravityAccelerometer-energy()-Z                 : num  -0.952 -0.957 -0.964 -0.967 -0.964 ...
 $ timeGravityAccelerometer-iqr()-X                    : num  -0.971 -0.977 -0.976 -0.977 -0.976 ...
 $ timeGravityAccelerometer-iqr()-Y                    : num  -0.985 -0.987 -0.987 -0.979 -0.971 ...
 $ timeGravityAccelerometer-iqr()-Z                    : num  -0.989 -0.96 -0.937 -0.98 -0.968 ...
 $ timeGravityAccelerometer-entropy()-X                : num  -0.433 -0.491 -0.464 -0.775 -0.581 ...
 $ timeGravityAccelerometer-entropy()-Y                : num  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
 $ timeGravityAccelerometer-entropy()-Z                : num  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
 $ timeGravityAccelerometer-arCoeff()-X,1              : num  -0.486 -0.569 -0.603 -0.544 -0.471 ...
 $ timeGravityAccelerometer-arCoeff()-X,2              : num  0.553 0.626 0.67 0.619 0.541 ...
 $ timeGravityAccelerometer-arCoeff()-X,3              : num  -0.617 -0.683 -0.737 -0.692 -0.608 ...
 $ timeGravityAccelerometer-arCoeff()-X,4              : num  0.678 0.737 0.803 0.764 0.673 ...
 $ timeGravityAccelerometer-arCoeff()-Y,1              : num  -0.064 0.0337 -0.0369 -0.0826 -0.0773 ...
 $ timeGravityAccelerometer-arCoeff()-Y,2              : num  0.0524 -0.0377 0.0339 0.0877 0.0782 ...
 $ timeGravityAccelerometer-arCoeff()-Y,3              : num  -0.1016 -0.0267 -0.0954 -0.1537 -0.1397 ...
 $ timeGravityAccelerometer-arCoeff()-Y,4              : num  0.173 0.116 0.181 0.242 0.223 ...
 $ timeGravityAccelerometer-arCoeff()-Z,1              : num  -0.358 -0.465 -0.455 -0.232 -0.282 ...
 $ timeGravityAccelerometer-arCoeff()-Z,2              : num  0.437 0.522 0.509 0.313 0.359 ...
 $ timeGravityAccelerometer-arCoeff()-Z,3              : num  -0.515 -0.58 -0.563 -0.391 -0.435 ...
 $ timeGravityAccelerometer-arCoeff()-Z,4              : num  0.587 0.634 0.614 0.465 0.508 ...
 $ timeGravityAccelerometer-correlation()-X,Y          : num  0.791 0.768 0.861 0.791 0.925 ...
 $ timeGravityAccelerometer-correlation()-X,Z          : num  0.3905 -0.8549 0.0986 -0.646 0.4185 ...
 $ timeGravityAccelerometer-correlation()-Y,Z          : num  -0.116 -0.731 0.152 -0.922 0.526 ...
 $ timeBodyAccelerometerJerk-MEAN()-X                  : num  -0.44 -0.0719 0.2723 -0.1959 0.2738 ...
 $ timeBodyAccelerometerJerk-MEAN()-Y                  : num  -0.2229 0.0728 -0.1213 0.0875 0.4261 ...
 $ timeBodyAccelerometerJerk-MEAN()-Z                  : num  0.0689 0.224 -0.4609 -0.2304 0.2568 ...
 $ timeBodyAccelerometerJerk-SD()-X                    : num  -0.278 -0.344 -0.297 -0.208 -0.235 ...
 $ timeBodyAccelerometerJerk-SD()-Y                    : num  -0.1593 -0.0302 -0.0856 -0.03 -0.1004 ...
 $ timeBodyAccelerometerJerk-SD()-Z                    : num  -0.506 -0.519 -0.454 -0.375 -0.415 ...
 $ timeBodyAccelerometerJerk-mad()-X                   : num  -0.218 -0.266 -0.236 -0.131 -0.17 ...
 $ timeBodyAccelerometerJerk-mad()-Y                   : num  -0.0784 0.0743 0.0301 0.069 -0.0561 ...
 $ timeBodyAccelerometerJerk-mad()-Z                   : num  -0.473 -0.51 -0.437 -0.34 -0.393 ...
 $ timeBodyAccelerometerJerk-max()-X                   : num  -0.48 -0.598 -0.549 -0.3 -0.3 ...
 $ timeBodyAccelerometerJerk-max()-Y                   : num  -0.538 -0.538 -0.638 -0.456 -0.456 ...
 $ timeBodyAccelerometerJerk-max()-Z                   : num  -0.754 -0.74 -0.74 -0.614 -0.614 ...
 $ timeBodyAccelerometerJerk-min()-X                   : num  0.171 0.46 0.394 0.394 0.375 ...
 $ timeBodyAccelerometerJerk-min()-Y                   : num  0.384 0.264 0.264 0.161 0.161 ...
 $ timeBodyAccelerometerJerk-min()-Z                   : num  0.388 0.388 0.377 0.236 0.236 ...
 $ timeBodyAccelerometerJerk-sma()                     : num  -0.227 -0.214 -0.188 -0.098 -0.164 ...
  [list output truncated]