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
