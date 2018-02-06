#Data is downloaded and saved at the same directory as this R file in a folder named "UCI HAR Dataset"
filename <- "getdata_dataset.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


##Objective 1: Merge training and test set into one data set
#Note, objective 4: Appropriately labels the data set with descriptive variable names. is done throughtout the code

#Read each file under test and train subdirectories

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <-read.table("./UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt", as.is=TRUE)
activity_labels = read.table("./UCI HAR Dataset/activity_labels.txt",as.is =TRUE)


#Label columns

colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_ID"
colnames(subject_train) <- "subject_ID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activity_ID"
colnames(subject_test) <- "subject_ID"

colnames(activity_labels) <- c("activity_ID", "activity_type")

#Now, we want to merge all data into one set and asign their column names

merged_all <- rbind( cbind(subject_train,x_train,y_train), cbind(subject_test,x_test,y_test))
colnames(merged_all) <- c("subject_ID", features[,2] ,"activity_ID")

#Objective 2: Extracts only the measurements on the mean and standard deviation for each measurement.

column_names <- colnames(merged_all)
mean_and_stdev <- (grepl("activity_ID", column_names)| grepl("subject_ID", column_names) |grepl("mean",column_names)|grepl("std",column_names))

set_of_mean_and_stdev <- merged_all[,mean_and_stdev == TRUE]

#Objective 3: Uses descriptive activity names to name the activities in the data set

set_of_mean_and_stdev$activity_ID <- factor(set_of_mean_and_stdev$activity_ID, levels = activity_labels[,1], labels=activity_labels[,2])

#Objective 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
set_of_mean_and_stdev <- humanActivity %>% group_by(subject_ID, activity_ID) %>% summarise_each(funs(mean))
write.table(set_of_mean_and_stdev, "second_tidy_set.txt", row.names = FALSE)
