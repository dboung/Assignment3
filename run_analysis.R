
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

library(dplyr)
#read data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Step 1 - Merge Dataset of train and test into one and named it new_df
dfX <- rbind(x_train, x_test)
dfY <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
new_df <- cbind(subject, dfY, dfX)

#Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
new_tidy_df <-new_df %>%
  select(subject,code,contains("mean"),contains("std"))

#Step 3 - Uses descriptive activity names to name the activities in the data set.
new_tidy_df$code <- activities[new_tidy_df$code,]

#Step 4 - Appropriately labels the data set with descriptive variable names.
names(new_tidy_df)[2] = "activity"
names(new_tidy_df)<-gsub("Acc", "Accelerometer", names(new_tidy_df))
names(new_tidy_df)<-gsub("Gyro", "Gyroscope", names(new_tidy_df))
names(new_tidy_df)<-gsub("BodyBody", "Body", names(new_tidy_df))
names(new_tidy_df)<-gsub("Mag", "Magnitude", names(new_tidy_df))
names(new_tidy_df)<-gsub("^t", "Time", names(new_tidy_df))
names(new_tidy_df)<-gsub("^f", "Frequency", names(new_tidy_df))
names(new_tidy_df)<-gsub("tBody", "TimeBody", names(new_tidy_df))
names(new_tidy_df)<-gsub("-mean()", "Mean", names(new_tidy_df), ignore.case = TRUE)
names(new_tidy_df)<-gsub("-std()", "STD", names(new_tidy_df), ignore.case = TRUE)
names(new_tidy_df)<-gsub("-freq()", "Frequency", names(new_tidy_df), ignore.case = TRUE)
names(new_tidy_df)<-gsub("angle", "Angle", names(new_tidy_df))
names(new_tidy_df)<-gsub("gravity", "Gravity", names(new_tidy_df))

#Step 5 -  From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.

second_tidy_df <- new_tidy_df%>%
  group_by(subject, activity$code,activity$activity) %>%
  summarise_all(funs(mean))

second_tidy_df<- second_tidy_df[,-4]
names(second_tidy_df)[2] <- "code"
names(second_tidy_df)[3] <- "actvity"

write.table(second_tidy_df, "Data.txt", row.name=FALSE)
write.csv(second_tidy_df, "Data.csv")
