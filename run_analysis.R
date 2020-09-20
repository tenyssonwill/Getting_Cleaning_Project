run_analysis <- function(){
  library(dplyr)
  filename <- "project_data_file.zip"
  
  # Checking and Download files (to do not download everytime)
  if (!file.exists(filename)){ 
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
  }  
  
  # Checking if folder of unziped data exists (to do not unzip everytime)
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename)
  }
  
  #Reading feature and activities names
  features_names <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
  activities_names <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
  
  #Reading subject codes feature and activities names
  train_subject_codes  <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  test_subject_codes   <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  merged_subject_codes <- rbind(train_subject_codes, test_subject_codes)
  
  #Reading subject codes feature and activities names
  train_activity_codes  <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
  test_activity_codes   <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
  merged_activity_codes <- rbind(train_activity_codes, test_activity_codes)
  
  #Reading subject codes feature and activities names
  train_features_vectors  <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features_names$functions)
  test_features_vectors   <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features_names$functions)
  merged_features_vectors <- rbind(train_features_vectors, test_features_vectors)
  
  # 1. Merges the training and the test sets to create one data set.
  merged_Data <- cbind(merged_subject_codes,merged_activity_codes, merged_features_vectors)
  
  #2. Extracts only the measurements on the mean and standard deviation for each measurement.
  tidy_Data <- merged_Data %>% select(subject, code, contains("mean"), contains("std"))
  
  #3. Uses descriptive activity names to name the activities in the data set.
  tidy_Data$code <- activities_names[tidy_Data$code, 2]
  
  #4. Appropriately labels the data set with descriptive variable names.
  names(tidy_Data)<-gsub("code", "activity", names(tidy_Data))
  names(tidy_Data)<-gsub("Acc", "Accelerometer", names(tidy_Data))
  names(tidy_Data)<-gsub("Gyro", "Gyroscope", names(tidy_Data))
  names(tidy_Data)<-gsub("BodyBody", "Body", names(tidy_Data))
  names(tidy_Data)<-gsub("Mag", "Magnitude", names(tidy_Data))
  names(tidy_Data)<-gsub("^t", "Time", names(tidy_Data))
  names(tidy_Data)<-gsub("freq", "Frequency", names(tidy_Data), ignore.case = TRUE)
  names(tidy_Data)<-gsub("^f", "Frequency", names(tidy_Data))
  names(tidy_Data)<-gsub("tBody", "TimeBody", names(tidy_Data))
  names(tidy_Data)<-gsub("mean", "Mean", names(tidy_Data), ignore.case = TRUE)
  names(tidy_Data)<-gsub("std", "StandardDeviation", names(tidy_Data), ignore.case = TRUE)
  names(tidy_Data)<-gsub("angle", "Angle", names(tidy_Data))
  names(tidy_Data)<-gsub("gravity", "Gravity", names(tidy_Data))
  names(tidy_Data)<-gsub("\\.", "", names(tidy_Data))
  
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  mean_tidy_data <- tidy_Data %>%  group_by(subject, activity) %>% summarise_all(funs(mean))
  write.table(mean_tidy_data, "mean_tidy_data.txt", row.name=FALSE)
  
  names(mean_tidy_data)
}