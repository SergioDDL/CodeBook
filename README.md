CodeBook
========
## Create one R script called run_analysis.R that does the following: 
 ## 1. Merges the training and the test sets to create one data set. 
 ## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
 ## 3. Uses descriptive activity names to name the activities in the data set 
 ## 4. Appropriately labels the data set with descriptive activity names. 
 ## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 




#Step 1: "Merges the training and the test sets to create one data set." 
 # read subject training data 
 subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject_id")) 
# this will assign row number as the values of ID column 
 subject_train <- as.numeric(rownames(subject_train)) 
 # this willread training data 
 X_train = read.table("UCI HAR Dataset/train/X_train.txt") 
 # this will assign row number as the values of ID column as required by the assignment
 X_train11 <- as.numeric(rownames(X_train)) 
 # this will read the activity training data 
 y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))  # max = 6 
 #y_train =  will merge(y_train, activity_labels) 
 # assign row number as the values of ID column which is discribed here having the numeric digits at the end.
 y_train22 <- as.numeric(rownames(y_train)) 
 # merge subject_train and y_train to train 
 train <- merge(subject_train, y_train, all=TRUE) 
 # merge train and X_train 
 train <- merge(train, X_train, all=TRUE) 
 

 
 
 
 
 # this portion will read subject training data 
 subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id")) 
 # here now it will assign row number as the values of ID column 
 subject_test <- as.numeric(rownames(subject_test)) 
 #  now it willread testing data 
 X_test = read.table("UCI HAR Dataset/test/X_test.txt") 
 # again here it will assign row number as the values of ID column 
 X_test11 <- as.numeric(rownames(X_test)) 
 # it will serve to read activity testing data 
 y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id"))  # max = 6 
 #the y_test = merge(y_test, activity_labels) 
 # here it will assign row number as the values of ID column 
 y_test22 <- as.numeric(rownames(y_test)) 
 #  now merge subject_test and y_test to train 
 test <- merge(subject_test, y_test, all=TRUE)  
 # now merge test and X_test 
 test <- merge(test, X_test, all=TRUE)  
   
 #now it will combine train and test 
 data1 <- rbind(train, test) 
 
 
 
 

 #Step 2: "This portion here extracts only the measurements on the mean and standard deviation for each measurement." 
 features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)  
 #as stated this Extracts only the measurements on the mean and standard deviation for each measurement.  
 #this should work accordingly
 selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ] 
 data2 <- data1[, c(c(1, 2, 3), selected_features$feature_id + 3) ] 
 

 
 
 
 #Step 3: "This portion Uses descriptive activity names to name the activities in the data set and i am hoping it works." 
 activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),) # 
 data3 = merge(data2, activity_labels) 
 

 
 
 
 #Step 4: "As asked this portion Appropriately labels the data set with descriptive activity names."  
 selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label) 
 selected_features$feature_label = gsub("-", ".", selected_features$feature_label) 
 for (i in 1:length(selected_features$feature_label)) { 
     colnames(data3)[i + 3] <- selected_features$feature_label[i] 
   } 
 data4 = data3 
 
 
 
 

 #Step 5: "lastly this portion Creates a second, independent tidy data set with the average of each variable for each activity and each subject." 
 dropings <- c("ID","activity_label") 
 data5 <- data4[,!(names(data4) %in% dropings)] 
 aggrrdata <-aggregate(data5, by=list(subject = data5$subject_id, activity = data5$activity_id), FUN=mean, na.rm=TRUE) 
 dropings <- c("subject","activity") 
 aggrrdata <- aggdata[,!(names(aggdata) %in% dropings)] 
 aggrrdata = merge(aggdata, activity_labels) 
 write.csv(file="submit.csv", x=aggdata) 
