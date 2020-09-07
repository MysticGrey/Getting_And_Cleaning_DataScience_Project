# Step1: Reading the Data
library(dplyr)
path <- file.path("UCI HAR Dataset")
X_train <- read.table(file.path(path,"train", "X_train.txt"), header = FALSE)
X_test <- read.table(file.path(path,"test", "X_test.txt"), header = FALSE)
y_train <- read.table(file.path(path,"train", "y_train.txt"), header = FALSE)
y_test <- read.table(file.path(path,"test", "y_test.txt"), header = FALSE)
subject_train <- read.table(file.path(path,"train", "subject_train.txt"), header = FALSE)
subject_test <- read.table(file.path(path,"test", "subject_test.txt"), header = FALSE)

# Step 2: Merges the training and the test sets for each dataset.
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
features <- read.table(file.path(path,"features.txt"), header = FALSE)

# Step 3: Appropriately labels the data set with descriptive variable names. 
names(X) <- features$V2
names(X) <- gsub("\\(\\)","",names(X))
names(y) <- c("activity")
names(subject) <- ("subject")

# Step 4: Merge all the data into a single Dataset.
Data <- cbind(subject,y, X)

# Step 5: Extracts only the measurements on the mean and standard deviation for each measurement.
posMerge <- grep("mea[n]$|mean-|std+",names(X), value = TRUE)
Data <- Data[,c("subject","activity",posMerge)]

# Step 6: Uses descriptive activity names to name the activities in the data set
act_label <- read.table(file.path(path,"activity_labels.txt"), header = FALSE)
Data[,2] <- sapply(Data[,2],function(x){s <- act_label[which(act_label$V1==x),2]})
write.table(Data, file = "Data_Final.txt",row.name=FALSE)

# Step 7: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr);
TidyData<-aggregate(. ~subject + activity, Data, mean)
TidyData<-TidyData[order(TidyData$subject,TidyData$activity),]
write.table(TidyData, file = "Tidy_Data.txt",row.name=FALSE) 