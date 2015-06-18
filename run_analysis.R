library(dplyr)
library(plyr)


# 0. Read the data
########################################################################
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subj_test <- read.table("subject_test.txt")

x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subj_train <- read.table("subject_train.txt")

features <- read.table("features.txt") 

activities <- read.table("activity_labels.txt")


# 1. Merges the training and the test sets to create one data set.
########################################################################
x_data <- rbind(x_test, x_train)
y_data <- rbind(y_test, y_train)
subj_data <- rbind(subj_test, subj_train) 


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
########################################################################
mean_and_std_features <- grep("-std()|-mean()\\>", features$V2)
x_data <- x_data[, mean_and_std_features]
features_names <- features[grep("-std()|-mean()\\>", features$V2), "V2"]
names(x_data) <- features_names


# 3. Uses descriptive activity names to name the activities in the data set
########################################################################
y_data2 <- left_join(y_data, activities, by = "V1")
names(y_data2) <- c("V1", "Activity")
y_data3 <- subset(y_data2, select = -V1)


# 4. Appropriately labels the data set with descriptive variable names. 
########################################################################
names(subj_data) <- "Subject"

all_data <- cbind(subj_data, y_data3, x_data)


# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject
########################################################################
averages_data <- ddply(all_data, .(Subject, Activity), 
                     function(x) colMeans(x[, 3:68]))

write.table(averages_data, "tidy_data_avg.txt", row.name=FALSE)
