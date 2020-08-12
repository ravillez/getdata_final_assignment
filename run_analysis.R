# GETTING AND CLEANING DATA COURSE PROJECT

library(dplyr)

# Downloading and unzipping data set.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "dataset.zip", method = "curl")
unzip("./dataset.zip")

# STEP 1(a): Tidying test data set
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
colnames(x_test) <- features[, 2] # Adding column names.

y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt", header = FALSE)
colnames(y_test) <- "activity" # Adding column name.

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
colnames(subject_test) <- "subject" # Adding column name.

test <- cbind(y_test, subject_test, x_test) # Merging test data set.

# STEP 1(b): Tidying training data set
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
colnames(x_train) <- features[, 2] # Adding column names.

y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = FALSE)
colnames(y_train) <- "activity" # Adding column name.

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
colnames(subject_train) <- "subject" # Adding column name.

training <- cbind(y_train, subject_train, x_train) # Merging training data set.

# STEP 1(c): Merging test and training data sets.
completedata <- rbind(test, training)

# STEP 2: Extracting only the measurements on the mean and the standard deviation for each measurement.
measureselect <- (grepl("activity", colnames(completedata)) | grepl("subject", colnames(completedata)) 
                  | grepl("mean", colnames(completedata)) | grepl("std", colnames(completedata)))
datasubset <- completedata[,measureselect == TRUE]
View(datasubset)

# STEP 3: Using descriptive activity names to name the activities in the data set.
datasubset <- datasubset %>% mutate(activity = recode(activity, `1` = "walking", `2` = "walking_upstairs", `3` = "walking_downstairs",
                                                `4` = "sitting", `5` = "standing", `6` = "laying"))

# STEP 4: Labeling the data set with descriptive variable names.
colnames(datasubset) <- gsub("Acc", "Accelerometer", colnames(datasubset))
colnames(datasubset) <- gsub("Gyro", "Gyroscope", colnames(datasubset))
colnames(datasubset) <- gsub("Mag", "Magnitude", colnames(datasubset))
colnames(datasubset) <- gsub("Freq", "Frequency", colnames(datasubset))
colnames(datasubset) <- gsub("BodyBody", "Body", colnames(datasubset))
colnames(datasubset) <- gsub("^f", "Frequency", colnames(datasubset))
colnames(datasubset) <- gsub("^t", "Time", colnames(datasubset))
colnames(datasubset) <- gsub("mean", "Mean", colnames(datasubset))
colnames(datasubset) <- gsub("std", "StandardDeviation", colnames(datasubset))
colnames(datasubset) <- gsub('[-()]', '', colnames(datasubset))
names(datasubset)

# STEP 5: Create a second, independent tidy data set with the average of each variable for each activity and each subject
summarydata <- datasubset %>% group_by(subject, activity) %>% summarise(across(1:79, mean))

# Create text file with tidy data set created in Step 5.
write.table(summarydata, "summarydata.txt", row.names = FALSE, quote = FALSE)