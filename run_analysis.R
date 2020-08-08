library(dplyr)

trainingSubjects <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/train/subject_train.txt")
trainingValues <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/train/X_train.txt")
trainingActivity <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/train/y_train.txt")

# read test data
testSubjects <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("F:/steny/software/R/sample data/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/test/y_test.txt")

# read features, don't convert text labels to factors
features <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/features.txt")
## note: feature names (in features[, 2]) are not unique

# read activity labels
activities <- read.table( "F:/steny/software/R/sample data/UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")
humanActivity <- rbind(
     cbind(trainingSubjects, trainingValues, trainingActivity),
     cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols


humanActivityMeans <- humanActivity %>% 
     group_by(subject, activity) %>%
     summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)