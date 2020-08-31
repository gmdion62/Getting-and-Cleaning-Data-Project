# Download and unzip the activity data if directory is absent

# check if the data directory exists already
if (file.exists("./UCI HAR Dataset") == FALSE) {
        
        # url of zipped dataset and associated files
        zip_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        
        # download the file to local drive
        download.file(zip_url, destfile = "./UCI HAR Dataset.zip")   
        
        # unzip the file, resulting in "UCI HAR Dataset" directory and files
        unzip("./UCI HAR Dataset.zip")
        
        #remove the zip file
        file.remove("./UCI HAR Dataset.zip")
}

# for both test and train data

# read in the activity names from activity_labels.txt
activity_ids_and_names_4_both <- read.table("./UCI HAR Dataset/activity_labels.txt")

# rename the columns for activity_ids_and_labels_4_both
names(activity_ids_and_names_4_both)[1] <- "activityID"
names(activity_ids_and_names_4_both)[2] <- "activityName"

# clean up the activity name data to be standard format
activity_ids_and_names_4_both[1,"activityName"] <- "walking"
activity_ids_and_names_4_both[2,"activityName"] <- "walkingUpstairs"
activity_ids_and_names_4_both[3,"activityName"] <- "walkingDownstairs"
activity_ids_and_names_4_both[4,"activityName"] <- "sitting"
activity_ids_and_names_4_both[5,"activityName"] <- "standing"
activity_ids_and_names_4_both[6,"activityName"] <- "laying"

#
#
#
#
#
# TEST DATA INPUT

# Load the subject ids corresponding to test (same length as activity raw test data)
subject_ids_4_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Rename the subject column to subjectID
names(subject_ids_4_test)[1] <- "subjectID"

# Load the activity ids corresponding to test (same length as activity raw test data)
activity_ids_4_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Create a vector for the corresponding names
activity_names_4_test <- seq(1,nrow(activity_ids_4_test))

for (row_id in 1:nrow(activity_ids_4_test))
{
        activity_id <- activity_ids_4_test[row_id,1]
        activity_name <- activity_ids_and_names_4_both[activity_id,"activityName"]
        activity_names_4_test[row_id] <- activity_name
}

# make a single table of subject ids and activity names for the activity rows
subject_id_activity_name <- cbind(subject_ids_4_test,activity_names_4_test)

# rename the activity name column
names(subject_id_activity_name)[2] <- "activityName"

# read in the feature names from features.txt
activity_feature_names <- read.table("./UCI HAR Dataset/features.txt")

# get the activity data
activity_data_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

# assign the names of columns
names(activity_data_test) <- activity_feature_names$V2

# create a column to show rows were test data
test_data_type <- rep("test",nrow(activity_data_test))

# bind the subject ids and activity names rows to the actual activity data
activity_data_test <- cbind(subject_id_activity_name,test_data_type,activity_data_test)

# rename the train_data_type column to testOrTrainTypeData
names(activity_data_test)[3] <- "testOrTrainTypeData"


#
#
#
#
#
# TRAIN DATA INPUT

# Load the subject ids corresponding to train (same length as activity raw train data)
subject_ids_4_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Rename the subject column to subjectID
names(subject_ids_4_train)[1] <- "subjectID"

# Load the activity ids corresponding to train (same length as activity raw train data)
activity_ids_4_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Create a vector for the corresponding names
activity_names_4_train <- seq(1,nrow(activity_ids_4_train))

for (row_id in 1:nrow(activity_ids_4_train))
{
        activity_id <- activity_ids_4_train[row_id,1]
        activity_name <- activity_ids_and_names_4_both[activity_id,"activityName"]
        activity_names_4_train[row_id] <- activity_name
}

# make a single table of subject ids and activity names for the activity rows
subject_id_activity_name <- cbind(subject_ids_4_train,activity_names_4_train)

# rename the activity name column
names(subject_id_activity_name)[2] <- "activityName"

# read in the feature names from features.txt
activity_feature_names <- read.table("./UCI HAR Dataset/features.txt")

# get the activity data
activity_data_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

# assign the names of columns
names(activity_data_train) <- activity_feature_names$V2

# create a column to show rows were test data
train_data_type <- rep("train",nrow(activity_data_train))

# bind the subject ids and activity names rows to the actual activity data
activity_data_train <- cbind(subject_id_activity_name,train_data_type,activity_data_train)

# rename the train_data_type column to testOrTrainTypeData
names(activity_data_train)[3] <- "testOrTrainTypeData"

# BIND THE ROWS of TEST AND TRAIN DATA AND SORT IT
all_activity_data <- rbind(activity_data_test, activity_data_train)


# pick out those columns that have mean or std dev data

# get index of column names with "mean" and "std" in them
cols_to_include <- as.numeric(grep("mean|std",names(activity_data_train)))

# reduce the data to columns of interest and key (sub.id,act.id,testORtrain)
reduced_columns_activity_data <- all_activity_data[names(all_activity_data) %in% c("subjectID","activityName","testOrTrainTypeData")]
reduced_columns_activity_data <- cbind(reduced_columns_activity_data,all_activity_data[cols_to_include])

# clean up the column names
for (i in 4:ncol(reduced_columns_activity_data))
{
        # get the name
        name_of_column <- names(reduced_columns_activity_data)[i]

        # clean up the name, lower case to Upper for embedded string
        name_of_column <- sub("mean", "Mean", name_of_column)
        name_of_column <- sub("std", "Std", name_of_column)
        name_of_column <- sub("gravity", "Gravity", name_of_column)

        # remove dash
        name_of_column <- gsub("-", "", name_of_column)

        # remove commas
        name_of_column <- gsub(",", "", name_of_column)

        # remove parentheses
        name_of_column <- gsub("\\(|\\)", "", name_of_column)

        # reassign the name
        names(reduced_columns_activity_data)[i] <- name_of_column
}

# make a new Tidy dataset with averages of each feature for each subject/activity

library(dplyr)

# loop over the 30 subjects (anonymously named 1 to 30) and their 6 activities
# and reduce the subset to averages for all the features selected

# create a final tidy dataframe that has 1 row, but same column names
final_tidy <- reduced_columns_activity_data[c(1),]

# one empty row to add
final_tidy_empty <- final_tidy

# zero out the rows for final_tidy
final_tidy <- final_tidy[-c(1),]

# looping over 30 subjects
for (subject_id in 1:30) {
        
        for (activity_id in 1:6)
        {
                # selecting the rows for walking for this subject
                next_subset_subject_activity <- filter(reduced_columns_activity_data,subjectID==subject_id&activityName==activity_ids_and_names_4_both[activity_id,"activityName"])
                
                # add a new row to final_tidy
                final_tidy <- rbind(final_tidy,final_tidy_empty)
                
                # increment the row number to update
                final_tidy_row <- 6*(subject_id-1) + activity_id
                
                # update the final tidy data with the subject/activity and testORtrain flag for subject
                final_tidy[final_tidy_row,1] <- subject_id
                final_tidy[final_tidy_row,2] <- activity_ids_and_names_4_both[activity_id,"activityName"]
                final_tidy[final_tidy_row,3] <- next_subset_subject_activity[1,"testOrTrainTypeData"]          
                
                for (col_activity in 4:ncol(next_subset_subject_activity)) 
                {
                        final_tidy[final_tidy_row,col_activity] <- mean(next_subset_subject_activity[,col_activity]) 
                }
        }
        
}

# write the tidy data out to disk
if (file.exists("./final_tidy_data.txt") == TRUE) {
        
        # remove any existing file for tidy data
        file.remove("./final_tidy_data.txt")
}

# write out tidy data
write.table(final_tidy, "./final_tidy_data.txt")

# install dataMaid package
install.packages("dataMaid")

# load the dataMaid library
library(dataMaid)

# generate a Code Book
makeCodebook(final_tidy)
