##Function takes data from multiple txt files and combines into a tidy data set with selected variables

run_analysis <- function(){
    
    #download zip file / unzip to working dir()
    url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url1, destfile = "CleaningDataProject.zip")
    unzip("CleaningDataProject.zip")
    
    library(dplyr)
    #create tables from *test.txt files
    X_test <- read.table("test/X_test.txt")
    y_test <- read.table("test/y_test.txt")
    subject_test <- read.table("test/subject_test.txt")
    
    #create tables from *train.txt files
    X_train <- read.table("train/X_train.txt")
    y_train <- read.table("train/y_train.txt")
    subject_train <- read.table("train/subject_train.txt") 
    
    #create tables for features and activity labels
    features <- read.table("features.txt")
    activity_labels <- read.table("activity_labels.txt")
    
    #rename cols for test data
    colnames(X_test) <- features[, 2]
    colnames(y_test) <- "activityId"
    colnames(subject_test) <- "subjectId"
    
    #rename col for training data
    colnames(X_train) <- features[, 2]
    colnames(y_train) <- "activityId"
    colnames(subject_train) <- "subjectId"
    
    #rename cols for activity ids and labels
    colnames(activity_labels) <- c("activityId", "activityLabel")
    
    #combine all testing data by a column bind
    mrgtest <- cbind(y_test, subject_test, X_test)
    #combine all training data by a column bind
    mrgtrain <- cbind(y_train, subject_train, X_train)
    #combine testing and traing data by a row bind
    combdata <- rbind(mrgtest, mrgtrain)
    
    #stipulations for columns to keep in new data.frame - use escape command(\\b\\b) for finding mean / std
    correct_cols <- (grepl("activityId", colnames(combdata)) |
                         grepl("subjectId", colnames(combdata)) |
                         grepl("\\bmean()\\b", colnames(combdata)) |
                         grepl("\\bstd()\\b", colnames(combdata)))
    
    #create new data.frame based on stipulations above
    allData <- combdata[, correct_cols == TRUE]
    
    #merge descriptive activity label into a new data frame and leave out the activity Id - reorganize column sequence
    allactivitylabels <- merge(x = allData, y = activity_labels, by = "activityId")
    allactivitylabels <- select(allactivitylabels, -activityId)
    allactivitylabels <- select(allactivitylabels, subjectId, activityLabel, 2:67)
    
    #create tidy data set to group by subjectId and the activityLabel and summarize columns remaining by the column mean
    tidySet <- allactivitylabels %>%
        group_by(subjectId, activityLabel) %>%
        summarize_all(funs(mean))

    
    #write the tidy table to a text file in the working directory
    write.table(tidySet, "tidy.txt", row.names = FALSE, col.names = TRUE, sep="\t", quote = TRUE)
}