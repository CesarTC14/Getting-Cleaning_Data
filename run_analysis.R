## Extract all files and folders from de downloaded file
## setwd() to the 'UCI HAR Dataset' folder

run <- function() {
        library(dplyr)
        WD <- getwd()
        
        #read of generic tables
        activities <- read.table("activity_labels.txt")
        names(activities) <- c("activity","description")
        activ <- tbl_df(activities)
        features <- read.table("features.txt")
        names(features) <- c("code", "feature")
        
        #read test set variables
        setwd("test")
        testsubject <- read.table("subject_test.txt")
        names(testsubject) <- "subjectid"
        testactiv <- read.table("y_test.txt")
        names(testactiv) <- "activity"
        ttdata <- read.table("X_test.txt")
        names(ttdata) <- features$feature
        
        #select only variables that expose the mean and standard deviation of measures
        keepmean <- grep("(.*)-mean[[:punct:]](.*)", features$feature, ignore.case = TRUE)
        keepstd <- grep("(.*)-std[[:punct:]](.*)", features$feature, ignore.case = TRUE)
        keep <- c(keepmean,keepstd)
        ttdata <- ttdata[, keep]
        testdata <- data.frame(cbind(testsubject, testactiv, ttdata))
        
        #read train set variables
        setwd(WD)
        setwd("train")
        trainsubject <- read.table("subject_train.txt")
        names(trainsubject) <- "subjectid"
        trainactiv <- read.table("y_train.txt")
        names(trainactiv) <- "activity"
        trdata <- read.table("X_train.txt")
        names(trdata) <- features$feature
        
        #select only variables that expose the mean and standard deviation of measures
        trdata <- trdata[, keep]
        traindata <- data.frame(cbind(trainsubject, trainactiv, trdata))
        
        #reset WD
        setwd(WD)
        
        #merge train and test data sets
        data <- tbl_df(merge(testdata, traindata, all = TRUE))
        data2 <- left_join(data, activ, by = "activity")
        
        #organize first dataset
        ds1 <- select(data2, c(1, 69, 3:68)) %>% rename(activity = description)
        
        #organize second dataset
        ds2 <- group_by(ds1, activity, subjectid) %>% summarize_all(funs(mean))
        
        #exeport tidy dataset to the 'UCI HAR Dataset' folder
        write.table(ds2, file = "mean_measures.txt", row.names = FALSE)
}