
#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed 
	#to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
	#This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#preparing for downloads, load libraries
library(httr)
library(plyr)
# set working directory
setwd("C:/Users/stuartspern/Documents/Downloads/Courses/Data Science/R_working_directory/Course3_Week4")

# check for existence of destination folder "data" , create if necessary
if (!file.exists("data")) {
    dir.create("data")
  }
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "./data/dataset.zip"
#download data if it doesn't already exist
if(!file.exists(zipfile)){
	download.file(zipurl, zipfile, method="auto")
}

#unzip and create data and results folders if they don't already exist
datafolder <- "./data/UCI HAR Dataset"
if (!file.exists(datafolder)) {
    dir.create(datafolder)
  }
resultsfolder <- "./data/results"
if (!file.exists(resultsfolder)) {
    dir.create(resultsfolder)
  }
#Unzip the zipped archive
unzip(zipfile, list = FALSE, overwrite = TRUE, exdir = "./data" )


#function to read txt and convert to data.frame (as opposed to a data.table) since # of records in a #table <10K 
getdataframe <- function (filename,cols = NULL){
	f <- paste(datafolder,filename,sep="/")
	df <- data.frame()
	if(is.null(cols)){
		df <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		df <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
	}
	df
}

#obtain list of possible column names from $V2 column of "features.txt" dataset
features <- getdataframe("features.txt")

#read data and build overall dataframe
getmergeddataframe <- function(type, features){
	subject_data <- getdataframe(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y_data <- getdataframe(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x_data <- getdataframe(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject_data,y_data,x_data))
}

#obtain overall test and train dataframes
test <- getmergeddataframe("test", features)
train <- getmergeddataframe("train", features)

#function to save provided data in the indicated folder
saveresults <- function (data,name){
	file <- paste(resultsfolder, "/", name,".csv" ,sep="")
	write.csv(data,file)
}

### required activities ###

#1) Append the training and the test sets to create one data set.

Mergeddata <- rbind(train, test)
Mergeddata <- arrange(Mergeddata, id) # order by subject id

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- Mergeddata[,c(1,2,grep("std", colnames(Mergeddata)), grep("mean", colnames(Mergeddata)))]
saveresults(mean_and_std,"mean_and_std_dataset")

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- getdataframe("activity_labels.txt") # $V2 column lists the 6 different activities

#4) Appropriately labels the data set with descriptive variable names. 
mean_and_std$activity <- factor(mean_and_std$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")