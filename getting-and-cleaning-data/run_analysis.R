##Getting-and-cleaning data

#install.packages("dplyr") #incase package is not installed

library(dplyr)
setwd("C:/Users/abirp/OneDrive/Desktop/Coursera_Projects/Course_3_getting_and_cleaning_data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
##Read the train data (X and Y)
list.files('./train')
# [1] "Inertial Signals"  "subject_train.txt" "X_train.txt"       "y_train.txt"      
subject_train_data<-read.table("./train/subject_train.txt")
train_data_X <- read.table("./train/X_train.txt")
train_data_Y <- read.table("./train/y_train.txt")

## Read the test data(X and Y)
list.files('./test')
# [1] "Inertial Signals" "subject_test.txt" "X_test.txt"       "y_test.txt"      
subject_test_data<-read.table("./test/subject_test.txt")
test_data_X <- read.table("./test/X_test.txt")
test_data_Y <- read.table("./test/y_test.txt")


## Activity Labels
activity_labels<-read.table("./activity_labels.txt")

## Reading features list 
features<-read.table("./features.txt")

# Part 1: Merge the training and the test sets to create one data set.
combined_data_X<-rbind(train_data_X,test_data_X)
combined_data_Y<-rbind(train_data_Y,test_data_Y)
combined_data_subject<-rbind(subject_train_data,subject_test_data)

## Changing the column names to the required names given in the features file
colnames(combined_data_X)<-features$V2
colnames(combined_data_Y)<-c("Predictor_Type_of_Action")
colnames(combined_data_subject)<-c("Subject")


all_data<-cbind(combined_data_subject,combined_data_X,combined_data_Y)
# Order - Subject,Data_values list,predictor variable
## Shortlisting columns based on 
## mean(): Mean value
## std(): Standard deviation
shortlist_columns<-grep("mean()|std()",colnames(all_data))

## Arranging data in an orderly way
subset_data<-all_data[,c(1,shortlist_columns,ncol(all_data))]

## Removing "-" and "()" from the column names
colnames(subset_data)<-gsub("-","_",colnames(subset_data))
colnames(subset_data)<-gsub("\\(\\)","",colnames(subset_data))


##independent tidy data set with the average of each variable for each activity and each subject.
subset_data_average <- subset_data %>%
  group_by(Subject, Predictor_Type_of_Action) %>%
  summarise_each(funs(mean))

##Write to a dataset 
write.table(subset_data_average,file = './cleaned_data.txt')
