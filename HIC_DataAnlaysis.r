library("dplyr")
library("tidyr")

setwd("F:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset")

#xtest <- read.table("J:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
#ytest <- read.table("J:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
#xtrain <- read.table("J:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
#ytrain <- read.table("J:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
#colNames <- read.table("J:/Repos/701_TrainingDatasets/proj3/UCI HAR Dataset/features.txt", quote="\"", comment.char="")

xtest <- read.table("test/X_test.txt", quote="\"", comment.char="")
ytest <- read.table("test/y_test.txt", quote="\"", comment.char="")
xtrain <- read.table("train/X_train.txt", quote="\"", comment.char="")
ytrain <- read.table("train/y_train.txt", quote="\"", comment.char="")
colNames <- read.table("features.txt", quote="\"", comment.char="")

subjecttest <- read.table("test/subject_test.txt", quote="\"", comment.char="")
subjecttrain <- read.table("train/subject_train.txt", quote="\"", comment.char="")

names(subjecttest) <- "subject"
names(subjecttrain) <- "subject"
names(xtest) <- colNames$V2
names(xtrain) <- colNames$V2

cbind(subjecttrain, xtrain) -> xtrain
cbind(subjecttest, xtest) -> xtest

# 1) Merge Training and Data to x & y

rbind(xtest,xtrain) -> x
rbind(ytest,ytrain) -> y

# 1.1 ) Fix names, set as tbl_df

x <- tbl_df(x)

# 2)  Extract means and st devs to 'means' and 'stds'
#            retain index vectors
grep("std", names(x)) -> indxStd
grep("mean", names(x)) -> indxMean

means <- x[,indxMean]
stds <- x[,indxStd]

meansStds <- cbind(x[,1],means, stds)

# 3) pass back names of activities to y
activityLabels <- read.table("activity_labels.txt", quote="\"", comment.char="")

colYNames <- vector(mode = "character", length = dim(y)[1])
yv <- y$V1
yvC <- as.character(yv)

yvC[yvC == "1"] <- as.character(activityLabels[1,2])
yvC[yvC == "2"] <- as.character(activityLabels[2,2])
yvC[yvC == "3"] <- as.character(activityLabels[3,2])
yvC[yvC == "4"] <- as.character(activityLabels[4,2])
yvC[yvC == "5"] <- as.character(activityLabels[5,2])
yvC[yvC == "6"] <- as.character(activityLabels[6,2])

#yvC <- levels(yvC)

yd <- tbl_df(y)
#names(yvC) <- c( "activityLabel")

XY <- cbind(yvC, meansStds)
names(XY)[1] <- "ActivityLabel"
actXY <- group_by(XY, ActivityLabel, subject )

outputTable <- summarize_each(actXY, funs(mean))

write.table(outputTable, "TidyMeans.txt")
