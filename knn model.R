#############################
#     PROSTATE CANCER       #  
#############################

# LOAD REQUISITE LIBRARIES
library(dplyr)
library(class)
library(gmodels)

# FETCH DAT AFILES
setwd("C:/Users/AKASH/Documents/DS/Prostate Cancer")
dat <- read.csv("Prostate_Cancer.csv")

# DATA GLANCE
str(dat)
db <- dat[-1]
table(db$diagnosis_result)

# NORMALIZING THE DATA FUNCTION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

db_n <- as.data.frame(lapply(db[2:9], normalize))
summary(db_n$radius)

# SPLIT DATA IN TRAIN 65% AND TEST 35% 
db_train <- db_n[1:65,]
db_test <- db_n[66:100,]


# SEPERATE OUT THE LABELS COLUMN
db_train_labels <- db[1:65, 1]
db_test_labels <- db[66:100, 1]


# RPREPARE THE k-NN MODEL WITH k = 5
db_test_pred <- knn(train = db_train, test = db_test, cl = db_train_labels, k=8)


# CHECK THE ACCURACY WITH CONFUSION MATRIX
CrossTable(x = db_test_labels, y = db_test_pred, prop.chisq = FALSE)
