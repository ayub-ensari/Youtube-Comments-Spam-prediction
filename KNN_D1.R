
install.packages("boot")
install.packages("plyr")
install.packages("matrixStats")
install.packages("class")


# Load libraries
library(class)
library(boot)
library(plyr)
library(neuralnet)
library(matrixStats)

prc <- read.csv("Absenteeism_work.csv", header=T)
prc$Work.load.Average.day = as.numeric(prc$Work.load.Average.day)
prc$Absenteeism.time.in.hours = as.factor(prc$Absenteeism.time.in.hours)
str(prc)
# different class in the class attribute and their values
table(prc$Absenteeism.time.in.hours)
# per cent display of each class contribution
round(prop.table(table(prc$Absenteeism.time.in.hours)) * 100, digits = 1) 

# normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

prc_n <- as.data.frame(lapply(prc[1:20], normalize))
summary(prc_n$Height)


# Random sampling
samplesize = 0.70 * nrow(prc)
set.seed(80)
index = sample( seq_len ( nrow ( prc ) ), size = samplesize )

# Create training and test set
prc_train = prc_n[ index, ]
prc_test = prc_n[ -index, ]
table(prc_train$Absenteeism.time.in.hours)
table(prc_test$Absenteeism.time.in.hours)


prc_train_labels <- prc[index, 21]
prc_test_labels <- prc[-index, 21] 


# running KNN
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=10)
# creating confusion matrix
cMatrix = as.matrix(table(prc_test_pred, prc_test_labels))
tp = sum(diag(cMatrix))
tp
#calculating accuracy
accuracy = sum(diag(cMatrix)) / length(prc_test_labels)
accuracy

###### Runing the KNN for differnt values of K and storing them. 
accuracy <- rep(0, 30)
k <- 1:30
for(x in k){
  prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=x)
  cMatrix = as.matrix(table(prc_test_pred, prc_test_labels))
  accuracy[x] <- sum(diag(cMatrix)) / length(prc_test_labels)
}
# plot the values
plot(k, accuracy, type = 'b')


