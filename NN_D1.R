install.packages("neuralnet")
install.packages("boot")
install.packages("plyr")
install.packages("matrixStats")

# Load libraries
library(boot)
library(plyr)
library(neuralnet)
library(matrixStats)


rm(list = ls())
# Read the Data
data = read.csv("Absenteeism_work.csv", header=T)
data$Work.load.Average.day = as.numeric(data$Work.load.Average.day)
str(data)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]


## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
max
min
scaled = as.data.frame(scale(data, center = min, scale = max - min))


## Fit neural network 

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
myCol = colnames(data)
predictVar = myCol[!myCol%in%"Absenteeism.time.in.hours"]
predictVar = paste(predictVar, collapse = "+")
Myformula = as.formula(paste("Absenteeism.time.in.hours~",predictVar,collapse = "+"))

set.seed(2)
NN = neuralnet(formula = Myformula, trainNN, hidden = c(8,4) , linear.output = T )

# plot neural network
plot(NN)



## Prediction using neural network
predict_testNN = compute(NN, testNN[,c(1:20)])
predict_testNN = (predict_testNN$net.result * (max(data$Absenteeism.time.in.hours) - min(data$Absenteeism.time.in.hours))) + min(data$Absenteeism.time.in.hours)

plot(datatest$Absenteeism.time.in.hours, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real Absent time in hours")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$Absenteeism.time.in.hours - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN

Accuracy = 1-RMSE.NN*(1/100)
Accuracy

