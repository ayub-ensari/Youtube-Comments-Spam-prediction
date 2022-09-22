install.packages("corrplot")
install.packages("ggplot2")


library(corrplot)
library(ggplot2)


rm(list = ls())
#reading file into data frame
Absent_data = read.csv ("Absenteeism_work.csv", header = TRUE, sep = ",", row.names=NULL)
Spam_data = read.csv ("YouTube_Spam_All.csv", stringsAsFactors = FALSE)

summary(Absent_data)
summary(Spam_data)
str(Absent_data)
str(Spam_data)
################ Missing Values in the data ###################
# Check data to see if there are missing values.
length(which(!complete.cases(Absent_data)))
length(which(!complete.cases(Spam_data)))

################ Data Normalization ###########################
#Min-Max normalization using build-in functions -> for Absenteeism dataset.
scaled = as.data.frame(scale(data, center = min, scale = max - min))
#Min-Max normalization using user-define function -> for Spam Daaset.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

prc_n <- as.data.frame(lapply(prc[1:20], normalize))
################ Explore individual data ######################

Absent_data$Work.load.Average.day = as.numeric(Absent_data$Work.load.Average.day)
Absent_data$Absenteeism.time.in.hours = as.factor(Absent_data$Absenteeism.time.in.hours)
str(Absent_data)
dim(Absent_data)
attributes(Absent_data)
# explore individual variable
summary(Absent_data)
var(Absent_data$Month.of.absence)
var(Absent_data$Transportation.expense)
var(Absent_data$Distance.from.Residence.to.Work)
var(Absent_data$Service.time)
var(Absent_data$Work.load.Average.day)
var(Absent_data$Weight)
var(Absent_data)

hist(Absent_data$Month.of.absence, labels = TRUE, col = rainbow(10), breaks = c(unique(Absent_data$Month.of.absence)))
hist(Absent_data$Transportation.expense, labels = TRUE)
hist(Absent_data$Distance.from.Residence.to.Work, labels = TRUE)
hist(Absent_data$Service.time, labels = TRUE)
hist(Absent_data$Age, labels = TRUE)
hist(Absent_data$Hit.target, labels = TRUE)
hist(Absent_data$Education, labels = TRUE)
hist(Absent_data$Son, labels = TRUE)
hist(Absent_data$Pet, labels = TRUE)
hist(Absent_data$Weight, labels = TRUE)
hist(Absent_data$Height, labels = TRUE)
hist(Absent_data$Body.mass.index, labels = TRUE)
hist(Absent_data$Absenteeism.time.in.hours, labels = TRUE)


plot(density(Absent_data$Month.of.absence))
plot(density(Absent_data$Transportation.expense))
plot(density(Absent_data$Distance.from.Residence.to.Work))
plot(density(Absent_data$Service.time))
plot(density(Absent_data$Work.load.Average.day))
plot(density(Absent_data$Weight))

table(Absent_data$Absenteeism.time.in.hours)
pie(table(Absent_data$Absenteeism.time.in.hours))
barplot(table(Absent_data$Absenteeism.time.in.hours))

################# Pie Chart of categorical attributes.
Absent_data$Reason.for.absence = as.factor(Absent_data$Reason.for.absence)
Absent_data$Day.of.the.week = as.factor(Absent_data$Day.of.the.week)
Absent_data$Seasons = as.factor(Absent_data$Seasons)
Absent_data$Absenteeism.time.in.hours = as.factor(Absent_data$Absenteeism.time.in.hours)

pie(table(Absent_data$Reason.for.absence), main = "Absent_data$Reason.for.absence")
pie(table(Absent_data$Day.of.the.week), main ="Absent_data$Day.of.the.week" )
pie(table(Absent_data$Seasons), main = "Absent_data$Seasons")
pie(table(Absent_data$Absenteeism.time.in.hours), main = "Absent_data$Absenteeism.time.in.hours")

################# Explore Multiple Variables ################

#distance matrix of Absent_data by correlation
corVal = cor(Absent_data[,1:20])
corVal

#boxplot
boxplot(Absent_data$Month.of.absence~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Transportation.expense~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Distance.from.Residence.to.Work~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Service.time~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Age~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Work.load.Average.day~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Hit.target~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Weight~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Height~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Body.mass.index~Absent_data$Absenteeism.time.in.hours, data= Absent_data)
boxplot(Absent_data$Education~Absent_data$Absenteeism.time.in.hours, data= Absent_data)

pairs(Absent_data[1:10])
#distribution analysis

ggplot(Absent_data, aes(Absent_data$Distance.from.Residence.to.Work)) +
  geom_histogram()
