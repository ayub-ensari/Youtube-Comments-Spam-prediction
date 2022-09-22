# Install all required packages.
install.packages(c("ggplot2", "e1071", "caret", "quanteda", 
                   "irlba", "randomForest"))

rm(list = ls())
# Load up the .CSV data and explore in RStudio.
#spam.raw <- read.csv("Youtube01-Psy.csv", stringsAsFactors = FALSE)
spam.raw <- read.csv("YouTube_Spam_All.csv", stringsAsFactors = FALSE)
View(spam.raw)


# Clean up the data frame and view our handiwork.
spam.raw <- spam.raw[, 4:5]
names(spam.raw) <- c("Text", "Label")
View(spam.raw)



# Check data to see if there are missing values.
length(which(!complete.cases(spam.raw)))



# Convert our class label into a factor.
spam.raw$Label <- as.factor(spam.raw$Label)



# The first step, as always, is to explore the data.
# First, let's take a look at distibution of the class labels (i.e., ham vs. spam).
prop.table(table(spam.raw$Label))



# Next up, let's get a feel for the distribution of text lengths of the SMS 
# messages by adding a new feature for the length of each message.
spam.raw$TextLength <- nchar(spam.raw$Text)
summary(spam.raw$TextLength)



# At a minimum we need to split our data into a training set and a
# test set. In a true project we would want to use a three-way split 
# of training, validation, and test.
#
# As we know that our data has non-trivial class imbalance, we'll 
# use the mighty caret package to create a randomg train/test split 
# that ensures the correct ham/spam class label proportions (i.e., 
# we'll use caret for a random stratified split).
#install.packages("caret")
library(caret)
help(package = "caret")


# Use caret to create a 70%/30% stratified split. Set the random
# seed for reproducibility.
set.seed(32984)
indexes <- createDataPartition(spam.raw$Label, times = 1,
                               p = 0.7, list = FALSE)

train <- spam.raw[indexes,]
test <- spam.raw[-indexes,]


# Verify proportions.
prop.table(table(train$Label))
prop.table(table(test$Label))



# Text analytics requires a lot of data exploration, data pre-processing
# and data wrangling. Let's explore some examples.


# There are many packages in the R ecosystem for performing text
# analytics. One of the newer packages in quanteda. The quanteda
# package has many useful functions for quickly and easily working
# with text data.
install.packages("quanteda")
library(quanteda)
help(package = "quanteda")


# Tokenize SMS text messages.
train.tokens <- tokens(train$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

# Take a look at a specific SMS message and see how it transforms.
train.tokens[[210]]


# Lower case the tokens.
train.tokens <- tokens_tolower(train.tokens)
train.tokens[[357]]


# Use quanteda's built-in stopword list for English.
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
train.tokens[[210]]


# Perform stemming on the tokens.
train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens[[210]]


# Create our first bag-of-words model.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transform to a matrix and inspect.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
View(train.tokens.matrix[1:20, 1:100])
dim(train.tokens.matrix)


# Investigate the effects of stemming.
colnames(train.tokens.matrix)[1:50]


# Per best practices, we will leverage cross validation (CV) as
# the basis of our modeling process. Using CV we can create 
# estimates of how well our model will do in Production on new,
# unseen data. CV is powerful, but the downside is that it
# requires more processing and therefore more time.


# Setup a the feature data frame with labels.
train.tokens.df <- cbind(Label = train$Label, data.frame(train.tokens.dfm))

str(train.tokens.df)
# Often, tokenization requires some additional pre-processing
names(train.tokens.df)[c(146, 148, 235, 238)]


# Cleanup column names.
names(train.tokens.df) <- make.names(names(train.tokens.df))


# Use caret to create stratified folds for 10-fold cross validation repeated 
# 3 times (i.e., create 30 random stratified samples)
set.seed(48743)
cv.folds <- createMultiFolds(train$Label, k = 10, times = 3)

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv.folds)

# Our data frame is non-trivial in size. As such, CV runs will take 
# quite a long time to run. To cut down on total execution time, use
# the doSNOW package to allow for multi-core training in parallel.
#
# WARNING - The following code is configured to run on a workstation-
#           or server-class machine (i.e., 12 logical cores). Alter
#           code to suit your HW environment.
#
install.packages("doSNOW")
library(doSNOW)


# Time the code execution
start.time <- Sys.time()


# Create a cluster to work on 10 logical cores.
cl <- makeCluster(1, type = "SOCK")
registerDoSNOW(cl)


# As our data is non-trivial in size at this point, use a single decision
# tree alogrithm as our first model. We will graduate to using more 
# powerful algorithms later when we perform feature extraction to shrink
# the size of our data.
rpart.cv.1 <- train(Label ~ ., data = train.tokens.df, method = "rpart", 
                    trControl = cv.cntrl, tuneLength = 7)


# Processing is done, stop cluster.
stopCluster(cl)


# Total time of execution on workstation was approximately 4 minutes. 
total.time <- Sys.time() - start.time
total.time


# Check out our results.
rpart.cv.1

