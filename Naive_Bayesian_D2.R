# Install all required packages.
install.packages(c("ggplot2", "e1071", "caret", "quanteda", 
                   "irlba", "randomForest"))

rm(list = ls())
# Load up the .CSV data and explore in RStudio.
sms_spam_df <- read.csv("Youtube01-Psy.csv", stringsAsFactors = FALSE)
sms_spam_df <- read.csv("YouTube_Spam_All.csv", stringsAsFactors = FALSE)
View(sms_spam_df)


# Clean up the data frame and view our handiwork.
sms_spam_df <- sms_spam_df[, 4:5]
names(sms_spam_df) <- c("Text", "Label")
View(sms_spam_df)

# Check data to see if there are missing values.
length(which(!complete.cases(sms_spam_df)))

# Convert our class label into a factor.
sms_spam_df$Label <- as.factor(sms_spam_df$Label)

# The first step, as always, is to explore the data.
# First, let's take a look at distibution of the class labels (i.e., ham vs. spam).
prop.table(table(sms_spam_df$Label))


# Next up, let's get a feel for the distribution of text lengths of the SMS 
# messages by adding a new feature for the length of each message.
sms_spam_df$TextLength <- nchar(sms_spam_df$Text)
summary(sms_spam_df$TextLength)


install.packages("tm")
library(tm)

sms_corpus = Corpus(VectorSource(sms_spam_df$Text))
print(sms_corpus)
inspect(sms_corpus[1:3])


#translate all letters to lower case
clean_corpus <- tm_map(sms_corpus, tolower)
# remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
# remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
#remove stop words
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
#remove white spaces
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

inspect(clean_corpus[1:3])

sms_dtm <- DocumentTermMatrix(clean_corpus)
inspect(sms_dtm[1:4, 30:35])

spam_indices <- which(sms_spam_df$Label == 1)
spam_indices[1:3]
ham_indices <- which(sms_spam_df$Label == 0)
ham_indices[1:3]

install.packages("wordcloud")
library(wordcloud)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_AllData_spam.png", width=600,height=480)
wordcloud(clean_corpus[ham_indices], scale = c(5,0.1), min.freq=2,  random.order = FALSE,rot.per=.55, colors = pal2)
wordcloud(clean_corpus[spam_indices], scale = c(5,0.1), min.freq=2,  random.order = FALSE,rot.per=.55, colors = pal2)
dev.off()

#splitting testing and training data =======================
samplesize = 0.70 * nrow(sms_spam_df)
set.seed(80)
index = sample( seq_len ( nrow ( sms_spam_df ) ), size = samplesize )

sms_raw_train <- sms_spam_df[index,]
sms_raw_test <- sms_spam_df[-index,]

sms_dtm_train <- sms_dtm[index,]
sms_dtm_test <- sms_dtm[-index,]
sms_corpus_train <- clean_corpus[index]
sms_corpus_test <- clean_corpus[-index]

spam <- subset(sms_raw_train, Label == 1)
ham <- subset(sms_raw_train, Label == 0)
five_times_words <- findFreqTerms(sms_dtm_train, 2)
length(five_times_words)

sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))

sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

sms_train <- apply(sms_train, 2, convert_count)
sms_test <- apply(sms_test, 2, convert_count)

install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, factor(sms_raw_train$Label))
class(sms_classifier)

sms_test_pred <- predict(sms_classifier, newdata=sms_test)

table(sms_test_pred, sms_raw_test$Label)

RMSE.NN = (sum(( as.numeric(sms_raw_test$Label) - as.numeric(sms_test_pred))^2) / nrow(sms_raw_test)) ^ 0.5
RMSE.NN
