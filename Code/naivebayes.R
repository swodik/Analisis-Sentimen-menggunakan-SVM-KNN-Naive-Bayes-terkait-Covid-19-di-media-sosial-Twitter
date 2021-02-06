##Naive Bayes
#partition
df = read.csv("M:/MCC/Fajar/Coding/enek/data_label.csv")
df$klasifikasi = factor(df$klasifikasi)
corpus.clean = df[,4]
corpus.clean = Corpus(VectorSource(corpus.clean))
dtm = DocumentTermMatrix(corpus.clean, control = list(weighting=weightTfIdf))

df.train = df[1001:10000,]
df.test = df[1:1000,]

dtm.train = dtm[1001:10000,]
dtm.test = dtm[1:1000,]

corpus.clean.train = corpus.clean[1001:10000]
corpus.clean.test = corpus.clean[1:1000]

#Featured Selection
fivefreq <- findFreqTerms(dtm.train,5)
length((fivefreq))
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train,
                                   control=list(dictionary = fivefreq))
dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test,
                                  control=list(dictionary = fivefreq))
dim(dtm.test.nb)

#Boolan Naive Bayes
convert_countNB <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#Naive Bayes Model
trainNB <- apply(dtm.train.nb, 2, convert_countNB)
testNB <- apply(dtm.test.nb, 2, convert_countNB)

#Training
classifier <- naiveBayes(trainNB, df.train$klasifikasi, laplace = 1)

#Use the NB classifier we built to make predictions on the test set
pred <- predict(classifier, testNB)

#Create a truth table by tabulating the predicted class labels with the actual predicted class labels with the actual class labels
NB_table=table("Prediction"= pred, "Actual" = df.test$klasifikasi)
NB_table

#confussion Matrix
conf.matNB <- confusionMatrix(pred, df.test$klasifikasi)
conf.matNB