###SVM
cf = read.csv("M:/MCC/Fajar/Coding/enek/data_label.csv")
cf$klasifikasi = factor(cf$klasifikasi)
corpus2 = Corpus(VectorSource(cf$text))
dtm = DocumentTermMatrix(corpus2)

dtMatrix = create_matrix(cf["text"], language = "id", removeStopwords = FALSE,
                         removeNumbers = FALSE, stemWords = FALSE, tm::weightTfIdf)

#mat = as.matrix(dtMatrix)
container = create_container(dtMatrix, cf$klasifikasi, trainSize = 1001:10000, testSize = 1:1000, virgin = FALSE)
model = train_model(container, "SVM", kernel = "linear", cost = 1)
result = classify_model(container, model)
result$SVM_LABEL = factor(result$SVM_LABEL)
confussionMatrix = confusionMatrix(cf$klasifikasi[1:1000], result[,"SVM_LABEL"])
confussionMatrix