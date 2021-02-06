####KNN
cf = read.csv("M:/MCC/Fajar/Coding/enek/data_label.csv")
cf$klasifikasi = factor(cf$klasifikasi)
corpus2 = Corpus(VectorSource(cf$text))
dtm = DocumentTermMatrix(corpus2, control = list(weighting=weightTfIdf))

cf.train = cf[1001:10000,]
cf.test = cf[1:1000,]

dtm.train = dtm[1001:10000,]
dtm.test = dtm[1:1000,]

data_train = as.data.frame(as.matrix(dtm.train))
data_test = as.data.frame(as.matrix(dtm.test))

data_train1 = cbind(klasifikasi = factor(cf.train$klasifikasi), data_train)
data_test1 = cbind(klasifikasi = factor(cf.test$klasifikasi), data_test)

##mencari K
i=1
k.optm=1
for (i in 1:10){
  knn.mod <- knn(train = dtm.train, test = dtm.test, cl = cf.train$klasifikasi, k=i)
  k.optm[i] <- 100 * sum(cf.test$klasifikasi == knn.mod)/NROW(cf.test)
  k=i
  cat(k,'=',k.optm[i],'
      ')
}

model = kknn(klasifikasi~., data_train1, data_test1, k=20, kernel = "optimal")
perform = confusionMatrix(model$fitted.values, data_test1$klasifikasi)
perform

xval = train.kknn(klasifikasi~., data_train1, kmax = 20, kernel = c("optimal", "rectangular", "inv", "gaussian", "triangular"), scale = T)
xval

model = knn(dtm.train, dtm.test, cf.train$klasifikasi, k=3)
confusionMatrix = confusionMatrix(model, cf.test$klasifikasi)
confusionMatrix