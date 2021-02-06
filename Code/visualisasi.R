data1 = read.csv("positif.csv")
corpus = Corpus(VectorSource(data1$text))
corpus<- tm_map(corpus, gsub, pattern="peumbuhan", replacement="tumbuh")
corpus <- tm_map(corpus, removeWords,"pei")

dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word, col = "yellow",
        main = "Most Frequent Words", ylab = "Word frequencies")

v <- as.list(findAssocs(dtm, terms = c("protokol","masyarakat","indonesia","pemerintah","masker"),
                        corlimit = c(0.15, 0.15, 0.15, 0.15, 0.15)))
v
