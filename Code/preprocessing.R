###Preprocessing Data
##import data
maret <- read.csv("maret.csv")
april <- read.csv("april.csv")
mei <- read.csv("mei.csv")
juni <- read.csv("juni.csv")
juli <- read.csv("juli.csv")

data <- rbind(maret[1:1000,], april[1:1000,], mei[1:1000,], juni[1:1000,], juli[1:1000,])

write.csv(data, "data_mentah.csv")
data<- data$tweet

##case folding
#case folding
data<- tolower(data)

#menghapus rt
data <- gsub("rt", "", data)

#menghapus username
data <- gsub("@\\w+", "", data)

#menghapus punctuation
data <- gsub("[[:punct:]]", "", data)

#menghapus links
data <- gsub("http\\w+", "", data)

#menghapus tabulasi
data <- gsub("[ |\t]{2,}", "", data)

#menghapus blank spaces awal
data <- gsub("^ ", "", data)

#menghapus blank spaces akhir
data <- gsub(" $", "", data)

#menghapus nomor
data<- removeNumbers(data)

#memanjangkan singkatan
singkatan <- read.csv("colloquial-indonesian-lexicon.csv")
data <- replace_internet_slang(data, slang = paste0("\\b",
                                                        singkatan$slang, "\\b"),
                                 replacement = singkatan$formal, ignore.case = TRUE)

#menghapus simbol
data <- strip(data)

#convert corpus
corpus <- Corpus(VectorSource(data))

#Stemming
stem_text <- function(text,mc.cores=1) {
  #stem each word in a block of text
  stem_string <- function(str)
  {
    str <- tokenize(x=str)
    str <- sapply(str, katadasaR)
    str <- paste(str, collapse="")
    return(str)
  }
  #stem each text block in turn
  x <- mclapply(X=text, FUN=stem_string, mc.cores=mc.cores)
  #return stemed text blocks
  return(unlist(x))
}
corpus<- tm_map(corpus, stem_text)

#filtering
myStopwords = readLines("stopwordID.csv")
corpus<- tm_map(corpus, removeWords, myStopwords)
corpus <- tm_map(corpus, removeWords,
               c("covid","corona","julai","tanggal","virus", "sih", "nya","pei"))

##tokenizing
tokenizing <- function(x) strsplit(as.character(x), ";")
corpus<- tm_map(corpus, tokenizing)

#replace word
corpus<- tm_map(corpus, gsub, pattern="berik", replacement="beri")
corpus<- tm_map(corpus, gsub, pattern="terjd", replacement="terjadi")
corpus<- tm_map(corpus, gsub, pattern="kes", replacement="kasus")
corpus<- tm_map(corpus, gsub, pattern="moga", replacement="semoga")
corpus<- tm_map(corpus, gsub, pattern="laku", replacement="lakukan")
corpus<- tm_map(corpus, gsub, pattern="laksana", replacement="laksanakan")
corpus<- tm_map(corpus, gsub, pattern="kalo", replacement="kalau")

#Build document term matrix
#method 1
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
dataframe<-data.frame(text=unlist(sapply(corpus, `[`)),
                      stringsAsFactors=F)

write.csv(dataframe, "data_cleaning.csv")
