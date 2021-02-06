##Pelabelan
##Pelabelan
kalimat2<-read.csv("data_cleaning.csv",header=TRUE)

#skoring
positif <- scan("D:/MCC/Fajar/RRrr/positive-words.txt",what="character",comment.char=";")
negatif <- scan("D:/MCC/Fajar/RRrr/negative-words.txt",what="character ",comment.char=";")
kata.positif = c(positif, "bagus","suka","senang","nice","mantap")
kata.negatif = c(negatif, "taik","anjing","payah","susah","kecewa")
score.sentiment = function(kalimat2, kata.positif, kata.negatif,
                           .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat, kata.positif,
                                    kata.negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat)
    kalimat = gsub('[[:cntrl:]]', '', kalimat)
    kalimat = gsub('\\d+', '', kalimat)
    kalimat = tolower(kalimat)
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)}
hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
View(hasil)

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0,"Negatif", "Positif")
hasil$klasifikasi
View(hasil)

#EXCHANGE ROW SEQUENCE
data <- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "data_label.csv")
