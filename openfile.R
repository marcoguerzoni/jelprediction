#opening a long list of file xml and create a table with interesting variable only


install.packages("NLP")
install.packages("pdftools")
install.packages("tm")
install.packages("proxy")
install.packages("topicmodels")
install.packages("mime")
install.packages("stringr")
install.packages("tesseract")
install.packages("textclean")
install.packages("SnowballC")
install.packages("tidytext")
install.packages("purrr")
install.packages("udpipe")
install.packages("rlist")

setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelprediction")
#very important

library(pdftools)
library(purrr)
library(NLP)
library(tm)
library(proxy) #require last version of R
library(topicmodels)
library(httpuv)
library(mime)
#library(servr)
library(stringr)
library(tesseract)
library(textclean)
source("ldavis_function.R")
source("removecommonterms.R")
library(remotes)
library(tidyr)
library(tidytext)
library(ggplot2)
library(rlist)
library(LDAvis)


setwd("ngram1")
library(NLP)
fileList<-list.files(pattern=".txt")
doclist <- vector(mode = "list", length = length(fileList))


for (j in 22469:length(fileList) ){
txt <-as.data.frame(scan(fileList[j], what="character", sep="\t"), colnames=c("data"))
even_indexes<-seq(2,dim(txt)[1],2)
odd_indexes<-seq(1,dim(txt)[1],2)
text<-as.data.frame(as.character(txt[odd_indexes,]))
text$freq<-as.numeric(as.character(txt[even_indexes,]))
names(text)[1] <- "term"
colnames(text)[2] <- "freq"

for (i in 1:dim(text)[1]){
  

text$content[i]<- ifelse(as.numeric(text$freq[1])>1,paste(rep(text$term[i], times=text$freq[i]), collapse = " "),text$term[i])

}

doclist[[j]]<-paste(text$content, collapse = " ")
print(j)
}


list.save(doclist, file = "dataharvesting.RData")
txt<- list.load("dataharvesting.RData")

#########cleaning

txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt<-gsub("http[^[:space:]]*", "", txt)
txt<-gsub("www[^[:space:]]*", "", txt)
txt<-gsub('[\u2013:\u2016]', "", txt)
txt<-stringi::stri_trans_general(txt, "latin-ascii")
txt <- gsub("[^\x20-\x7E]", "", txt)

txt <- replace_non_ascii(txt, replacement = "", remove.nonconverted = TRUE)




#create a corpus
corp <- Corpus(VectorSource(txt))


######CLEAN CORPUS


corp  <- tm_map(corp , stemDocument)
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeWords,stopwords("en"))
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,removeNumbers)



dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
dtm1<-removeSparseTerms(dtm, 0.98)
dtm1 <- removeCommonTerms(dtm1 ,0.8)
library(topicmodels)



#convert dtm1 into a corpus
dtm2list <- apply(dtm1, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert to a Corpus
myCorp <- Corpus(VectorSource(dtm2list))


#reconvert Coprus to DTM
dtm <- DocumentTermMatrix(myCorp)

#DTM and myCorp are the final data to use.
setwd("..")

save(dtm, file="jstorfinaldtm.Rdata")
load("jstorfinaldtm.Rdata")
save(myCorp, file = "jstorfinalcorpus.Rdata")
load("jstorfinalcorpus.Rdata")
####some statistic
wordcount <- colSums(as.matrix(dtm))
topten <- head(sort(wordcount, decreasing=TRUE), 10)
many <- head(sort(wordcount, decreasing=TRUE), 10000)

library(reshape2)


#plot topten
dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
#print(fig)


png(filename="jstortopten.png")
plot(fig)
dev.off()


# dfplot <- as.data.frame(melt(many))
# dfplot$word <- dimnames(dfplot)[[1]]
# dfplot$word <- factor(dfplot$word,
#                       levels=dfplot$word[order(dfplot$value,
#                                                decreasing=TRUE)])
# 
# fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
# fig <- fig + xlab("Word in Corpus")
# fig <- fig + ylab("Count")+ 
#   theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank())
# print(fig)

#install.packages("wordcloud") # word-cloud generator
library(wordcloud)
set.seed(1234)
# wordcloud<- wordcloud(words = names(wordcount), freq = wordcount, min.freq = 1,
#                       max.words=200, random.order=FALSE, rot.per=0.35, 
#                       colors=brewer.pal(8, "Dark2"))
# 
# png(filename="jstorwordcloud.png")
# plot(wordcloud)
# dev.off()


#22 311
# 500+
#1000
#dtm.new   <- dtm[rowTotals> 0, ] 
ap_lda1 <- LDA(dtm, 27, method = "Gibbs",control = list(iter = 100, seed = 33))

rowTotals<- apply(dtm,1,sum)
dtm<-dtm[rowTotals>0,]
save(ap_lda1, file = "jstorldamodel.Rdata")

serVis(topicmodels2LDAvis(ap_lda1),  out.dir = 'vis')

serVis(topicmodels2LDAvis(ap_lda1))

