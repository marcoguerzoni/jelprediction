load("finaldtmprediction.Rdata")



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
print(fig)


png(filename="topten.png")
plot(fig)
dev.off()


dfplot <- as.data.frame(melt(many))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")+ 
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
print(fig)

install.packages("wordcloud") # word-cloud generator
library(wordcloud)
set.seed(1234)
wordcloud<- wordcloud(words = names(wordcount), freq = wordcount, min.freq = 1,
                      max.words=200, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))

png(filename="wordcloud.png")
plot(wordcloud)
dev.off()


#22 311
# 500+
#1000
#dtm.new   <- dtm[rowTotals> 0, ] 
load("ldamodel.Rdata")

test.topics <- posterior(ap_lda1,dtm)
test.topics1 <- apply(test.topics$topics, 1, which.max)

test.topics[2]
hist(test.topics)
library(lattice)
print(levelplot(test.topics[[2]], xlab = "Articles" , ylab = "Topic" , scales = list(tck = 1, x = list(rot = 45)), col.regions = gray (27:0/27), colorkey = list(space = "right",tick.number = 10)))













