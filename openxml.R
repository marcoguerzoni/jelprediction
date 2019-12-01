setwd("metadata")

library("XML")

# Also load the other required package.
library("methods")

# Give the input file name to the function.
xmldoc <- xmlParse(file = "journal-article-10.1086_666653.xml")
ex <- xmlToList(xmldoc)
rootNode <- xmlRoot(xmldoc)
data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
cd.catalog <- data.frame(t(data),row.names=NULL)
xmldataframe <- xmlToDataFrame("journal-article-10.2307_43553009.xml")

doc <- xmlTreeParse("journal-article-10.2307_43553009.xml")
names(xmlChildren(doc$doc$children[["dataset"]]))

# Print the result.
print(result)
data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))

setwd("ngram1")