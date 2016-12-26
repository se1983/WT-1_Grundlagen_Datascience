install.packages("koRpus")
install.packages("stringr")
install.packages("functional")
install.packages("RJSONIO")
install.packages("~/Library/RStudio/rmr2_3.3.1.tar.gz", repos = NULL, type = "source")
help(hadoop.settings)

library("plyr")
library("koRpus")

####################################
# Analysis Function
####################################

analyse.text = function(data, .progress='none')
{
  require(koRpus)
  require(plyr)

  result = laply(data$article, function(article) {

    file.connection <- file("~/Desktop/GSN/tmp/tmp.txt")
    writeLines(article, file.connection)
    close(file.connection)
    tagged.text <- treetag("~/Desktop/GSN/tmp/tmp.txt", treetagger="manual", lang="de", TT.options=list(path="~/Library/Treetagger", preset="de-utf8"))

    #write(data.frame(taggedText(tagged.text)), file="~/Desktop/GSN/tmp/tmp.txt", append=TRUE)
    #write.csv(data.frame(taggedText(tagged.text)), file="~/Desktop/GSN/tmp/tagged.csv", sep="\t", row.names=FALSE, append=TRUE)

    result = textFeatures(tagged.text)

    return(result)

  }, .progress=.progress )

  result.df = data.frame(data, result)
  return(result.df)
}


####################################
# -1- Data Preparation
####################################

# Load CSV File
data <- read.csv("Desktop/GSN/Sentimentanalyse/testSPON.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data$day <- as.Date(data$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data$year = laply(data$day, function(date) unlist(str_split(date, '\\.'))[3])
data$month = laply(data$day, function(date) unlist(str_split(date, '\\.'))[2])
data$day = laply(data$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data$article <- lapply(data$article, as.character)

####################################
# -2- Data Analysis
####################################

# Analyse text regarding all Part Of Speech
data.text.analysis = analyse.text(data, .progress='text')


#token <- "NA"
#tag <- "NA"
#lemma <- "NA"
#lttr <- "NA"
#wclass <- "NA"
#desc <- "NA"
#stop <- "NA"
#stem <- "NA"
#headline <- "NA"
#weekday <- "NA"
#day <- "NA"
#month <- "NA"
#year <- "NA"
#time <- "NA"
#cats <- "NA"

#test = data.frame(token, tag, lemma, lttr, wclass, desc, stop, stem, headline, weekday, day, month)


#tagged.text <- treetag("/Users/admin/Desktop/GSN/Sentimentanalyse/test.txt", treetagger="manual", lang="de", TT.options=list(path="~/Treetagger", preset="de-utf8"))
#str(describe(tagged.text))
#lex.div(tagged.text)
#show(tagged.text)
#kRp.filter.wclass(txt, corp.rm.class = "nonpunct", corp.rm.tag = c(),as.vector = FALSE)
