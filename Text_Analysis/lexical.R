install.packages("plyr")
install.packages("dplyr")
install.packages("koRpus")
install.packages("stringr")

library("plyr")
library("dplyr")
library("koRpus")
library("stringr")

####################################
# Analysis Function
####################################

analyse.text = function(data)
{
  result = laply(data$article, function(article) {
    return(textFeatures(get.tagged.text(article)))
  })

  result.df = data.frame(data, result)
  return(result.df)
}

get.tagged.text <- function(article) {

  article <- as.character(article)

  file.connection <- file("~/Desktop/GSN/tmp/tmp.txt")
  writeLines(article, file.connection)
  close(file.connection)

  tagged.text <- treetag("~/Desktop/GSN/tmp/tmp.txt", treetagger="manual", lang="de", TT.options=list(path="~/Library/Treetagger", preset="de-utf8"))
  return(tagged.text)
}

####################################
# -1- Data Preparation
####################################

## SPON
# Load CSV File
data.SPON <- read.csv("/Users/admin/Desktop/GSN/Daten/SPON_complete.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Split String into Day, Month, Year
data.SPON$year = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[3])
data.SPON$month = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[2])
data.SPON$day = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data.SPON$article <- laply(data.SPON$article, as.character)

##JF
# Load CSV File
data.JF <- read.csv("/Users/admin/Desktop/GSN/Daten/jungefreiheit.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Split String into Day, Month, Year
data.JF$year = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[3])
data.JF$month = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[2])
data.JF$day = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data.JF$article <- laply(data.JF$article, as.character)

####################################
# -2- Data Analysis
####################################

# Analyse text regarding all Part Of Speech
text.analysis.SPON = analyse.text(data.SPON)
text.analysis.JF = analyse.text(data.JF)

####################################
# -3- Result Processing
####################################


####################################
# -4- Saving Result
####################################

write.csv(text.analysis.SPON, file=paste("/Users/admin/Desktop/GSN/Textanalyse/lexical/SPON/", Sys.time(), ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(text.analysis.JF, file=paste("/Users/admin/Desktop/GSN/Textanalyse/lexical/JF/", Sys.time(), ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
