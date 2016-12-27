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

analyse.text = function(data, .progress='none')
{
  result <- laply(data$article, function(article) {
    result <- textFeatures(get.tagged.text(article))
    write.csv(result, file=paste("/Users/admin/Desktop/GSN/Textanalyse/lexical/tmp/", Sys.time(), ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
    return ()
    #if(article != "")
    #else return (data.frame(uniquWd=c("NA"), complx=c("NA"), sntCt=c("NA"), sntLen=c("NA"), syllCt=c("NA"), charCt=c("NA"), lttrCt=c("NA"), FOG=c("NA"), flesch=c("NA")))
  }, .progress=.progress )

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
data.SPON <- filter(data.SPON, data.SPON$article != "")

##JF
# Load CSV File
data.JF <- read.csv("/Users/admin/Desktop/GSN/Daten/jungefreiheit.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Split String into Day, Month, Year
data.JF$year = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[3])
data.JF$month = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[2])
data.JF$day = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data.JF$article <- laply(data.JF$article, as.character)
data.JF <- filter(data.JF, data.JF$article != "")

####################################
# -2- Data Analysis
####################################

##SPON
# Analyse text regarding all Part Of Speech
text.analysis.SPON = analyse.text(data.SPON, .progress='text')

##JF
# Analyse text regarding all Part Of Speech
text.analysis.JF = analyse.text(data.JF, .progress='text')

####################################
# -3- Result Processing
####################################


####################################
# -4- Saving Result
####################################

write.csv(text.analysis.SPON, file="/Users/admin/Desktop/GSN/Textanalyse/lexical/SPON/SPON_lexical.csv", sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(text.analysis.JF, file="/Users/admin/Desktop/GSN/Textanalyse/lexical/JF/JF_lexical.csv", sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
