#install.packages("plyr")
#install.packages("dplyr")
#install.packages("/home/marisa/R/koRpus_0.06-5.tar.gz", repos = NULL, type = "source")
#install.packages("stringr")

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
    return (result)
  }, .progress=.progress )

  result.df = data.frame(data, result)
  return(result.df)
}

get.tagged.text <- function(article) {

  article <- as.character(article)

  file.connection <- file("/home/marisa/tmp/lexical/tmp.txt")
  writeLines(article, file.connection)
  close(file.connection)

  tagged.text <- treetag("/home/marisa/tmp/lexical/tmp.txt", treetagger="manual", lang="de", TT.options=list(path="/home/marisa/TreeTagger", preset="de-utf8"))
  return(tagged.text)
}

####################################
# -1- Data Preparation
####################################

# Load CSV File
data.SPON <- read.csv("/share/Data/SPON_complete", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Split String into Day, Month, Year
data.SPON$year = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[3])
data.SPON$month = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[2])
data.SPON$day = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data.SPON$article <- laply(data.SPON$article, as.character)
data.SPON <- filter(data.SPON, data.SPON$article != "")


####################################
# -2- Data Analysis
####################################

# Analyse text regarding all Part Of Speech
text.analysis.SPON = analyse.text(data.SPON, .progress='text')

####################################
# -4- Saving Result
####################################

write.csv(text.analysis.JF, file="/share/Ergebnisse/Lexical/JF/JF_lexical.csv", sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
