#install.packages("plyr")
#install.packages("dplyr")
#install.packages("stringr")

library("plyr")
library("dplyr")
library("stringr")

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

data.SPON.grouped = summarise(group_by(data.SPON, year), article = paste(article, sep = ' ', collapse = ' '))

####################################
# -2- Data Saving
####################################

for(i in 1:length(data.SPON.grouped$article)) {
  write.csv(data.SPON.grouped$article[i], file=paste("/share/tmp/lexical/SPON/", data$year[i], ".csv"), row.names=FALSE, fileEncoding = "UTF-16LE")
}
