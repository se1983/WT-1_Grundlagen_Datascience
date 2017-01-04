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
data.JF <- read.csv("/share/DATA/jungefreiheit.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Split String into Day, Month, Year
data.JF$year = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[3])
data.JF$month = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[2])
data.JF$day = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert Factor to String
data.JF$article <- laply(data.JF$article, as.character)
data.JF <- filter(data.JF, data.JF$article != "")

data.JF.grouped = summarise(group_by(data.JF, year), article = paste(article, sep = ' ', collapse = ' '))

####################################
# -2- Data Saving
####################################

for(i in 1:length(data.JF.grouped$article)) {
  write.csv(data.JF.grouped$article[i], file=paste("/share/tmp/lexical/JF/", data$year[i], ".csv"), row.names=FALSE, fileEncoding = "UTF-16LE")
}
