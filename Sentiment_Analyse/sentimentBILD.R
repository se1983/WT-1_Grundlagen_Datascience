#install.packages("stringr")
#install.packages("plyr")
#install.packages("dplyr")

library("plyr")
library("stringr")
library("dplyr")

####################################
# Analysis Function
####################################

analyse.sentiment = function(data, pos.words, neu.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":

  sentiments = laply(data$article, function(article, pos.words, neu.words, neg.words) {

  # clean up article with R's regex-driven global substitute, gsub():
  article = gsub('[^a-zA-Z]', ' ', article)

  # and convert to lower case:
  article = tolower(article)

  # split into words. str_split is in the stringr package
  word.list = str_split(article, '\\s+')

  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)

  # compare words to the dictionaries of positiv & neutral & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  neu.matches = match(words, neu.words)

  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:

  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  neu.matches = !is.na(neu.matches)

  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  result = c(sum(pos.matches), sum(neu.matches), sum(neg.matches), sum(pos.matches)/length(words), sum(neu.matches)/length(words), sum(neg.matches)/length(words))
  return(result)

  }, pos.words, neu.words, neg.words, .progress=.progress )

  sentiments.df = data.frame(positiv_abs=sentiments[ ,1], neutral_abs=sentiments[ ,2], negativ_abs=sentiments[ ,3], positiv_rel=sentiments[ ,4], neutral_rel=sentiments[ ,5], negativ_rel=sentiments[ ,6], day=data$day, month=data$month, year=data$year)
  return(sentiments.df)
}

####################################
# -1- Data Preparation
####################################

######### Sentiment Files ##########

# Load sentiment files
pos.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Positive-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")
neg.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Negative-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")
neu.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Neutral-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")

# Filter all sentiment words to word lists
pos.words = pos.table$V2
neg.words = neg.table$V2
neu.words = neu.table$V2

# Filter all NN sentiment words to word lists
nn.neg.words = neg.table[neg.table$V3 == "NN", ]$V2
nn.pos.words = pos.table[pos.table$V3 == "NN", ]$V2
nn.neu.words = neu.table[neu.table$V3 == "NN", ]$V2

# Filter all NE sentiment words to word lists
ne.neg.words = neg.table[neg.table$V3 == "NE", ]$V2
ne.pos.words = pos.table[pos.table$V3 == "NE", ]$V2
ne.neu.words = neu.table[neu.table$V3 == "NE", ]$V2

# Filter all XY sentiment words to word lists
xy.neg.words = neg.table[neg.table$V3 == "XY", ]$V2
xy.pos.words = pos.table[pos.table$V3 == "XY", ]$V2
xy.neu.words = neu.table[neu.table$V3 == "XY", ]$V2

# Filter all CA sentiment words to word lists
ca.neg.words = neg.table[neg.table$V3 == "CA", ]$V2
ca.pos.words = pos.table[pos.table$V3 == "CA", ]$V2
ca.neu.words = neu.table[neu.table$V3 == "CA", ]$V2

# Filter all AD sentiment words to word lists
ad.neg.words = neg.table[neg.table$V3 == "AD", ]$V2
ad.pos.words = pos.table[pos.table$V3 == "AD", ]$V2
ad.neu.words = neu.table[neu.table$V3 == "AD", ]$V2

# Filter all PD sentiment words to word lists
pd.neg.words = neg.table[neg.table$V3 == "PD", ]$V2
pd.pos.words = pos.table[pos.table$V3 == "PD", ]$V2
pd.neu.words = neu.table[neu.table$V3 == "PD", ]$V2

# Filter all VV sentiment words to word lists
vv.neg.words = neg.table[neg.table$V3 == "VV", ]$V2
vv.pos.words = pos.table[pos.table$V3 == "VV", ]$V2
vv.neu.words = neu.table[neu.table$V3 == "VV", ]$V2

# Filter all AP sentiment words to word lists
ap.neg.words = neg.table[neg.table$V3 == "AP", ]$V2
ap.pos.words = pos.table[pos.table$V3 == "AP", ]$V2
ap.neu.words = neu.table[neu.table$V3 == "AP", ]$V2

# Filter all PT sentiment words to word lists
pt.neg.words = neg.table[neg.table$V3 == "PT", ]$V2
pt.pos.words = pos.table[pos.table$V3 == "PT", ]$V2
pt.neu.words = neu.table[neu.table$V3 == "PT", ]$V2

# Filter all FM sentiment words to word lists
fm.neg.words = neg.table[neg.table$V3 == "FM", ]$V2
fm.pos.words = pos.table[pos.table$V3 == "FM", ]$V2
fm.neu.words = neu.table[neu.table$V3 == "FM", ]$V2

# Filter all PI sentiment words to word lists
pi.neg.words = neg.table[neg.table$V3 == "PI", ]$V2
pi.pos.words = pos.table[pos.table$V3 == "PI", ]$V2
pi.neu.words = neu.table[neu.table$V3 == "PI", ]$V2

# Filter all VM sentiment words to word lists
vm.neg.words = neg.table[neg.table$V3 == "VM", ]$V2
vm.pos.words = pos.table[pos.table$V3 == "VM", ]$V2
vm.neu.words = neu.table[neu.table$V3 == "VM", ]$V2

# Filter all KO sentiment words to word lists
ko.neg.words = neg.table[neg.table$V3 == "KO", ]$V2
ko.pos.words = pos.table[pos.table$V3 == "KO", ]$V2
ko.neu.words = neu.table[neu.table$V3 == "KO", ]$V2

# Filter all PR sentiment words to word lists
pr.neg.words = neg.table[neg.table$V3 == "PR", ]$V2
pr.pos.words = pos.table[pos.table$V3 == "PR", ]$V2
pr.neu.words = neu.table[neu.table$V3 == "PR", ]$V2

# Filter all PP sentiment words to word lists
pp.neg.words = neg.table[neg.table$V3 == "PP", ]$V2
pp.pos.words = pos.table[pos.table$V3 == "PP", ]$V2
pp.neu.words = neu.table[neu.table$V3 == "PP", ]$V2

############### BILD ################

# Load CSV File
data.BILD <- read.csv("/Users/admin/Desktop/GSN/Daten/bild2.csv", encoding="UTF-16LE", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data.BILD$day <- as.Date(data.BILD$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data.BILD$year = laply(data.BILD$V1, function(date) unlist(str_split(date, '\\-'))[1])
data.BILD$month = laply(data.BILD$V1, function(date) unlist(str_split(date, '\\-'))[2])
data.BILD$day = laply(data.BILD$V1, function(date) unlist(str_split(date, '\\-'))[3])
data.BILD$day = laply(data.BILD$day, function(date) unlist(str_split(date, 'T'))[1])

data.BILD = data.frame(article=data.BILD$V2, url=data.BILD$V3, day=data.BILD$day, month=data.BILD$month, year=data.BILD$year)

# Convert String to factor
data.BILD$article <- laply(data.BILD$article, as.character)
data.BILD <- filter(data.BILD, data.BILD$article != "")
data.BILD$article<-as.factor(data.BILD$article)

####################################
# -2- Data Analysis
####################################

############### BILD ################

# Analyse text regarding all Part Of Speech
data.BILD.sentiments.all = analyse.sentiment(data.BILD, pos.words, neu.words, neg.words, .progress='text')
# Analyse text regarding only NN
data.BILD.sentiments.nn = analyse.sentiment(data.BILD, nn.pos.words, nn.neu.words, nn.neg.words, .progress='text')
# Analyse text regarding only NE
data.BILD.sentiments.ne = analyse.sentiment(data.BILD, ne.pos.words, ne.neu.words, ne.neg.words, .progress='text')
# Analyse text regarding only XY
data.BILD.sentiments.xy = analyse.sentiment(data.BILD, xy.pos.words, xy.neu.words, xy.neg.words, .progress='text')
# Analyse text regarding only CA
data.BILD.sentiments.ca = analyse.sentiment(data.BILD, ca.pos.words, ca.neu.words, ca.neg.words, .progress='text')
# Analyse text regarding only AD
data.BILD.sentiments.ad = analyse.sentiment(data.BILD, ad.pos.words, ad.neu.words, ad.neg.words, .progress='text')
# Analyse text regarding only PD
data.BILD.sentiments.pd = analyse.sentiment(data.BILD, pd.pos.words, pd.neu.words, pd.neg.words, .progress='text')
# Analyse text regarding only VV
data.BILD.sentiments.vv = analyse.sentiment(data.BILD, vv.pos.words, vv.neu.words, vv.neg.words, .progress='text')
# Analyse text regarding only AP
data.BILD.sentiments.ap = analyse.sentiment(data.BILD, ap.pos.words, ap.neu.words, ap.neg.words, .progress='text')
# Analyse text regarding only PT
data.BILD.sentiments.pt = analyse.sentiment(data.BILD, pt.pos.words, pt.neu.words, pt.neg.words, .progress='text')
# Analyse text regarding only FM
data.BILD.sentiments.fm = analyse.sentiment(data.BILD, fm.pos.words, fm.neu.words, fm.neg.words, .progress='text')
# Analyse text regarding only PI
data.BILD.sentiments.pi = analyse.sentiment(data.BILD, pi.pos.words, pi.neu.words, pi.neg.words, .progress='text')
# Analyse text regarding only KO
data.BILD.sentiments.ko = analyse.sentiment(data.BILD, ko.pos.words, ko.neu.words, ko.neg.words, .progress='text')
# Analyse text regarding only PR
data.BILD.sentiments.pr = analyse.sentiment(data.BILD, pr.pos.words, pr.neu.words, pr.neg.words, .progress='text')
# Analyse text regarding only PP
data.BILD.sentiments.pp = analyse.sentiment(data.BILD, pp.pos.words, pp.neu.words, pp.neg.words, .progress='text')

####################################
########## All Catagories ##########
####################################

catagory = "All"

####################################
# -3- Result Processing
####################################

############### BILD ################

############# Grouping ##############
######### By Year, Month, Day #######

data.BILD.grouped.all = summarise(group_by(data.BILD.sentiments.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.nn = summarise(group_by(data.BILD.sentiments.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.ne = summarise(group_by(data.BILD.sentiments.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.xy = summarise(group_by(data.BILD.sentiments.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.ca = summarise(group_by(data.BILD.sentiments.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.ad = summarise(group_by(data.BILD.sentiments.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.pd = summarise(group_by(data.BILD.sentiments.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.vv = summarise(group_by(data.BILD.sentiments.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.ap = summarise(group_by(data.BILD.sentiments.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.pt = summarise(group_by(data.BILD.sentiments.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.fm = summarise(group_by(data.BILD.sentiments.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.pi = summarise(group_by(data.BILD.sentiments.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.ko = summarise(group_by(data.BILD.sentiments.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.pr = summarise(group_by(data.BILD.sentiments.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.BILD.grouped.pp = summarise(group_by(data.BILD.sentiments.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.BILD.grouped.all <- data.BILD.grouped.all[order(data.BILD.grouped.all$year, data.BILD.grouped.all$month, data.BILD.grouped.all$day), ]
data.BILD.grouped.nn <- data.BILD.grouped.nn[order(data.BILD.grouped.nn$year, data.BILD.grouped.nn$month, data.BILD.grouped.nn$day), ]
data.BILD.grouped.ne <- data.BILD.grouped.ne[order(data.BILD.grouped.ne$year, data.BILD.grouped.ne$month, data.BILD.grouped.ne$day), ]
data.BILD.grouped.xy <- data.BILD.grouped.xy[order(data.BILD.grouped.xy$year, data.BILD.grouped.xy$month, data.BILD.grouped.xy$day), ]
data.BILD.grouped.ca <- data.BILD.grouped.ca[order(data.BILD.grouped.ca$year, data.BILD.grouped.ca$month, data.BILD.grouped.ca$day), ]
data.BILD.grouped.ad <- data.BILD.grouped.ad[order(data.BILD.grouped.ad$year, data.BILD.grouped.ad$month, data.BILD.grouped.ad$day), ]
data.BILD.grouped.pd <- data.BILD.grouped.pd[order(data.BILD.grouped.pd$year, data.BILD.grouped.pd$month, data.BILD.grouped.pd$day), ]
data.BILD.grouped.vv <- data.BILD.grouped.vv[order(data.BILD.grouped.vv$year, data.BILD.grouped.vv$month, data.BILD.grouped.vv$day), ]
data.BILD.grouped.ap <- data.BILD.grouped.ap[order(data.BILD.grouped.ap$year, data.BILD.grouped.ap$month, data.BILD.grouped.ap$day), ]
data.BILD.grouped.pt <- data.BILD.grouped.pt[order(data.BILD.grouped.pt$year, data.BILD.grouped.pt$month, data.BILD.grouped.pt$day), ]
data.BILD.grouped.fm <- data.BILD.grouped.fm[order(data.BILD.grouped.fm$year, data.BILD.grouped.fm$month, data.BILD.grouped.fm$day), ]
data.BILD.grouped.pi <- data.BILD.grouped.pi[order(data.BILD.grouped.pi$year, data.BILD.grouped.pi$month, data.BILD.grouped.pi$day), ]
data.BILD.grouped.ko <- data.BILD.grouped.ko[order(data.BILD.grouped.ko$year, data.BILD.grouped.ko$month, data.BILD.grouped.ko$day), ]
data.BILD.grouped.pr <- data.BILD.grouped.pr[order(data.BILD.grouped.pr$year, data.BILD.grouped.pr$month, data.BILD.grouped.pr$day), ]
data.BILD.grouped.pp <- data.BILD.grouped.pp[order(data.BILD.grouped.pp$year, data.BILD.grouped.pp$month, data.BILD.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### BILD ################

write.csv(data.BILD.grouped.all, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/", "BILD_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.nn, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.ne, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.xy, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.ca, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.ad, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.pd, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.vv, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.ap, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.pt, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.fm, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.pi, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.ko, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.pr, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
write.csv(data.BILD.grouped.pp, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/", catagory, "/",  "BILD_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")
