install.packages("stringr")
install.packages("plyr")

library("plyr")
library("stringr")

####################################
# Analysis Function
####################################

analyse.sentiment = function(sentences, pos.words, neu.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":

  sentiments = laply(sentences$article, function(sentence, pos.words, neu.words, neg.words) {

  # clean up sentences with R's regex-driven global substitute, gsub():

  sentence = gsub('[^a-zA-Z]', ' ', sentence)

  # and convert to lower case:

  sentence = tolower(sentence)

  # split into words. str_split is in the stringr package

  word.list = str_split(sentence, '\\s+')

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

  sentiments.df = data.frame(positiv=sentiments[ ,1], neutral=sentiments[ ,2], negativ=sentiments[ ,3], positiv.cum=sentiments[ ,4], neutral.cum=sentiments[ ,5], negativ.cum=sentiments[ ,6], headline=sentences$headline, weekday=sentences$weekday, day=sentences$day, month=sentences$month, year=sentences$year, time=sentences$time, cats=sentences$cats)
  return(sentiments.df)
}

####################################
# -1- Data Preparation
####################################

# Load sentiment files
pos.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Positive-Lemma-21042012.tsv', encoding="UTF-8", header=FALSE, sep="\t")
neg.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Negative-Lemma-21042012.tsv', encoding="UTF-8", header=FALSE, sep="\t")
neu.table = read.table('/Users/admin/Desktop/GSN/Sentimentanalyse/GermanPolarityClues-2012/GermanPolarityClues-Neutral-Lemma-21042012.tsv', encoding="UTF-8", header=FALSE, sep="\t")

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

# Load CSV File
data <- read.csv("Desktop/GSN/Sentimentanalyse/testSPON.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data$day <- as.Date(data$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data$year = laply(data$day, function(date) unlist(str_split(date, '\\.'))[3])
data$month = laply(data$day, function(date) unlist(str_split(date, '\\.'))[2])
data$day = laply(data$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert String to factor
data$article<-as.factor(data$article)

####################################
# -2- Data Analysis
####################################

# Analyse text regarding all Part Of Speech
data.sentiments.all = analyse.sentiment(data, pos.words, neu.words, neg.words, .progress='text')

# Analyse text regarding only NN
data.sentiments.nn = analyse.sentiment(data, nn.pos.words, nn.neu.words, nn.neg.words, .progress='text')

# Analyse text regarding only NE
data.sentiments.ne = analyse.sentiment(data, ne.pos.words, ne.neu.words, ne.neg.words, .progress='text')

# Analyse text regarding only XY
data.sentiments.xy = analyse.sentiment(data, xy.pos.words, xy.neu.words, xy.neg.words, .progress='text')

# Analyse text regarding only CA
data.sentiments.ca = analyse.sentiment(data, ca.pos.words, ca.neu.words, ca.neg.words, .progress='text')

# Analyse text regarding only AD
data.sentiments.ad = analyse.sentiment(data, ad.pos.words, ad.neu.words, ad.neg.words, .progress='text')

# Analyse text regarding only PD
data.sentiments.pd = analyse.sentiment(data, pd.pos.words, pd.neu.words, pd.neg.words, .progress='text')

# Analyse text regarding only VV
data.sentiments.vv = analyse.sentiment(data, vv.pos.words, vv.neu.words, vv.neg.words, .progress='text')

# Analyse text regarding only AP
data.sentiments.ap = analyse.sentiment(data, ap.pos.words, ap.neu.words, ap.neg.words, .progress='text')

# Analyse text regarding only PT
data.sentiments.pt = analyse.sentiment(data, pt.pos.words, pt.neu.words, pt.neg.words, .progress='text')

# Analyse text regarding only FM
data.sentiments.fm = analyse.sentiment(data, fm.pos.words, fm.neu.words, fm.neg.words, .progress='text')

# Analyse text regarding only PI
data.sentiments.pi = analyse.sentiment(data, pi.pos.words, pi.neu.words, pi.neg.words, .progress='text')

# Analyse text regarding only KO
data.sentiments.ko = analyse.sentiment(data, ko.pos.words, ko.neu.words, ko.neg.words, .progress='text')

# Analyse text regarding only PR
data.sentiments.pr = analyse.sentiment(data, pr.pos.words, pr.neu.words, pr.neg.words, .progress='text')

# Analyse text regarding only PP
data.sentiments.pp = analyse.sentiment(data, pp.pos.words, pp.neu.words, pp.neg.words, .progress='text')

####################################
# -3- Result Processing
####################################

# Filter data by Categories
data.filtered.politik <- filter(data.sentiments.all, grepl("Politik", data.sentiments.all$cats))
data.filtered.wirtschaft <- filter(data.sentiments.all, grepl("Wirtschaft", data.sentiments.all$cats))
data.filtered.ausland <- filter(data.sentiments.all, grepl("Ausland", data.sentiments.all$cats))
data.filtered.nahostkonflikt <- filter(data.sentiments.all, grepl("Nahostkonflikt", data.sentiments.all$cats))

# Group data by Day, Month, Year and cummulate Sentiments
all.cum = summarise(group_by(data.sentiments.all, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
nn.cum = summarise(group_by(data.sentiments.nn, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ne.cum = summarise(group_by(data.sentiments.ne, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
xy.cum = summarise(group_by(data.sentiments.xy, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ca.cum = summarise(group_by(data.sentiments.ca, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ad.cum = summarise(group_by(data.sentiments.ad, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pd.cum = summarise(group_by(data.sentiments.pd, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
vv.cum = summarise(group_by(data.sentiments.vv, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ap.cum = summarise(group_by(data.sentiments.ap, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pt.cum = summarise(group_by(data.sentiments.pt, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
fm.cum = summarise(group_by(data.sentiments.fm, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pi.cum = summarise(group_by(data.sentiments.pi, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ko.cum = summarise(group_by(data.sentiments.ko, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pr.cum = summarise(group_by(data.sentiments.pr, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pp.cum = summarise(group_by(data.sentiments.pp, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))

# Order all results by Day and Time
#data.sentiments.all <- data.sentiments.all[order(data.sentiments.all$day, data.sentiments.all$time), ]
#data.sentiments.nn <- data.sentiments.nn[order(data.sentiments.nn$day, data.sentiments.nn$time), ]
#data.sentiments.ne <- data.sentiments.ne[order(data.sentiments.ne$day, data.sentiments.ne$time), ]
#data.sentiments.xy <- data.sentiments.xy[order(data.sentiments.xy$day, data.sentiments.xy$time), ]
#data.sentiments.ca <- data.sentiments.ca[order(data.sentiments.ca$day, data.sentiments.ca$time), ]
#data.sentiments.ad <- data.sentiments.ad[order(data.sentiments.ad$day, data.sentiments.ad$time), ]
#data.sentiments.pd <- data.sentiments.pd[order(data.sentiments.pd$day, data.sentiments.pd$time), ]
#data.sentiments.vv <- data.sentiments.vv[order(data.sentiments.vv$day, data.sentiments.vv$time), ]
#data.sentiments.ap <- data.sentiments.ap[order(data.sentiments.ap$day, data.sentiments.ap$time), ]
#data.sentiments.pt <- data.sentiments.pt[order(data.sentiments.pt$day, data.sentiments.pt$time), ]
#data.sentiments.fm <- data.sentiments.fm[order(data.sentiments.fm$day, data.sentiments.fm$time), ]
#data.sentiments.pi <- data.sentiments.pi[order(data.sentiments.pi$day, data.sentiments.pi$time), ]
#data.sentiments.ko <- data.sentiments.ko[order(data.sentiments.ko$day, data.sentiments.ko$time), ]
#data.sentiments.pr <- data.sentiments.pr[order(data.sentiments.pr$day, data.sentiments.pr$time), ]
#data.sentiments.pp <- data.sentiments.pp[order(data.sentiments.pp$day, data.sentiments.pp$time), ]

####################################
# -4- Saving Result
####################################

#write.csv(data.sentiments.all, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.nn, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.ne, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.xy, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.ca, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.ad, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.pd, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.vv, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.ap, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.pt, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.fm, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.pi, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.ko, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.pr, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)
#write.csv(data.sentiments.pp, file="/Users/admin/Desktop/GSN/Sentimentanalyse/SPONSentiments.csv", sep="\t", row.names=TRUE)

# Appearances of Parts Of Speech in Sentiment Lists

#negativ
#1  NN
#2  NE
#3  XY
#4  CA
#5  AD
#6  PD
#7  VV
#8  AP
#9  PT
#10 FM
#11 PI
#12 VM

#neutral
#1  NN
#2  NE
#3  FM
#4  XY
#5  VV
#6  KO
#7  AD
#8  PI
#9  PT
#10 AP
#11 PR
#12 VM
#13 PP

#positiv
#1 NN
#2 VV
#3 FM
#4 NE
#5 XY
#6 AD
#7 CA
#8 AP
#9 PT
