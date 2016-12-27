install.packages("stringr")
install.packages("plyr")
install.packages("dplyr")

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

  sentiments.df = data.frame(positiv_abs=sentiments[ ,1], neutral_abs=sentiments[ ,2], negativ_abs=sentiments[ ,3], positiv_rel=sentiments[ ,4], neutral_rel=sentiments[ ,5], negativ_rel=sentiments[ ,6], headline=data$headline, weekday=data$weekday, day=data$day, month=data$month, year=data$year, time=data$time, cats=data$cats)
  return(sentiments.df)
}

####################################
# -1- Data Preparation
####################################

######### Sentiment Files ##########

# Load sentiment files
pos.table = read.table('/share/GermanPolarityClues-2012/GermanPolarityClues-Positive-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")
neg.table = read.table('/share/GermanPolarityClues-2012/GermanPolarityClues-Negative-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")
neu.table = read.table('/share/Data/GermanPolarityClues-Neutral-Lemma-21042012.tsv', encoding="UTF-16LE", header=FALSE, sep="\t")

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

############### SPON ################

# Load CSV File
data.SPON <- read.csv("/share/Data/SPON_complete", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data.SPON$day <- as.Date(data.SPON$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data.SPON$year = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[3])
data.SPON$month = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[2])
data.SPON$day = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert String to factor
data.SPON$article <- laply(data.SPON$article, as.character)
data.SPON <- filter(data.SPON, data.SPON$article != "")
data.SPON$article<-as.factor(data.SPON$article)

############### JF ################

# Load CSV File
data.JF <- read.csv("/share/DATA/jungefreiheit.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data.JF$day <- as.Date(data.JF$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data.JF$year = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[3])
data.JF$month = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[2])
data.JF$day = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert String to factor
data.JF$article <- laply(data.JF$article, as.character)
data.JF <- filter(data.JF, data.JF$article != "")
data.JF$article<-as.factor(data.JF$article)

####################################
# -2- Data Analysis
####################################

############### SPON ################

# Analyse text regarding all Part Of Speech
data.SPON.sentiments.all = analyse.sentiment(data.SPON, pos.words, neu.words, neg.words, .progress='text')
# Analyse text regarding only NN
data.SPON.sentiments.nn = analyse.sentiment(data.SPON, nn.pos.words, nn.neu.words, nn.neg.words, .progress='text')
# Analyse text regarding only NE
data.SPON.sentiments.ne = analyse.sentiment(data.SPON, ne.pos.words, ne.neu.words, ne.neg.words, .progress='text')
# Analyse text regarding only XY
data.SPON.sentiments.xy = analyse.sentiment(data.SPON, xy.pos.words, xy.neu.words, xy.neg.words, .progress='text')
# Analyse text regarding only CA
data.SPON.sentiments.ca = analyse.sentiment(data.SPON, ca.pos.words, ca.neu.words, ca.neg.words, .progress='text')
# Analyse text regarding only AD
data.SPON.sentiments.ad = analyse.sentiment(data.SPON, ad.pos.words, ad.neu.words, ad.neg.words, .progress='text')
# Analyse text regarding only PD
data.SPON.sentiments.pd = analyse.sentiment(data.SPON, pd.pos.words, pd.neu.words, pd.neg.words, .progress='text')
# Analyse text regarding only VV
data.SPON.sentiments.vv = analyse.sentiment(data.SPON, vv.pos.words, vv.neu.words, vv.neg.words, .progress='text')
# Analyse text regarding only AP
data.SPON.sentiments.ap = analyse.sentiment(data.SPON, ap.pos.words, ap.neu.words, ap.neg.words, .progress='text')
# Analyse text regarding only PT
data.SPON.sentiments.pt = analyse.sentiment(data.SPON, pt.pos.words, pt.neu.words, pt.neg.words, .progress='text')
# Analyse text regarding only FM
data.SPON.sentiments.fm = analyse.sentiment(data.SPON, fm.pos.words, fm.neu.words, fm.neg.words, .progress='text')
# Analyse text regarding only PI
data.SPON.sentiments.pi = analyse.sentiment(data.SPON, pi.pos.words, pi.neu.words, pi.neg.words, .progress='text')
# Analyse text regarding only KO
data.SPON.sentiments.ko = analyse.sentiment(data.SPON, ko.pos.words, ko.neu.words, ko.neg.words, .progress='text')
# Analyse text regarding only PR
data.SPON.sentiments.pr = analyse.sentiment(data.SPON, pr.pos.words, pr.neu.words, pr.neg.words, .progress='text')
# Analyse text regarding only PP
data.SPON.sentiments.pp = analyse.sentiment(data.SPON, pp.pos.words, pp.neu.words, pp.neg.words, .progress='text')

############### JF ################

# Analyse text regarding all Part Of Speech
data.JF.sentiments.all = analyse.sentiment(data.JF, pos.words, neu.words, neg.words, .progress='text')
# Analyse text regarding only NN
data.JF.sentiments.nn = analyse.sentiment(data.JF, nn.pos.words, nn.neu.words, nn.neg.words, .progress='text')
# Analyse text regarding only NE
data.JF.sentiments.ne = analyse.sentiment(data.JF, ne.pos.words, ne.neu.words, ne.neg.words, .progress='text')
# Analyse text regarding only XY
data.JF.sentiments.xy = analyse.sentiment(data.JF, xy.pos.words, xy.neu.words, xy.neg.words, .progress='text')
# Analyse text regarding only CA
data.JF.sentiments.ca = analyse.sentiment(data.JF, ca.pos.words, ca.neu.words, ca.neg.words, .progress='text')
# Analyse text regarding only AD
data.JF.sentiments.ad = analyse.sentiment(data.JF, ad.pos.words, ad.neu.words, ad.neg.words, .progress='text')
# Analyse text regarding only PD
data.JF.sentiments.pd = analyse.sentiment(data.JF, pd.pos.words, pd.neu.words, pd.neg.words, .progress='text')
# Analyse text regarding only VV
data.JF.sentiments.vv = analyse.sentiment(data.JF, vv.pos.words, vv.neu.words, vv.neg.words, .progress='text')
# Analyse text regarding only AP
data.JF.sentiments.ap = analyse.sentiment(data.JF, ap.pos.words, ap.neu.words, ap.neg.words, .progress='text')
# Analyse text regarding only PT
data.JF.sentiments.pt = analyse.sentiment(data.JF, pt.pos.words, pt.neu.words, pt.neg.words, .progress='text')
# Analyse text regarding only FM
data.JF.sentiments.fm = analyse.sentiment(data.JF, fm.pos.words, fm.neu.words, fm.neg.words, .progress='text')
# Analyse text regarding only PI
data.JF.sentiments.pi = analyse.sentiment(data.JF, pi.pos.words, pi.neu.words, pi.neg.words, .progress='text')
# Analyse text regarding only KO
data.JF.sentiments.ko = analyse.sentiment(data.JF, ko.pos.words, ko.neu.words, ko.neg.words, .progress='text')
# Analyse text regarding only PR
data.JF.sentiments.pr = analyse.sentiment(data.JF, pr.pos.words, pr.neu.words, pr.neg.words, .progress='text')
# Analyse text regarding only PP
data.JF.sentiments.pp = analyse.sentiment(data.JF, pp.pos.words, pp.neu.words, pp.neg.words, .progress='text')

####################################
########## All Catagories ##########
####################################

catagory = "All"

####################################
# -3- Result Processing
####################################

############### SPON ################

############# Grouping ##############
######### By Year, Month, Day #######

data.SPON.grouped.all = summarise(group_by(data.SPON.sentiments.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.nn = summarise(group_by(data.SPON.sentiments.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ne = summarise(group_by(data.SPON.sentiments.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.xy = summarise(group_by(data.SPON.sentiments.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ca = summarise(group_by(data.SPON.sentiments.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ad = summarise(group_by(data.SPON.sentiments.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pd = summarise(group_by(data.SPON.sentiments.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.vv = summarise(group_by(data.SPON.sentiments.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ap = summarise(group_by(data.SPON.sentiments.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pt = summarise(group_by(data.SPON.sentiments.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.fm = summarise(group_by(data.SPON.sentiments.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pi = summarise(group_by(data.SPON.sentiments.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ko = summarise(group_by(data.SPON.sentiments.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pr = summarise(group_by(data.SPON.sentiments.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pp = summarise(group_by(data.SPON.sentiments.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.SPON.grouped.all <- data.SPON.grouped.all[order(data.SPON.grouped.all$year, data.SPON.grouped.all$month, data.SPON.grouped.all$day), ]
data.SPON.grouped.nn <- data.SPON.grouped.nn[order(data.SPON.grouped.nn$year, data.SPON.grouped.nn$month, data.SPON.grouped.nn$day), ]
data.SPON.grouped.ne <- data.SPON.grouped.ne[order(data.SPON.grouped.ne$year, data.SPON.grouped.ne$month, data.SPON.grouped.ne$day), ]
data.SPON.grouped.xy <- data.SPON.grouped.xy[order(data.SPON.grouped.xy$year, data.SPON.grouped.xy$month, data.SPON.grouped.xy$day), ]
data.SPON.grouped.ca <- data.SPON.grouped.ca[order(data.SPON.grouped.ca$year, data.SPON.grouped.ca$month, data.SPON.grouped.ca$day), ]
data.SPON.grouped.ad <- data.SPON.grouped.ad[order(data.SPON.grouped.ad$year, data.SPON.grouped.ad$month, data.SPON.grouped.ad$day), ]
data.SPON.grouped.pd <- data.SPON.grouped.pd[order(data.SPON.grouped.pd$year, data.SPON.grouped.pd$month, data.SPON.grouped.pd$day), ]
data.SPON.grouped.vv <- data.SPON.grouped.vv[order(data.SPON.grouped.vv$year, data.SPON.grouped.vv$month, data.SPON.grouped.vv$day), ]
data.SPON.grouped.ap <- data.SPON.grouped.ap[order(data.SPON.grouped.ap$year, data.SPON.grouped.ap$month, data.SPON.grouped.ap$day), ]
data.SPON.grouped.pt <- data.SPON.grouped.pt[order(data.SPON.grouped.pt$year, data.SPON.grouped.pt$month, data.SPON.grouped.pt$day), ]
data.SPON.grouped.fm <- data.SPON.grouped.fm[order(data.SPON.grouped.fm$year, data.SPON.grouped.fm$month, data.SPON.grouped.fm$day), ]
data.SPON.grouped.pi <- data.SPON.grouped.pi[order(data.SPON.grouped.pi$year, data.SPON.grouped.pi$month, data.SPON.grouped.pi$day), ]
data.SPON.grouped.ko <- data.SPON.grouped.ko[order(data.SPON.grouped.ko$year, data.SPON.grouped.ko$month, data.SPON.grouped.ko$day), ]
data.SPON.grouped.pr <- data.SPON.grouped.pr[order(data.SPON.grouped.pr$year, data.SPON.grouped.pr$month, data.SPON.grouped.pr$day), ]
data.SPON.grouped.pp <- data.SPON.grouped.pp[order(data.SPON.grouped.pp$year, data.SPON.grouped.pp$month, data.SPON.grouped.pp$day), ]

############### JF ################

############# Grouping ##############
######### By Year, Month, Day #######

data.JF.grouped.all = summarise(group_by(data.JF.sentiments.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.nn = summarise(group_by(data.JF.sentiments.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ne = summarise(group_by(data.JF.sentiments.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.xy = summarise(group_by(data.JF.sentiments.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ca = summarise(group_by(data.JF.sentiments.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ad = summarise(group_by(data.JF.sentiments.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pd = summarise(group_by(data.JF.sentiments.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.vv = summarise(group_by(data.JF.sentiments.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ap = summarise(group_by(data.JF.sentiments.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pt = summarise(group_by(data.JF.sentiments.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.fm = summarise(group_by(data.JF.sentiments.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pi = summarise(group_by(data.JF.sentiments.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ko = summarise(group_by(data.JF.sentiments.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pr = summarise(group_by(data.JF.sentiments.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pp = summarise(group_by(data.JF.sentiments.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
######### By Year, Month, Day #######

data.JF.grouped.all <- data.JF.grouped.all[order(data.JF.grouped.all$year, data.JF.grouped.all$month, data.JF.grouped.all$day), ]
data.JF.grouped.nn <- data.JF.grouped.nn[order(data.JF.grouped.nn$year, data.JF.grouped.nn$month, data.JF.grouped.nn$day), ]
data.JF.grouped.ne <- data.JF.grouped.ne[order(data.JF.grouped.ne$year, data.JF.grouped.ne$month, data.JF.grouped.ne$day), ]
data.JF.grouped.xy <- data.JF.grouped.xy[order(data.JF.grouped.xy$year, data.JF.grouped.xy$month, data.JF.grouped.xy$day), ]
data.JF.grouped.ca <- data.JF.grouped.ca[order(data.JF.grouped.ca$year, data.JF.grouped.ca$month, data.JF.grouped.ca$day), ]
data.JF.grouped.ad <- data.JF.grouped.ad[order(data.JF.grouped.ad$year, data.JF.grouped.ad$month, data.JF.grouped.ad$day), ]
data.JF.grouped.pd <- data.JF.grouped.pd[order(data.JF.grouped.pd$year, data.JF.grouped.pd$month, data.JF.grouped.pd$day), ]
data.JF.grouped.vv <- data.JF.grouped.vv[order(data.JF.grouped.vv$year, data.JF.grouped.vv$month, data.JF.grouped.vv$day), ]
data.JF.grouped.ap <- data.JF.grouped.ap[order(data.JF.grouped.ap$year, data.JF.grouped.ap$month, data.JF.grouped.ap$day), ]
data.JF.grouped.pt <- data.JF.grouped.pt[order(data.JF.grouped.pt$year, data.JF.grouped.pt$month, data.JF.grouped.pt$day), ]
data.JF.grouped.fm <- data.JF.grouped.fm[order(data.JF.grouped.fm$year, data.JF.grouped.fm$month, data.JF.grouped.fm$day), ]
data.JF.grouped.pi <- data.JF.grouped.pi[order(data.JF.grouped.pi$year, data.JF.grouped.pi$month, data.JF.grouped.pi$day), ]
data.JF.grouped.ko <- data.JF.grouped.ko[order(data.JF.grouped.ko$year, data.JF.grouped.ko$month, data.JF.grouped.ko$day), ]
data.JF.grouped.pr <- data.JF.grouped.pr[order(data.JF.grouped.pr$year, data.JF.grouped.pr$month, data.JF.grouped.pr$day), ]
data.JF.grouped.pp <- data.JF.grouped.pp[order(data.JF.grouped.pp$year, data.JF.grouped.pp$month, data.JF.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(data.SPON.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/", "SPON_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### JF ################

write.csv(data.JF.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_ALL", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

#####################################
############## Politik ##############
#####################################

####################################
# -3- Result Processing
####################################

catagory = "Politik"

################ SPON ###############

############# Filtering #############
########### By Catagories ###########

data.SPON.filtered.all <- filter(data.SPON.sentiments.all, grepl(catagory, data.SPON.sentiments.all$cats))
data.SPON.filtered.nn <- filter(data.SPON.sentiments.nn, grepl(catagory, data.SPON.sentiments.nn$cats))
data.SPON.filtered.ne <- filter(data.SPON.sentiments.ne, grepl(catagory, data.SPON.sentiments.ne$cats))
data.SPON.filtered.xy <- filter(data.SPON.sentiments.xy, grepl(catagory, data.SPON.sentiments.xy$cats))
data.SPON.filtered.ca <- filter(data.SPON.sentiments.ca, grepl(catagory, data.SPON.sentiments.ca$cats))
data.SPON.filtered.ad <- filter(data.SPON.sentiments.ad, grepl(catagory, data.SPON.sentiments.ad$cats))
data.SPON.filtered.pd <- filter(data.SPON.sentiments.pd, grepl(catagory, data.SPON.sentiments.pd$cats))
data.SPON.filtered.vv <- filter(data.SPON.sentiments.vv, grepl(catagory, data.SPON.sentiments.vv$cats))
data.SPON.filtered.ap <- filter(data.SPON.sentiments.ap, grepl(catagory, data.SPON.sentiments.ap$cats))
data.SPON.filtered.pt <- filter(data.SPON.sentiments.pt, grepl(catagory, data.SPON.sentiments.pt$cats))
data.SPON.filtered.fm <- filter(data.SPON.sentiments.fm, grepl(catagory, data.SPON.sentiments.fm$cats))
data.SPON.filtered.pi <- filter(data.SPON.sentiments.pi, grepl(catagory, data.SPON.sentiments.pi$cats))
data.SPON.filtered.ko <- filter(data.SPON.sentiments.ko, grepl(catagory, data.SPON.sentiments.ko$cats))
data.SPON.filtered.pr <- filter(data.SPON.sentiments.pr, grepl(catagory, data.SPON.sentiments.pr$cats))
data.SPON.filtered.pp <- filter(data.SPON.sentiments.pp, grepl(catagory, data.SPON.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.SPON.grouped.all = summarise(group_by(data.SPON.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.nn = summarise(group_by(data.SPON.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ne = summarise(group_by(data.SPON.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.xy = summarise(group_by(data.SPON.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ca = summarise(group_by(data.SPON.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ad = summarise(group_by(data.SPON.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pd = summarise(group_by(data.SPON.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.vv = summarise(group_by(data.SPON.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ap = summarise(group_by(data.SPON.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pt = summarise(group_by(data.SPON.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.fm = summarise(group_by(data.SPON.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pi = summarise(group_by(data.SPON.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ko = summarise(group_by(data.SPON.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pr = summarise(group_by(data.SPON.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pp = summarise(group_by(data.SPON.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.SPON.grouped.all <- data.SPON.grouped.all[order(data.SPON.grouped.all$year, data.SPON.grouped.all$month, data.SPON.grouped.all$day), ]
data.SPON.grouped.nn <- data.SPON.grouped.nn[order(data.SPON.grouped.nn$year, data.SPON.grouped.nn$month, data.SPON.grouped.nn$day), ]
data.SPON.grouped.ne <- data.SPON.grouped.ne[order(data.SPON.grouped.ne$year, data.SPON.grouped.ne$month, data.SPON.grouped.ne$day), ]
data.SPON.grouped.xy <- data.SPON.grouped.xy[order(data.SPON.grouped.xy$year, data.SPON.grouped.xy$month, data.SPON.grouped.xy$day), ]
data.SPON.grouped.ca <- data.SPON.grouped.ca[order(data.SPON.grouped.ca$year, data.SPON.grouped.ca$month, data.SPON.grouped.ca$day), ]
data.SPON.grouped.ad <- data.SPON.grouped.ad[order(data.SPON.grouped.ad$year, data.SPON.grouped.ad$month, data.SPON.grouped.ad$day), ]
data.SPON.grouped.pd <- data.SPON.grouped.pd[order(data.SPON.grouped.pd$year, data.SPON.grouped.pd$month, data.SPON.grouped.pd$day), ]
data.SPON.grouped.vv <- data.SPON.grouped.vv[order(data.SPON.grouped.vv$year, data.SPON.grouped.vv$month, data.SPON.grouped.vv$day), ]
data.SPON.grouped.ap <- data.SPON.grouped.ap[order(data.SPON.grouped.ap$year, data.SPON.grouped.ap$month, data.SPON.grouped.ap$day), ]
data.SPON.grouped.pt <- data.SPON.grouped.pt[order(data.SPON.grouped.pt$year, data.SPON.grouped.pt$month, data.SPON.grouped.pt$day), ]
data.SPON.grouped.fm <- data.SPON.grouped.fm[order(data.SPON.grouped.fm$year, data.SPON.grouped.fm$month, data.SPON.grouped.fm$day), ]
data.SPON.grouped.pi <- data.SPON.grouped.pi[order(data.SPON.grouped.pi$year, data.SPON.grouped.pi$month, data.SPON.grouped.pi$day), ]
data.SPON.grouped.ko <- data.SPON.grouped.ko[order(data.SPON.grouped.ko$year, data.SPON.grouped.ko$month, data.SPON.grouped.ko$day), ]
data.SPON.grouped.pr <- data.SPON.grouped.pr[order(data.SPON.grouped.pr$year, data.SPON.grouped.pr$month, data.SPON.grouped.pr$day), ]
data.SPON.grouped.pp <- data.SPON.grouped.pp[order(data.SPON.grouped.pp$year, data.SPON.grouped.pp$month, data.SPON.grouped.pp$day), ]

############### JF ################

############# Filtering #############
########### By Catagories ###########

data.JF.filtered.all <- filter(data.JF.sentiments.all, grepl(catagory, data.JF.sentiments.all$cats))
data.JF.filtered.nn <- filter(data.JF.sentiments.nn, grepl(catagory, data.JF.sentiments.nn$cats))
data.JF.filtered.ne <- filter(data.JF.sentiments.ne, grepl(catagory, data.JF.sentiments.ne$cats))
data.JF.filtered.xy <- filter(data.JF.sentiments.xy, grepl(catagory, data.JF.sentiments.xy$cats))
data.JF.filtered.ca <- filter(data.JF.sentiments.ca, grepl(catagory, data.JF.sentiments.ca$cats))
data.JF.filtered.ad <- filter(data.JF.sentiments.ad, grepl(catagory, data.JF.sentiments.ad$cats))
data.JF.filtered.pd <- filter(data.JF.sentiments.pd, grepl(catagory, data.JF.sentiments.pd$cats))
data.JF.filtered.vv <- filter(data.JF.sentiments.vv, grepl(catagory, data.JF.sentiments.vv$cats))
data.JF.filtered.ap <- filter(data.JF.sentiments.ap, grepl(catagory, data.JF.sentiments.ap$cats))
data.JF.filtered.pt <- filter(data.JF.sentiments.pt, grepl(catagory, data.JF.sentiments.pt$cats))
data.JF.filtered.fm <- filter(data.JF.sentiments.fm, grepl(catagory, data.JF.sentiments.fm$cats))
data.JF.filtered.pi <- filter(data.JF.sentiments.pi, grepl(catagory, data.JF.sentiments.pi$cats))
data.JF.filtered.ko <- filter(data.JF.sentiments.ko, grepl(catagory, data.JF.sentiments.ko$cats))
data.JF.filtered.pr <- filter(data.JF.sentiments.pr, grepl(catagory, data.JF.sentiments.pr$cats))
data.JF.filtered.pp <- filter(data.JF.sentiments.pp, grepl(catagory, data.JF.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.JF.grouped.all = summarise(group_by(data.JF.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.nn = summarise(group_by(data.JF.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ne = summarise(group_by(data.JF.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.xy = summarise(group_by(data.JF.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ca = summarise(group_by(data.JF.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ad = summarise(group_by(data.JF.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pd = summarise(group_by(data.JF.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.vv = summarise(group_by(data.JF.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ap = summarise(group_by(data.JF.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pt = summarise(group_by(data.JF.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.fm = summarise(group_by(data.JF.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pi = summarise(group_by(data.JF.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ko = summarise(group_by(data.JF.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pr = summarise(group_by(data.JF.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pp = summarise(group_by(data.JF.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
######### By Year, Month, Day #######

data.JF.grouped.all <- data.JF.grouped.all[order(data.JF.grouped.all$year, data.JF.grouped.all$month, data.JF.grouped.all$day), ]
data.JF.grouped.nn <- data.JF.grouped.nn[order(data.JF.grouped.nn$year, data.JF.grouped.nn$month, data.JF.grouped.nn$day), ]
data.JF.grouped.ne <- data.JF.grouped.ne[order(data.JF.grouped.ne$year, data.JF.grouped.ne$month, data.JF.grouped.ne$day), ]
data.JF.grouped.xy <- data.JF.grouped.xy[order(data.JF.grouped.xy$year, data.JF.grouped.xy$month, data.JF.grouped.xy$day), ]
data.JF.grouped.ca <- data.JF.grouped.ca[order(data.JF.grouped.ca$year, data.JF.grouped.ca$month, data.JF.grouped.ca$day), ]
data.JF.grouped.ad <- data.JF.grouped.ad[order(data.JF.grouped.ad$year, data.JF.grouped.ad$month, data.JF.grouped.ad$day), ]
data.JF.grouped.pd <- data.JF.grouped.pd[order(data.JF.grouped.pd$year, data.JF.grouped.pd$month, data.JF.grouped.pd$day), ]
data.JF.grouped.vv <- data.JF.grouped.vv[order(data.JF.grouped.vv$year, data.JF.grouped.vv$month, data.JF.grouped.vv$day), ]
data.JF.grouped.ap <- data.JF.grouped.ap[order(data.JF.grouped.ap$year, data.JF.grouped.ap$month, data.JF.grouped.ap$day), ]
data.JF.grouped.pt <- data.JF.grouped.pt[order(data.JF.grouped.pt$year, data.JF.grouped.pt$month, data.JF.grouped.pt$day), ]
data.JF.grouped.fm <- data.JF.grouped.fm[order(data.JF.grouped.fm$year, data.JF.grouped.fm$month, data.JF.grouped.fm$day), ]
data.JF.grouped.pi <- data.JF.grouped.pi[order(data.JF.grouped.pi$year, data.JF.grouped.pi$month, data.JF.grouped.pi$day), ]
data.JF.grouped.ko <- data.JF.grouped.ko[order(data.JF.grouped.ko$year, data.JF.grouped.ko$month, data.JF.grouped.ko$day), ]
data.JF.grouped.pr <- data.JF.grouped.pr[order(data.JF.grouped.pr$year, data.JF.grouped.pr$month, data.JF.grouped.pr$day), ]
data.JF.grouped.pp <- data.JF.grouped.pp[order(data.JF.grouped.pp$year, data.JF.grouped.pp$month, data.JF.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(data.SPON.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### JF ################

write.csv(data.JF.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

#####################################
############## Kultur ###############
#####################################

####################################
# -3- Result Processing
####################################

catagory = "Kultur"

################ SPON ###############

############# Filtering #############
########### By Catagories ###########

data.SPON.filtered.all <- filter(data.SPON.sentiments.all, grepl(catagory, data.SPON.sentiments.all$cats))
data.SPON.filtered.nn <- filter(data.SPON.sentiments.nn, grepl(catagory, data.SPON.sentiments.nn$cats))
data.SPON.filtered.ne <- filter(data.SPON.sentiments.ne, grepl(catagory, data.SPON.sentiments.ne$cats))
data.SPON.filtered.xy <- filter(data.SPON.sentiments.xy, grepl(catagory, data.SPON.sentiments.xy$cats))
data.SPON.filtered.ca <- filter(data.SPON.sentiments.ca, grepl(catagory, data.SPON.sentiments.ca$cats))
data.SPON.filtered.ad <- filter(data.SPON.sentiments.ad, grepl(catagory, data.SPON.sentiments.ad$cats))
data.SPON.filtered.pd <- filter(data.SPON.sentiments.pd, grepl(catagory, data.SPON.sentiments.pd$cats))
data.SPON.filtered.vv <- filter(data.SPON.sentiments.vv, grepl(catagory, data.SPON.sentiments.vv$cats))
data.SPON.filtered.ap <- filter(data.SPON.sentiments.ap, grepl(catagory, data.SPON.sentiments.ap$cats))
data.SPON.filtered.pt <- filter(data.SPON.sentiments.pt, grepl(catagory, data.SPON.sentiments.pt$cats))
data.SPON.filtered.fm <- filter(data.SPON.sentiments.fm, grepl(catagory, data.SPON.sentiments.fm$cats))
data.SPON.filtered.pi <- filter(data.SPON.sentiments.pi, grepl(catagory, data.SPON.sentiments.pi$cats))
data.SPON.filtered.ko <- filter(data.SPON.sentiments.ko, grepl(catagory, data.SPON.sentiments.ko$cats))
data.SPON.filtered.pr <- filter(data.SPON.sentiments.pr, grepl(catagory, data.SPON.sentiments.pr$cats))
data.SPON.filtered.pp <- filter(data.SPON.sentiments.pp, grepl(catagory, data.SPON.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.SPON.grouped.all = summarise(group_by(data.SPON.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.nn = summarise(group_by(data.SPON.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ne = summarise(group_by(data.SPON.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.xy = summarise(group_by(data.SPON.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ca = summarise(group_by(data.SPON.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ad = summarise(group_by(data.SPON.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pd = summarise(group_by(data.SPON.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.vv = summarise(group_by(data.SPON.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ap = summarise(group_by(data.SPON.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pt = summarise(group_by(data.SPON.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.fm = summarise(group_by(data.SPON.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pi = summarise(group_by(data.SPON.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ko = summarise(group_by(data.SPON.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pr = summarise(group_by(data.SPON.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pp = summarise(group_by(data.SPON.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.SPON.grouped.all <- data.SPON.grouped.all[order(data.SPON.grouped.all$year, data.SPON.grouped.all$month, data.SPON.grouped.all$day), ]
data.SPON.grouped.nn <- data.SPON.grouped.nn[order(data.SPON.grouped.nn$year, data.SPON.grouped.nn$month, data.SPON.grouped.nn$day), ]
data.SPON.grouped.ne <- data.SPON.grouped.ne[order(data.SPON.grouped.ne$year, data.SPON.grouped.ne$month, data.SPON.grouped.ne$day), ]
data.SPON.grouped.xy <- data.SPON.grouped.xy[order(data.SPON.grouped.xy$year, data.SPON.grouped.xy$month, data.SPON.grouped.xy$day), ]
data.SPON.grouped.ca <- data.SPON.grouped.ca[order(data.SPON.grouped.ca$year, data.SPON.grouped.ca$month, data.SPON.grouped.ca$day), ]
data.SPON.grouped.ad <- data.SPON.grouped.ad[order(data.SPON.grouped.ad$year, data.SPON.grouped.ad$month, data.SPON.grouped.ad$day), ]
data.SPON.grouped.pd <- data.SPON.grouped.pd[order(data.SPON.grouped.pd$year, data.SPON.grouped.pd$month, data.SPON.grouped.pd$day), ]
data.SPON.grouped.vv <- data.SPON.grouped.vv[order(data.SPON.grouped.vv$year, data.SPON.grouped.vv$month, data.SPON.grouped.vv$day), ]
data.SPON.grouped.ap <- data.SPON.grouped.ap[order(data.SPON.grouped.ap$year, data.SPON.grouped.ap$month, data.SPON.grouped.ap$day), ]
data.SPON.grouped.pt <- data.SPON.grouped.pt[order(data.SPON.grouped.pt$year, data.SPON.grouped.pt$month, data.SPON.grouped.pt$day), ]
data.SPON.grouped.fm <- data.SPON.grouped.fm[order(data.SPON.grouped.fm$year, data.SPON.grouped.fm$month, data.SPON.grouped.fm$day), ]
data.SPON.grouped.pi <- data.SPON.grouped.pi[order(data.SPON.grouped.pi$year, data.SPON.grouped.pi$month, data.SPON.grouped.pi$day), ]
data.SPON.grouped.ko <- data.SPON.grouped.ko[order(data.SPON.grouped.ko$year, data.SPON.grouped.ko$month, data.SPON.grouped.ko$day), ]
data.SPON.grouped.pr <- data.SPON.grouped.pr[order(data.SPON.grouped.pr$year, data.SPON.grouped.pr$month, data.SPON.grouped.pr$day), ]
data.SPON.grouped.pp <- data.SPON.grouped.pp[order(data.SPON.grouped.pp$year, data.SPON.grouped.pp$month, data.SPON.grouped.pp$day), ]

############### JF ################

############# Filtering #############
########### By Catagories ###########

data.JF.filtered.all <- filter(data.JF.sentiments.all, grepl(catagory, data.JF.sentiments.all$cats))
data.JF.filtered.nn <- filter(data.JF.sentiments.nn, grepl(catagory, data.JF.sentiments.nn$cats))
data.JF.filtered.ne <- filter(data.JF.sentiments.ne, grepl(catagory, data.JF.sentiments.ne$cats))
data.JF.filtered.xy <- filter(data.JF.sentiments.xy, grepl(catagory, data.JF.sentiments.xy$cats))
data.JF.filtered.ca <- filter(data.JF.sentiments.ca, grepl(catagory, data.JF.sentiments.ca$cats))
data.JF.filtered.ad <- filter(data.JF.sentiments.ad, grepl(catagory, data.JF.sentiments.ad$cats))
data.JF.filtered.pd <- filter(data.JF.sentiments.pd, grepl(catagory, data.JF.sentiments.pd$cats))
data.JF.filtered.vv <- filter(data.JF.sentiments.vv, grepl(catagory, data.JF.sentiments.vv$cats))
data.JF.filtered.ap <- filter(data.JF.sentiments.ap, grepl(catagory, data.JF.sentiments.ap$cats))
data.JF.filtered.pt <- filter(data.JF.sentiments.pt, grepl(catagory, data.JF.sentiments.pt$cats))
data.JF.filtered.fm <- filter(data.JF.sentiments.fm, grepl(catagory, data.JF.sentiments.fm$cats))
data.JF.filtered.pi <- filter(data.JF.sentiments.pi, grepl(catagory, data.JF.sentiments.pi$cats))
data.JF.filtered.ko <- filter(data.JF.sentiments.ko, grepl(catagory, data.JF.sentiments.ko$cats))
data.JF.filtered.pr <- filter(data.JF.sentiments.pr, grepl(catagory, data.JF.sentiments.pr$cats))
data.JF.filtered.pp <- filter(data.JF.sentiments.pp, grepl(catagory, data.JF.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.JF.grouped.all = summarise(group_by(data.JF.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.nn = summarise(group_by(data.JF.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ne = summarise(group_by(data.JF.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.xy = summarise(group_by(data.JF.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ca = summarise(group_by(data.JF.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ad = summarise(group_by(data.JF.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pd = summarise(group_by(data.JF.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.vv = summarise(group_by(data.JF.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ap = summarise(group_by(data.JF.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pt = summarise(group_by(data.JF.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.fm = summarise(group_by(data.JF.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pi = summarise(group_by(data.JF.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ko = summarise(group_by(data.JF.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pr = summarise(group_by(data.JF.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pp = summarise(group_by(data.JF.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
######### By Year, Month, Day #######

data.JF.grouped.all <- data.JF.grouped.all[order(data.JF.grouped.all$year, data.JF.grouped.all$month, data.JF.grouped.all$day), ]
data.JF.grouped.nn <- data.JF.grouped.nn[order(data.JF.grouped.nn$year, data.JF.grouped.nn$month, data.JF.grouped.nn$day), ]
data.JF.grouped.ne <- data.JF.grouped.ne[order(data.JF.grouped.ne$year, data.JF.grouped.ne$month, data.JF.grouped.ne$day), ]
data.JF.grouped.xy <- data.JF.grouped.xy[order(data.JF.grouped.xy$year, data.JF.grouped.xy$month, data.JF.grouped.xy$day), ]
data.JF.grouped.ca <- data.JF.grouped.ca[order(data.JF.grouped.ca$year, data.JF.grouped.ca$month, data.JF.grouped.ca$day), ]
data.JF.grouped.ad <- data.JF.grouped.ad[order(data.JF.grouped.ad$year, data.JF.grouped.ad$month, data.JF.grouped.ad$day), ]
data.JF.grouped.pd <- data.JF.grouped.pd[order(data.JF.grouped.pd$year, data.JF.grouped.pd$month, data.JF.grouped.pd$day), ]
data.JF.grouped.vv <- data.JF.grouped.vv[order(data.JF.grouped.vv$year, data.JF.grouped.vv$month, data.JF.grouped.vv$day), ]
data.JF.grouped.ap <- data.JF.grouped.ap[order(data.JF.grouped.ap$year, data.JF.grouped.ap$month, data.JF.grouped.ap$day), ]
data.JF.grouped.pt <- data.JF.grouped.pt[order(data.JF.grouped.pt$year, data.JF.grouped.pt$month, data.JF.grouped.pt$day), ]
data.JF.grouped.fm <- data.JF.grouped.fm[order(data.JF.grouped.fm$year, data.JF.grouped.fm$month, data.JF.grouped.fm$day), ]
data.JF.grouped.pi <- data.JF.grouped.pi[order(data.JF.grouped.pi$year, data.JF.grouped.pi$month, data.JF.grouped.pi$day), ]
data.JF.grouped.ko <- data.JF.grouped.ko[order(data.JF.grouped.ko$year, data.JF.grouped.ko$month, data.JF.grouped.ko$day), ]
data.JF.grouped.pr <- data.JF.grouped.pr[order(data.JF.grouped.pr$year, data.JF.grouped.pr$month, data.JF.grouped.pr$day), ]
data.JF.grouped.pp <- data.JF.grouped.pp[order(data.JF.grouped.pp$year, data.JF.grouped.pp$month, data.JF.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(data.SPON.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### JF ################

write.csv(data.JF.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")


#####################################
############ Wirtschaft #############
#####################################

####################################
# -3- Result Processing
####################################

catagory = "Wirtschaft"

################ SPON ###############

############# Filtering #############
########### By Catagories ###########

data.SPON.filtered.all <- filter(data.SPON.sentiments.all, grepl(catagory, data.SPON.sentiments.all$cats))
data.SPON.filtered.nn <- filter(data.SPON.sentiments.nn, grepl(catagory, data.SPON.sentiments.nn$cats))
data.SPON.filtered.ne <- filter(data.SPON.sentiments.ne, grepl(catagory, data.SPON.sentiments.ne$cats))
data.SPON.filtered.xy <- filter(data.SPON.sentiments.xy, grepl(catagory, data.SPON.sentiments.xy$cats))
data.SPON.filtered.ca <- filter(data.SPON.sentiments.ca, grepl(catagory, data.SPON.sentiments.ca$cats))
data.SPON.filtered.ad <- filter(data.SPON.sentiments.ad, grepl(catagory, data.SPON.sentiments.ad$cats))
data.SPON.filtered.pd <- filter(data.SPON.sentiments.pd, grepl(catagory, data.SPON.sentiments.pd$cats))
data.SPON.filtered.vv <- filter(data.SPON.sentiments.vv, grepl(catagory, data.SPON.sentiments.vv$cats))
data.SPON.filtered.ap <- filter(data.SPON.sentiments.ap, grepl(catagory, data.SPON.sentiments.ap$cats))
data.SPON.filtered.pt <- filter(data.SPON.sentiments.pt, grepl(catagory, data.SPON.sentiments.pt$cats))
data.SPON.filtered.fm <- filter(data.SPON.sentiments.fm, grepl(catagory, data.SPON.sentiments.fm$cats))
data.SPON.filtered.pi <- filter(data.SPON.sentiments.pi, grepl(catagory, data.SPON.sentiments.pi$cats))
data.SPON.filtered.ko <- filter(data.SPON.sentiments.ko, grepl(catagory, data.SPON.sentiments.ko$cats))
data.SPON.filtered.pr <- filter(data.SPON.sentiments.pr, grepl(catagory, data.SPON.sentiments.pr$cats))
data.SPON.filtered.pp <- filter(data.SPON.sentiments.pp, grepl(catagory, data.SPON.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.SPON.grouped.all = summarise(group_by(data.SPON.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.nn = summarise(group_by(data.SPON.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ne = summarise(group_by(data.SPON.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.xy = summarise(group_by(data.SPON.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ca = summarise(group_by(data.SPON.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ad = summarise(group_by(data.SPON.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pd = summarise(group_by(data.SPON.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.vv = summarise(group_by(data.SPON.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ap = summarise(group_by(data.SPON.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pt = summarise(group_by(data.SPON.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.fm = summarise(group_by(data.SPON.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pi = summarise(group_by(data.SPON.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ko = summarise(group_by(data.SPON.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pr = summarise(group_by(data.SPON.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pp = summarise(group_by(data.SPON.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.SPON.grouped.all <- data.SPON.grouped.all[order(data.SPON.grouped.all$year, data.SPON.grouped.all$month, data.SPON.grouped.all$day), ]
data.SPON.grouped.nn <- data.SPON.grouped.nn[order(data.SPON.grouped.nn$year, data.SPON.grouped.nn$month, data.SPON.grouped.nn$day), ]
data.SPON.grouped.ne <- data.SPON.grouped.ne[order(data.SPON.grouped.ne$year, data.SPON.grouped.ne$month, data.SPON.grouped.ne$day), ]
data.SPON.grouped.xy <- data.SPON.grouped.xy[order(data.SPON.grouped.xy$year, data.SPON.grouped.xy$month, data.SPON.grouped.xy$day), ]
data.SPON.grouped.ca <- data.SPON.grouped.ca[order(data.SPON.grouped.ca$year, data.SPON.grouped.ca$month, data.SPON.grouped.ca$day), ]
data.SPON.grouped.ad <- data.SPON.grouped.ad[order(data.SPON.grouped.ad$year, data.SPON.grouped.ad$month, data.SPON.grouped.ad$day), ]
data.SPON.grouped.pd <- data.SPON.grouped.pd[order(data.SPON.grouped.pd$year, data.SPON.grouped.pd$month, data.SPON.grouped.pd$day), ]
data.SPON.grouped.vv <- data.SPON.grouped.vv[order(data.SPON.grouped.vv$year, data.SPON.grouped.vv$month, data.SPON.grouped.vv$day), ]
data.SPON.grouped.ap <- data.SPON.grouped.ap[order(data.SPON.grouped.ap$year, data.SPON.grouped.ap$month, data.SPON.grouped.ap$day), ]
data.SPON.grouped.pt <- data.SPON.grouped.pt[order(data.SPON.grouped.pt$year, data.SPON.grouped.pt$month, data.SPON.grouped.pt$day), ]
data.SPON.grouped.fm <- data.SPON.grouped.fm[order(data.SPON.grouped.fm$year, data.SPON.grouped.fm$month, data.SPON.grouped.fm$day), ]
data.SPON.grouped.pi <- data.SPON.grouped.pi[order(data.SPON.grouped.pi$year, data.SPON.grouped.pi$month, data.SPON.grouped.pi$day), ]
data.SPON.grouped.ko <- data.SPON.grouped.ko[order(data.SPON.grouped.ko$year, data.SPON.grouped.ko$month, data.SPON.grouped.ko$day), ]
data.SPON.grouped.pr <- data.SPON.grouped.pr[order(data.SPON.grouped.pr$year, data.SPON.grouped.pr$month, data.SPON.grouped.pr$day), ]
data.SPON.grouped.pp <- data.SPON.grouped.pp[order(data.SPON.grouped.pp$year, data.SPON.grouped.pp$month, data.SPON.grouped.pp$day), ]

############### JF ################

############# Filtering #############
########### By Catagories ###########

data.JF.filtered.all <- filter(data.JF.sentiments.all, grepl(catagory, data.JF.sentiments.all$cats))
data.JF.filtered.nn <- filter(data.JF.sentiments.nn, grepl(catagory, data.JF.sentiments.nn$cats))
data.JF.filtered.ne <- filter(data.JF.sentiments.ne, grepl(catagory, data.JF.sentiments.ne$cats))
data.JF.filtered.xy <- filter(data.JF.sentiments.xy, grepl(catagory, data.JF.sentiments.xy$cats))
data.JF.filtered.ca <- filter(data.JF.sentiments.ca, grepl(catagory, data.JF.sentiments.ca$cats))
data.JF.filtered.ad <- filter(data.JF.sentiments.ad, grepl(catagory, data.JF.sentiments.ad$cats))
data.JF.filtered.pd <- filter(data.JF.sentiments.pd, grepl(catagory, data.JF.sentiments.pd$cats))
data.JF.filtered.vv <- filter(data.JF.sentiments.vv, grepl(catagory, data.JF.sentiments.vv$cats))
data.JF.filtered.ap <- filter(data.JF.sentiments.ap, grepl(catagory, data.JF.sentiments.ap$cats))
data.JF.filtered.pt <- filter(data.JF.sentiments.pt, grepl(catagory, data.JF.sentiments.pt$cats))
data.JF.filtered.fm <- filter(data.JF.sentiments.fm, grepl(catagory, data.JF.sentiments.fm$cats))
data.JF.filtered.pi <- filter(data.JF.sentiments.pi, grepl(catagory, data.JF.sentiments.pi$cats))
data.JF.filtered.ko <- filter(data.JF.sentiments.ko, grepl(catagory, data.JF.sentiments.ko$cats))
data.JF.filtered.pr <- filter(data.JF.sentiments.pr, grepl(catagory, data.JF.sentiments.pr$cats))
data.JF.filtered.pp <- filter(data.JF.sentiments.pp, grepl(catagory, data.JF.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.JF.grouped.all = summarise(group_by(data.JF.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.nn = summarise(group_by(data.JF.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ne = summarise(group_by(data.JF.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.xy = summarise(group_by(data.JF.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ca = summarise(group_by(data.JF.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ad = summarise(group_by(data.JF.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pd = summarise(group_by(data.JF.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.vv = summarise(group_by(data.JF.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ap = summarise(group_by(data.JF.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pt = summarise(group_by(data.JF.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.fm = summarise(group_by(data.JF.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pi = summarise(group_by(data.JF.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ko = summarise(group_by(data.JF.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pr = summarise(group_by(data.JF.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pp = summarise(group_by(data.JF.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
######### By Year, Month, Day #######

data.JF.grouped.all <- data.JF.grouped.all[order(data.JF.grouped.all$year, data.JF.grouped.all$month, data.JF.grouped.all$day), ]
data.JF.grouped.nn <- data.JF.grouped.nn[order(data.JF.grouped.nn$year, data.JF.grouped.nn$month, data.JF.grouped.nn$day), ]
data.JF.grouped.ne <- data.JF.grouped.ne[order(data.JF.grouped.ne$year, data.JF.grouped.ne$month, data.JF.grouped.ne$day), ]
data.JF.grouped.xy <- data.JF.grouped.xy[order(data.JF.grouped.xy$year, data.JF.grouped.xy$month, data.JF.grouped.xy$day), ]
data.JF.grouped.ca <- data.JF.grouped.ca[order(data.JF.grouped.ca$year, data.JF.grouped.ca$month, data.JF.grouped.ca$day), ]
data.JF.grouped.ad <- data.JF.grouped.ad[order(data.JF.grouped.ad$year, data.JF.grouped.ad$month, data.JF.grouped.ad$day), ]
data.JF.grouped.pd <- data.JF.grouped.pd[order(data.JF.grouped.pd$year, data.JF.grouped.pd$month, data.JF.grouped.pd$day), ]
data.JF.grouped.vv <- data.JF.grouped.vv[order(data.JF.grouped.vv$year, data.JF.grouped.vv$month, data.JF.grouped.vv$day), ]
data.JF.grouped.ap <- data.JF.grouped.ap[order(data.JF.grouped.ap$year, data.JF.grouped.ap$month, data.JF.grouped.ap$day), ]
data.JF.grouped.pt <- data.JF.grouped.pt[order(data.JF.grouped.pt$year, data.JF.grouped.pt$month, data.JF.grouped.pt$day), ]
data.JF.grouped.fm <- data.JF.grouped.fm[order(data.JF.grouped.fm$year, data.JF.grouped.fm$month, data.JF.grouped.fm$day), ]
data.JF.grouped.pi <- data.JF.grouped.pi[order(data.JF.grouped.pi$year, data.JF.grouped.pi$month, data.JF.grouped.pi$day), ]
data.JF.grouped.ko <- data.JF.grouped.ko[order(data.JF.grouped.ko$year, data.JF.grouped.ko$month, data.JF.grouped.ko$day), ]
data.JF.grouped.pr <- data.JF.grouped.pr[order(data.JF.grouped.pr$year, data.JF.grouped.pr$month, data.JF.grouped.pr$day), ]
data.JF.grouped.pp <- data.JF.grouped.pp[order(data.JF.grouped.pp$year, data.JF.grouped.pp$month, data.JF.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(data.SPON.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### JF ################

write.csv(data.JF.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

#####################################
############## Meinung ##############
#####################################

####################################
# -3- Result Processing
####################################

catagory = "Meinung"

################ SPON ###############

############# Filtering #############
########### By Catagories ###########

data.SPON.filtered.all <- filter(data.SPON.sentiments.all, grepl(catagory, data.SPON.sentiments.all$cats))
data.SPON.filtered.nn <- filter(data.SPON.sentiments.nn, grepl(catagory, data.SPON.sentiments.nn$cats))
data.SPON.filtered.ne <- filter(data.SPON.sentiments.ne, grepl(catagory, data.SPON.sentiments.ne$cats))
data.SPON.filtered.xy <- filter(data.SPON.sentiments.xy, grepl(catagory, data.SPON.sentiments.xy$cats))
data.SPON.filtered.ca <- filter(data.SPON.sentiments.ca, grepl(catagory, data.SPON.sentiments.ca$cats))
data.SPON.filtered.ad <- filter(data.SPON.sentiments.ad, grepl(catagory, data.SPON.sentiments.ad$cats))
data.SPON.filtered.pd <- filter(data.SPON.sentiments.pd, grepl(catagory, data.SPON.sentiments.pd$cats))
data.SPON.filtered.vv <- filter(data.SPON.sentiments.vv, grepl(catagory, data.SPON.sentiments.vv$cats))
data.SPON.filtered.ap <- filter(data.SPON.sentiments.ap, grepl(catagory, data.SPON.sentiments.ap$cats))
data.SPON.filtered.pt <- filter(data.SPON.sentiments.pt, grepl(catagory, data.SPON.sentiments.pt$cats))
data.SPON.filtered.fm <- filter(data.SPON.sentiments.fm, grepl(catagory, data.SPON.sentiments.fm$cats))
data.SPON.filtered.pi <- filter(data.SPON.sentiments.pi, grepl(catagory, data.SPON.sentiments.pi$cats))
data.SPON.filtered.ko <- filter(data.SPON.sentiments.ko, grepl(catagory, data.SPON.sentiments.ko$cats))
data.SPON.filtered.pr <- filter(data.SPON.sentiments.pr, grepl(catagory, data.SPON.sentiments.pr$cats))
data.SPON.filtered.pp <- filter(data.SPON.sentiments.pp, grepl(catagory, data.SPON.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.SPON.grouped.all = summarise(group_by(data.SPON.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.nn = summarise(group_by(data.SPON.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ne = summarise(group_by(data.SPON.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.xy = summarise(group_by(data.SPON.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ca = summarise(group_by(data.SPON.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ad = summarise(group_by(data.SPON.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pd = summarise(group_by(data.SPON.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.vv = summarise(group_by(data.SPON.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ap = summarise(group_by(data.SPON.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pt = summarise(group_by(data.SPON.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.fm = summarise(group_by(data.SPON.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pi = summarise(group_by(data.SPON.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.ko = summarise(group_by(data.SPON.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pr = summarise(group_by(data.SPON.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.SPON.grouped.pp = summarise(group_by(data.SPON.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
########### By Day and Time #########

data.SPON.grouped.all <- data.SPON.grouped.all[order(data.SPON.grouped.all$year, data.SPON.grouped.all$month, data.SPON.grouped.all$day), ]
data.SPON.grouped.nn <- data.SPON.grouped.nn[order(data.SPON.grouped.nn$year, data.SPON.grouped.nn$month, data.SPON.grouped.nn$day), ]
data.SPON.grouped.ne <- data.SPON.grouped.ne[order(data.SPON.grouped.ne$year, data.SPON.grouped.ne$month, data.SPON.grouped.ne$day), ]
data.SPON.grouped.xy <- data.SPON.grouped.xy[order(data.SPON.grouped.xy$year, data.SPON.grouped.xy$month, data.SPON.grouped.xy$day), ]
data.SPON.grouped.ca <- data.SPON.grouped.ca[order(data.SPON.grouped.ca$year, data.SPON.grouped.ca$month, data.SPON.grouped.ca$day), ]
data.SPON.grouped.ad <- data.SPON.grouped.ad[order(data.SPON.grouped.ad$year, data.SPON.grouped.ad$month, data.SPON.grouped.ad$day), ]
data.SPON.grouped.pd <- data.SPON.grouped.pd[order(data.SPON.grouped.pd$year, data.SPON.grouped.pd$month, data.SPON.grouped.pd$day), ]
data.SPON.grouped.vv <- data.SPON.grouped.vv[order(data.SPON.grouped.vv$year, data.SPON.grouped.vv$month, data.SPON.grouped.vv$day), ]
data.SPON.grouped.ap <- data.SPON.grouped.ap[order(data.SPON.grouped.ap$year, data.SPON.grouped.ap$month, data.SPON.grouped.ap$day), ]
data.SPON.grouped.pt <- data.SPON.grouped.pt[order(data.SPON.grouped.pt$year, data.SPON.grouped.pt$month, data.SPON.grouped.pt$day), ]
data.SPON.grouped.fm <- data.SPON.grouped.fm[order(data.SPON.grouped.fm$year, data.SPON.grouped.fm$month, data.SPON.grouped.fm$day), ]
data.SPON.grouped.pi <- data.SPON.grouped.pi[order(data.SPON.grouped.pi$year, data.SPON.grouped.pi$month, data.SPON.grouped.pi$day), ]
data.SPON.grouped.ko <- data.SPON.grouped.ko[order(data.SPON.grouped.ko$year, data.SPON.grouped.ko$month, data.SPON.grouped.ko$day), ]
data.SPON.grouped.pr <- data.SPON.grouped.pr[order(data.SPON.grouped.pr$year, data.SPON.grouped.pr$month, data.SPON.grouped.pr$day), ]
data.SPON.grouped.pp <- data.SPON.grouped.pp[order(data.SPON.grouped.pp$year, data.SPON.grouped.pp$month, data.SPON.grouped.pp$day), ]

############### JF ################

############# Filtering #############
########### By Catagories ###########

data.JF.filtered.all <- filter(data.JF.sentiments.all, grepl(catagory, data.JF.sentiments.all$cats))
data.JF.filtered.nn <- filter(data.JF.sentiments.nn, grepl(catagory, data.JF.sentiments.nn$cats))
data.JF.filtered.ne <- filter(data.JF.sentiments.ne, grepl(catagory, data.JF.sentiments.ne$cats))
data.JF.filtered.xy <- filter(data.JF.sentiments.xy, grepl(catagory, data.JF.sentiments.xy$cats))
data.JF.filtered.ca <- filter(data.JF.sentiments.ca, grepl(catagory, data.JF.sentiments.ca$cats))
data.JF.filtered.ad <- filter(data.JF.sentiments.ad, grepl(catagory, data.JF.sentiments.ad$cats))
data.JF.filtered.pd <- filter(data.JF.sentiments.pd, grepl(catagory, data.JF.sentiments.pd$cats))
data.JF.filtered.vv <- filter(data.JF.sentiments.vv, grepl(catagory, data.JF.sentiments.vv$cats))
data.JF.filtered.ap <- filter(data.JF.sentiments.ap, grepl(catagory, data.JF.sentiments.ap$cats))
data.JF.filtered.pt <- filter(data.JF.sentiments.pt, grepl(catagory, data.JF.sentiments.pt$cats))
data.JF.filtered.fm <- filter(data.JF.sentiments.fm, grepl(catagory, data.JF.sentiments.fm$cats))
data.JF.filtered.pi <- filter(data.JF.sentiments.pi, grepl(catagory, data.JF.sentiments.pi$cats))
data.JF.filtered.ko <- filter(data.JF.sentiments.ko, grepl(catagory, data.JF.sentiments.ko$cats))
data.JF.filtered.pr <- filter(data.JF.sentiments.pr, grepl(catagory, data.JF.sentiments.pr$cats))
data.JF.filtered.pp <- filter(data.JF.sentiments.pp, grepl(catagory, data.JF.sentiments.pp$cats))

############# Grouping ##############
######### By Year, Month, Day #######

data.JF.grouped.all = summarise(group_by(data.JF.filtered.all, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.nn = summarise(group_by(data.JF.filtered.nn, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ne = summarise(group_by(data.JF.filtered.ne, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.xy = summarise(group_by(data.JF.filtered.xy, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ca = summarise(group_by(data.JF.filtered.ca, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ad = summarise(group_by(data.JF.filtered.ad, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pd = summarise(group_by(data.JF.filtered.pd, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.vv = summarise(group_by(data.JF.filtered.vv, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ap = summarise(group_by(data.JF.filtered.ap, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pt = summarise(group_by(data.JF.filtered.pt, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.fm = summarise(group_by(data.JF.filtered.fm, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pi = summarise(group_by(data.JF.filtered.pi, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.ko = summarise(group_by(data.JF.filtered.ko, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pr = summarise(group_by(data.JF.filtered.pr, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))
data.JF.grouped.pp = summarise(group_by(data.JF.filtered.pp, year, month, day), positiv_abs = sum(positiv_abs), neutral_abs = sum(neutral_abs), negativ_abs = sum(negativ_abs), positiv_rel = mean(positiv_rel), neutral_rel = mean(neutral_rel), negativ_rel = mean(negativ_rel))

############# Ordering ##############
######### By Year, Month, Day #######

data.JF.grouped.all <- data.JF.grouped.all[order(data.JF.grouped.all$year, data.JF.grouped.all$month, data.JF.grouped.all$day), ]
data.JF.grouped.nn <- data.JF.grouped.nn[order(data.JF.grouped.nn$year, data.JF.grouped.nn$month, data.JF.grouped.nn$day), ]
data.JF.grouped.ne <- data.JF.grouped.ne[order(data.JF.grouped.ne$year, data.JF.grouped.ne$month, data.JF.grouped.ne$day), ]
data.JF.grouped.xy <- data.JF.grouped.xy[order(data.JF.grouped.xy$year, data.JF.grouped.xy$month, data.JF.grouped.xy$day), ]
data.JF.grouped.ca <- data.JF.grouped.ca[order(data.JF.grouped.ca$year, data.JF.grouped.ca$month, data.JF.grouped.ca$day), ]
data.JF.grouped.ad <- data.JF.grouped.ad[order(data.JF.grouped.ad$year, data.JF.grouped.ad$month, data.JF.grouped.ad$day), ]
data.JF.grouped.pd <- data.JF.grouped.pd[order(data.JF.grouped.pd$year, data.JF.grouped.pd$month, data.JF.grouped.pd$day), ]
data.JF.grouped.vv <- data.JF.grouped.vv[order(data.JF.grouped.vv$year, data.JF.grouped.vv$month, data.JF.grouped.vv$day), ]
data.JF.grouped.ap <- data.JF.grouped.ap[order(data.JF.grouped.ap$year, data.JF.grouped.ap$month, data.JF.grouped.ap$day), ]
data.JF.grouped.pt <- data.JF.grouped.pt[order(data.JF.grouped.pt$year, data.JF.grouped.pt$month, data.JF.grouped.pt$day), ]
data.JF.grouped.fm <- data.JF.grouped.fm[order(data.JF.grouped.fm$year, data.JF.grouped.fm$month, data.JF.grouped.fm$day), ]
data.JF.grouped.pi <- data.JF.grouped.pi[order(data.JF.grouped.pi$year, data.JF.grouped.pi$month, data.JF.grouped.pi$day), ]
data.JF.grouped.ko <- data.JF.grouped.ko[order(data.JF.grouped.ko$year, data.JF.grouped.ko$month, data.JF.grouped.ko$day), ]
data.JF.grouped.pr <- data.JF.grouped.pr[order(data.JF.grouped.pr$year, data.JF.grouped.pr$month, data.JF.grouped.pr$day), ]
data.JF.grouped.pp <- data.JF.grouped.pp[order(data.JF.grouped.pp$year, data.JF.grouped.pp$month, data.JF.grouped.pp$day), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(data.SPON.grouped.all, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_All", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.SPON.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/SPON/", catagory, "/",  "SPON_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### JF ################

write.csv(data.JF.grouped.nn, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NN", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ne, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_NE", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.xy, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_XY", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ca, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_CA", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ad, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pd, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PD", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.vv, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_VV", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ap, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_AP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pt, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PT", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.fm, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_FM", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pi, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PI", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.ko, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_KO", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pr, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PR", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(data.JF.grouped.pp, file=paste0("/share/Ergebnisse/Sentiment/JF", catagory, "/",  "JF_PP", ".csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")


#data.filtered.wirtschaft <- filter(data.SPON.sentiments.all, grepl("Wirtschaft", data.sentiments.all$cats))
#data.filtered.ausland <- filter(data.SPON.sentiments.all, grepl("Ausland", data.sentiments.all$cats))
#data.filtered.nahostkonflikt <- filter(data.SPON.sentiments.all, grepl("Nahostkonflikt", data.sentiments.all$cats))
