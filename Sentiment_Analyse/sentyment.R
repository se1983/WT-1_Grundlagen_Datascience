install.packages("stringr")
install.packages("plyr")
install.pachage("dplyr")

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

  sentiments.df = data.frame(positiv=sentiments[ ,1], neutral=sentiments[ ,2], negativ=sentiments[ ,3], positiv.cum=sentiments[ ,4], neutral.cum=sentiments[ ,5], negativ.cum=sentiments[ ,6], headline=data$headline, weekday=data$weekday, day=data$day, month=data$month, year=data$year, time=data$time, cats=data$cats)
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

######### News Paper Files ##########

############### SPON ################

# Load CSV File
data.SPON <- read.csv("/Users/admin/Desktop/GSN/Daten/SPON_complete.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data.SPON$day <- as.Date(data.SPON$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data.SPON$year = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[3])
data.SPON$month = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[2])
data.SPON$day = laply(data.SPON$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert String to factor
data.SPON$article<-as.factor(data.SPON$article)

############### JF ################

# Load CSV File
data.JF <- read.csv("/Users/admin/Desktop/GSN/Daten/jungefreiheit.csv", encoding="UTF-16LE", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

# Convert String to Date
# data.JF$day <- as.Date(data.JF$day, format="%d.%m.%Y")

# Split String into Day, Month, Year
data.JF$year = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[3])
data.JF$month = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[2])
data.JF$day = laply(data.JF$day, function(date) unlist(str_split(date, '\\.'))[1])

# Convert String to factor
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
# -3- Result Processing
####################################

############### SPON ################

############# Filtering #############
########### By Catagories ###########

catagory = "Politik"
data.SPON.filtered <- filter(data.SPON.sentiments.all, grepl(catagory, data.SPON.sentiments.all$cats))

#data.filtered.wirtschaft <- filter(data.SPON.sentiments.all, grepl("Wirtschaft", data.sentiments.all$cats))
#data.filtered.ausland <- filter(data.SPON.sentiments.all, grepl("Ausland", data.sentiments.all$cats))
#data.filtered.nahostkonflikt <- filter(data.SPON.sentiments.all, grepl("Nahostkonflikt", data.sentiments.all$cats))

############# Grouping ##############
######### By Year, Month, Day #######

all.SPON.grouped = summarise(group_by(data.SPON.sentiments.all, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
nn.SPON.grouped = summarise(group_by(data.SPON.sentiments.nn, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ne.SPON.grouped = summarise(group_by(data.SPON.sentiments.ne, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
xy.SPON.grouped = summarise(group_by(data.SPON.sentiments.xy, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ca.SPON.grouped = summarise(group_by(data.SPON.sentiments.ca, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ad.SPON.grouped = summarise(group_by(data.SPON.sentiments.ad, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pd.SPON.grouped = summarise(group_by(data.SPON.sentiments.pd, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
vv.SPON.grouped = summarise(group_by(data.SPON.sentiments.vv, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ap.SPON.grouped = summarise(group_by(data.SPON.sentiments.ap, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pt.SPON.grouped = summarise(group_by(data.SPON.sentiments.pt, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
fm.SPON.grouped = summarise(group_by(data.SPON.sentiments.fm, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pi.SPON.grouped = summarise(group_by(data.SPON.sentiments.pi, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ko.SPON.grouped = summarise(group_by(data.SPON.sentiments.ko, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pr.SPON.grouped = summarise(group_by(data.SPON.sentiments.pr, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pp.SPON.grouped = summarise(group_by(data.SPON.sentiments.pp, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))

############# Ordering ##############
########### By Day and Time #########

all.SPON.grouped <- all.SPON.grouped[order(all.SPON.grouped$day, all.SPON.grouped$time), ]
nn.SPON.grouped <- nn.SPON.grouped[order(nn.SPON.grouped$day, nn.SPON.grouped$time), ]
ne.SPON.grouped <- ne.SPON.grouped[order(ne.SPON.grouped$day, ne.SPON.grouped$time), ]
xy.SPON.grouped <- xy.SPON.grouped[order(xy.SPON.grouped$day, xy.SPON.grouped$time), ]
ca.SPON.grouped <- ca.SPON.grouped[order(ca.SPON.grouped$day, ca.SPON.grouped$time), ]
ad.SPON.grouped <- ad.SPON.grouped[order(ad.SPON.grouped$day, ad.SPON.grouped$time), ]
pd.SPON.grouped <- pd.SPON.grouped[order(pd.SPON.grouped$day, pd.SPON.grouped$time), ]
vv.SPON.grouped <- vv.SPON.grouped[order(vv.SPON.grouped$day, vv.SPON.grouped$time), ]
ap.SPON.grouped <- ap.SPON.grouped[order(ap.SPON.grouped$day, ap.SPON.grouped$time), ]
pt.SPON.grouped <- pt.SPON.grouped[order(pt.SPON.grouped$day, pt.SPON.grouped$time), ]
fm.SPON.grouped <- fm.SPON.grouped[order(fm.SPON.grouped$day, fm.SPON.grouped$time), ]
pi.SPON.grouped <- pi.SPON.grouped[order(pi.SPON.grouped$day, pi.SPON.grouped$time), ]
ko.SPON.grouped <- ko.SPON.grouped[order(ko.SPON.grouped$day, ko.SPON.grouped$time), ]
pr.SPON.grouped <- pr.SPON.grouped[order(pr.SPON.grouped$day, pr.SPON.grouped$time), ]
pp.SPON.grouped <- pp.SPON.grouped[order(pp.SPON.grouped$day, pp.SPON.grouped$time), ]

############### FJ ################

############# Filtering #############
########### By Catagories ###########

data.FJ.filtered <- filter(data.FJ.sentiments.all, grepl(catagory, data.FJ.sentiments.all$cats))

#data.filtered.wirtschaft <- filter(data.FJ.sentiments.all, grepl("Wirtschaft", data.sentiments.all$cats))
#data.filtered.ausland <- filter(data.FJ.sentiments.all, grepl("Ausland", data.sentiments.all$cats))
#data.filtered.nahostkonflikt <- filter(data.FJ.sentiments.all, grepl("Nahostkonflikt", data.sentiments.all$cats))

############# Grouping ##############
######### By Year, Month, Day #######

all.FJ.grouped = summarise(group_by(data.FJ.sentiments.all, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
nn.FJ.grouped = summarise(group_by(data.FJ.sentiments.nn, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ne.FJ.grouped = summarise(group_by(data.FJ.sentiments.ne, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
xy.FJ.grouped = summarise(group_by(data.FJ.sentiments.xy, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ca.FJ.grouped = summarise(group_by(data.FJ.sentiments.ca, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ad.FJ.grouped = summarise(group_by(data.FJ.sentiments.ad, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pd.FJ.grouped = summarise(group_by(data.FJ.sentiments.pd, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
vv.FJ.grouped = summarise(group_by(data.FJ.sentiments.vv, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ap.FJ.grouped = summarise(group_by(data.FJ.sentiments.ap, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pt.FJ.grouped = summarise(group_by(data.FJ.sentiments.pt, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
fm.FJ.grouped = summarise(group_by(data.FJ.sentiments.fm, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pi.FJ.grouped = summarise(group_by(data.FJ.sentiments.pi, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
ko.FJ.grouped = summarise(group_by(data.FJ.sentiments.ko, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pr.FJ.grouped = summarise(group_by(data.FJ.sentiments.pr, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))
pp.FJ.grouped = summarise(group_by(data.FJ.sentiments.pp, year, month, day), positiv = sum(positiv), neutral = sum(neutral), negativ = sum(negativ))

############# Ordering ##############
########### By Day and Time #########

all.FJ.grouped <- all.FJ.grouped[order(all.FJ.grouped$day, all.FJ.grouped$time), ]
nn.FJ.grouped <- nn.FJ.grouped[order(nn.FJ.grouped$day, nn.FJ.grouped$time), ]
ne.FJ.grouped <- ne.FJ.grouped[order(ne.FJ.grouped$day, ne.FJ.grouped$time), ]
xy.FJ.grouped <- xy.FJ.grouped[order(xy.FJ.grouped$day, xy.FJ.grouped$time), ]
ca.FJ.grouped <- ca.FJ.grouped[order(ca.FJ.grouped$day, ca.FJ.grouped$time), ]
ad.FJ.grouped <- ad.FJ.grouped[order(ad.FJ.grouped$day, ad.FJ.grouped$time), ]
pd.FJ.grouped <- pd.FJ.grouped[order(pd.FJ.grouped$day, pd.FJ.grouped$time), ]
vv.FJ.grouped <- vv.FJ.grouped[order(vv.FJ.grouped$day, vv.FJ.grouped$time), ]
ap.FJ.grouped <- ap.FJ.grouped[order(ap.FJ.grouped$day, ap.FJ.grouped$time), ]
pt.FJ.grouped <- pt.FJ.grouped[order(pt.FJ.grouped$day, pt.FJ.grouped$time), ]
fm.FJ.grouped <- fm.FJ.grouped[order(fm.FJ.grouped$day, fm.FJ.grouped$time), ]
pi.FJ.grouped <- pi.FJ.grouped[order(pi.FJ.grouped$day, pi.FJ.grouped$time), ]
ko.FJ.grouped <- ko.FJ.grouped[order(ko.FJ.grouped$day, ko.FJ.grouped$time), ]
pr.FJ.grouped <- pr.FJ.grouped[order(pr.FJ.grouped$day, pr.FJ.grouped$time), ]
pp.FJ.grouped <- pp.FJ.grouped[order(pp.FJ.grouped$day, pp.FJ.grouped$time), ]

####################################
# -4- Saving Result
####################################

############### SPON ################

write.csv(all.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "All", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(nn.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "NN", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ne.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "NE", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(xy.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "XY", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ca.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "CA", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ad.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "AD", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pd.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "PD", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(vv.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "VV", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ap.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "AP", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pt.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "PT", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(fm.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "FM", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pi.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "PI", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ko.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "KO", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pr.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "PR", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pp.SPON.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/SPON/", "PP", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")

############### FJ ################

write.csv(all.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "All", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(nn.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "NN", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ne.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "NE", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(xy.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "XY", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ca.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "CA", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ad.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "AD", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pd.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "PD", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(vv.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "VV", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ap.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "AP", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pt.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "PT", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(fm.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "FM", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pi.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "PI", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(ko.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "KO", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pr.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "PR", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
write.csv(pp.FJ.grouped, file=paste("/Users/admin/Desktop/GSN/Sentimentanalyse/sentiment/FJ/", "PP", catagory), sep=",", row.names=TRUE, fileEncoding = "UTF-16LE")
