data.SPON = read.csv("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/SPON/All/SPON_All.csv")
data.SPON = read.csv("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/BILD/All/BILD_All.csv")
data.JF = read.csv("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/JF/All/JF_ALL.csv")

data = rbind(data.SPON, data.BILD, data.JF)
data = filter(data, year > 2000)

data$year <- as.character(data$year)
data$month <- as.character(data$month)
data$day <- as.character(data$day)

data <- filter(data, year != "NA", month != "NA", day != "NA")

write.csv(data, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/rel_sentiment_val_2001_2016.csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")

#data$date <- paste(data$day, ".", data$month, ".", data$year, sep = '')
#data.date <- as.Date(data$date, format="%d.%m.%Y")

data$date = as.Date(paste0("01.", data$month, ".", data$year), format="%d.%m.%Y")

#data$month <- laply(data$month, function(month) {
#  res.month <- month
#  if (stri_length(month) == 1) res.month <- paste0("0", month)
#  return (res.month)
#})

#data$date <- laply(data$date, function(date) {
#  year <- unlist(str_split(date, '\\.'))[1]
#  month <- unlist(str_split(date, '\\.'))[2]
#  res <- paste0(year, ".", unlist(str_split(as.character((1000000/12) * (as.numeric(month) - 1)), '\\.'))[1])
#  return (res)
#})

#data$date <- paste0(data$year, ".", unlist(str_split(as.character((1000000/12) * (as.numeric(data$month) - 1)), '\\.'))[1])
#data$date <- unlist(str_split(as.character((1000000/12) * (as.numeric(data$month) - 1)), '\\.'))[1]
#data$date <- as.numeric(data$date)

data.agg <- summarise(group_by(data, date), count = n(), positiv_rel = sum(positiv_rel)/n(), neutral_rel = sum(neutral_rel)/n(), negativ_rel = sum(negativ_rel)/n())

neu.plot <- ggplot(data.agg, aes(date, neutral_rel)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth() +
    scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/rel_neutral_plot_2001_2016.jpg", neu.plot)

neg.plot <- ggplot(data.agg, aes(date, negativ_rel)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth(color = "red") +
    scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/rel_negativ_plot_2001_2016.jpg", neg.plot)

pos.plot <- ggplot(data.agg, aes(date, positiv_rel)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth(color = "green") +
    scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/rel_positiv_plot_2001_2016.jpg", pos.plot)

######### DELTA ##########

data.delta = data.frame(delta_positiv = 0, delta_neutral = 0, delta_negativ = 0, year=0, month=0, day=0, date=as.Date("01-01-2001", format="%d.%m.%Y"))

for(i in 1:(length(data$date)-1)) {
      tmp.delta <- data.frame(delta_positiv = sqrt((data$positiv_rel[i+1] - data$positiv_rel[i])^2) , delta_neutral = sqrt((data$neutral_rel[i+1] - data$neutral_rel[i])^2), delta_negativ = sqrt((data$negativ_rel[i+1] - data$negativ_rel[i])^2),  year=data$year[i+1], month=data$month[i+1], day=data$day[i+1], date=data$date[i+1])
      data.delta <- rbind(data.delta, tmp.delta)
}

data.delta <- filter(data.delta, day > 0)

write.csv(data.delta, file=paste0("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/delta_sentiment_val_2001_2016.csv"), sep=",", row.names=TRUE, fileEncoding = "UTF-8")

data.delta.agg <- summarise(group_by(data.delta, date), count = n(), delta_positiv = sum(delta_positiv)/n(), delta_neutral = sum(delta_neutral)/n(), delta_negativ = sum(delta_negativ)/n())

delta.neg.plot <- ggplot(data.delta.agg, aes(date, delta_negativ)) +
     geom_point(aes(size = count), alpha = 1/2) +
     geom_smooth(color = "red") +
     scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/delta_negativ_plot_2001_2016.jpg", delta.neg.plot)

delta.neu.plot <- ggplot(data.delta.agg, aes(date, delta_neutral)) +
     geom_point(aes(size = count), alpha = 1/2) +
     geom_smooth() +
     scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/delta_neutral_plot_2001_2016.jpg", delta.neu.plot)

delta.pos.plot <- ggplot(data.delta.agg, aes(date, delta_positiv)) +
     geom_point(aes(size = count), alpha = 1/2) +
     geom_smooth(color = "green") +
     scale_size_area()

ggsave("/Users/admin/Desktop/GSN/Ergebnisse/Sentiment/All/All/delta_positiv_plot_2001_2016.jpg", delta.pos.plot)
