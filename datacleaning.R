install.packages("dplyr")
install.packages("gdata")

library("dplyr")
library("gdata")

file.1 = read.csv("/share/Archive/Sponcrawler2016-12-13%2017%3A03%3A02.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
file.2 = read.csv("/share/Archive/sponcrawler2016-11-28%2023%3A51%3A37.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
file.3 = read.csv("/share/Archive/sponcrawler2016-11-29%2009%3A07%3A20.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
file.4 = read.csv("/share/Archive/sponcrawler2016-11-29%2016%3A14%3A41.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
file.5 = read.csv("/share/Archive/sponcrawler2016-11-30%2017%3A34%3A30.csv", encoding="UTF-8", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

attach(file.1)
attach(file.2)
attach(file.3)
attach(file.4)
attach(file.5)

all.files = rbind(file.1, file.2, file.3, file.4, file.5)
clean.data = allfiles %>% distinct(headline, .keep_all = TRUE)
write.csv(clean.data, file="/share/Archive/SPON_complete")
