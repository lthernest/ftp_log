# set the directory first! Move the ftp_log.txt to the folder


# fill = TRUE - fill in empty spaces
log_data <- read.table("ftp_log.txt", header = FALSE, fill = TRUE)

# find how many entries
dim(log_data)

# add the header names
colnames(log_data) <- c("week", "mm", "dd", "tt", "yy", "speed","ip","size", "doi", "NA", "NA", "NA", "NA","NA", "NA","NA", "NA","fin")


yy <- log_data$yy
dd <- log_data$dd
# convert the month to number
mm <- log_data$mm
mm <- match(mm,month.abb)
ip <- log_data$ip
fin <- log_data$fin


# merging year, month and day, display with the date format
dates <-as.Date(paste(yy, mm, dd, sep="-"), format="%Y-%m-%d")

# date of the first file download, remove NA value
min(dates, na.rm=TRUE)
# date of the last file download, remove NA value
max(dates, na.rm=TRUE)

# create a function to count no of row per month
count <-function(month,year){
# sum(mm_factor =='Feb')
# change the value to factor for counting
  mm_factor <- factor(log_data$mm)
  count_month <- sum(mm_factor == month & yy==year)
  return(count_month)
}


# data between 2017 and 2018

y_2018 <- c(count('Jan', 2018), count('Feb', 2018), count('Mar', 2018), count('Apr', 2018), count('May', 2018), count('Jun', 2018), count('Jul',2018), count('Aug',2018), count('Sep',2018), count('Oct',2018), count('Nov',2018), count('Dec',2018))

y_2017 <- c(count('Jan', 2017), count('Feb', 2017), count('Mar', 2017), count('Apr', 2017), count('May', 2017), count('Jun', 2017), count('Jul',2017), count('Aug',2017), count('Sep',2017), count('Oct',2017), count('Nov',2017), count('Dec',2017))

# assign the label of x-axis
x <- rep(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

# to keep the order of x-axis in data.frame
x1 <- factor(x, levels=x)

values <- c(y_2017, y_2018)
type <-c(rep("2017", 12), rep("2018", 12))
dl <- data.frame(x1,values)

# load in `ggplot2`
library(ggplot2)

p <-ggplot(dl, aes(x1, values)) + geom_bar(stat = "identity", aes(fill = type)) + xlab("Month") + ylab("Count") + ggtitle("Number of files downloaded per month") + theme_bw()
p

# to search if ftp_log contain parrot ip address '192.168.44.247'
'192.168.44.247' %in% ip

# to exclude all the incompete download, i.e. fin = "i"
complete_log_data <- subset(log_data, fin!="i")

#########try to work with APIs in R#############
#install.packages("httr")
#require("httr")

#install.packages("jsonlite")
#require("jsonlite")

# location field parameters
#base <- "https://ipapi.co"
#call1 <- paste(base,ip,"country", sep="/")
#get_country <- GET(call1)
############################################


# to generate country names of ip
install.packages("rgeolocate")
library(rgeolocate)

ipmmdb <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
ip_country <- maxmind(complete_log_data$ip, ipmmdb,"country_name")
country_list <- data.frame(table(ip_country))
ggplot(country_list, aes(x=ip_country, y=Freq)) + geom_bar(stat="identity") + labs(x="Country", y="Frequency")

# map visualization
install.packages("rworldmap")
library(rworldmap)
joinmap <- joinCountryData2Map(country_list, joinCode="NAME", nameJoinColumn="ip_country")
mapCountryData(joinmap, nameColumnToPlot="Freq", mapTitle="World", catMethod='fixedWidth')

