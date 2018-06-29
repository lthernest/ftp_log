# set the directory first

# read the file and fill the empty spaces
log_data <- read.table("ftp_log.txt", header = FALSE, fill = TRUE)

# add the header names
colnames(log_data) <- c("week", "mm", "dd", "tt", "yy", "speed","ip","size", "doi", "NA", "NA", "NA", "NA","NA", "NA","NA", "NA","fin")
ip <- log_data$ip
fin <- log_data$fin
yy <- log_data$yy
dd <- log_data$dd

# convert the month to number
mm <- log_data$mm
mm <- match(mm,month.abb)

# merging year, month and day, display with the date format
dates <-as.Date(paste(yy, mm, dd, sep="-"), format="%Y-%m-%d")

# date of the first file download, remove NA value
min(dates, na.rm=TRUE)
# date of the last file download, remove NA value
max(dates, na.rm=TRUE)

##########complete download per month############
# to search if ftp_log contain parrot ip address
'120.79.135.86' %in% ip

# to exclude all the parrot ip rows
ip_log_data <- subset(log_data, ip!="120.79.135.86")

complete_log_data <- ip_log_data[which(ip_log_data$fin == "c"),]


# create a function to count no. of row per month
count_no <-function(month,year){
  # sum(mm_factor =='Feb')
  # change the value to factor for counting
  mm_factor <- factor(complete_log_data$mm)
  count_month <- sum(mm_factor == month & complete_log_data$yy == year)
  return(count_month)
}


# data between 2017 and 2018
y_2018 <- c(count_no('Jan', 2018), count_no('Feb', 2018), count_no('Mar', 2018), count_no('Apr', 2018), count_no('May', 2018), count_no('Jun', 2018), count_no('Jul',2018), count_no('Aug',2018), count_no('Sep',2018), count_no('Oct',2018), count_no('Nov',2018), count_no('Dec',2018))
y_2017 <- c(count_no('Jan', 2017), count_no('Feb', 2017), count_no('Mar', 2017), count_no('Apr', 2017), count_no('May', 2017), count_no('Jun', 2017), count_no('Jul',2017), count_no('Aug',2017), count_no('Sep',2017), count_no('Oct',2017), count_no('Nov',2017), count_no('Dec',2017))

# assign the label of x-axis
x <- rep(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

# to keep the order of x-axis in data.frame
x1 <- factor(x, levels=x)
values <- c(y_2017, y_2018)
type <-c(rep("2017", 12), rep("2018", 12))
dl <- data.frame(x1,values)

# load in `ggplot2`
install.packages("ggplot2")
library(ggplot2)
p <-ggplot(dl, aes(x1, values)) + geom_bar(stat = "identity", aes(fill = type)) + xlab("Month") + ylab("Freq") + ggtitle("Number of completed files downloaded per month") + theme_bw()
p
ggsave("montly_dl.png", width=8, dpi=100)

##########generate country names of ip using rgeolocate##########
install.packages("rgeolocate")
library(rgeolocate)
ipmmdb <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")

# matching country ip
complete_ip_country <- maxmind(complete_log_data$ip, ipmmdb,"country_name")

# match result
complete_country_list <- data.frame(complete_log_data$ip, complete_ip_country)

ip_result <- data.frame(table(complete_ip_country))

# to test if some ip address is not recognized by the rgeolocate
install.packages("plyr")
library(plyr)

count(complete_country_list$country_name == "NA")
# 3673 entries are NA using rgeolocate package

# display the entries that country name is NA
na_ip_url <- complete_country_list[complete_country_list$country_name %in% NA,]
na_ip <-na_ip_url$complete_log_data.ip
group_na_ip <- count(na_ip)

##########generate country names of NA ip using API##########
install.packages("httr")
library("httr")

# location field parameters
base <- "https://ipapi.co"
na_ip_url <- paste(base,group_na_ip$x,"country_name", sep="/")

# creating an empty vector for collecting the country names
country_vec <- c()
get_country_text <- c()


# running a for loop to parse country names for each IP
for(i in seq_along(na_ip_url))
{
  # retrieve the the country name from URL
  get_country <- GET(na_ip_url[i])
  get_country_text <- content(get_country,"text")
  # pause 1s for each GET
  Sys.sleep(1)
  country_vec <- c(country_vec, get_country_text)
}

# combining IPs with its corresponding country names into a dataframe
na_country_list <- data.frame(na_ip_url,country_vec, group_na_ip$freq)

# aggregate the result
library(plyr)

na_ip_result <- ddply(na_country_list, 'na_country_list$country_vec', numcolwise(sum))

colnames(na_ip_result) <- c("complete_ip_country", "Freq")

# combine the total complete download result
total_ip <- rbind(ip_result, na_ip_result)

# aggregate the total ip result
total_ip_result <- ddply(total_ip, 'total_ip$complete_ip_country', numcolwise(sum))

#export as csv
write.csv(total_ip_result, file="total_ip_result.csv")


##########barchart###########
install.packages("ggplot2")
library(ggplot2)
ggplot(total_ip, aes(x=total_ip$complete_ip_country, y=Freq)) + geom_bar(stat="identity") + labs(x="Country", y="Frequency")


##########map visualization using rworldmap##########
install.packages("rworldmap")
library(rworldmap)
join_complete_map <- joinCountryData2Map(total_ip, joinCode="NAME", nameJoinColumn="complete_ip_country")
rmap <- mapCountryData(join_complete_map, nameColumnToPlot="Freq", mapTitle="Completed file download results organised by countries", catMethod='fixedWidth')

##########map visualization using googleVis##########
install.packages("googleVis")
library(googleVis)
gvismap <- gvisGeoChart(total_ip_result, locationvar = "total_ip$complete_ip_country", colorvar="Freq", options=list(width="80%", height="80%"))
plot(gvismap)

#export the html coding
print(gvismap)

##########list for incomplete download##########
incomplete_log_data <- ip_log_data[which(ip_log_data$fin == "i"),]
incomplete_ip_country <- maxmind(incomplete_log_data$ip, ipmmdb,"country_name")
incomplete_country_list <- data.frame(table(incomplete_ip_country))

# show the country with incomplete download in descending order
incomplete_country_list[order((incomplete_country_list$Freq), decreasing =TRUE),]
# China has the most incomplete downloads - 4,046,925

# export to csv
write.csv(incomplete_country_list, file="incomplete_country_list.csv")