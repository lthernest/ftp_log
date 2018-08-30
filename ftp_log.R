# set the directory first

# read the file and fill the empty spaces
log_data <- read.table("ftp_log.txt", header = FALSE, fill = TRUE)

# add the header names
colnames(log_data) <- c("week", "mm", "dd", "tt", "yy", "speed","ip","size", "doi", "NA", "NA", "NA", "NA","NA", "NA","NA", "NA","fin")

##########data cleaning############
# to search if ftp_log contain parrot ip address
'120.79.135.86' %in% ip

# to exclude all the parrot ip rows
ip_log_data <- subset(log_data, ip!="120.79.135.86")

# complete download per month
complete_log_data <- ip_log_data[which(ip_log_data$fin == "c"),]

# convert the month to number
complete_log_data$mm <- match(complete_log_data$mm,month.abb)

# find the records under /pub directory only
install.packages("stringr")
install.packages("dplyr")

library(stringr)
library(dplyr)

pub_complete_log_data <- complete_log_data %>% filter(str_detect(complete_log_data$doi, "/pub"))

###########doi analysis##########
library(stringr)

# split the path and get the doi number
doi_pub_complete_log_data <- str_split_fixed(pub_complete_log_data$doi, "/", 6)
colnames(doi_pub_complete_log_data) <- c("V1", "V2", "doi1", "V4", "doi2", "V6")
doi2_pub_complete_log_data <- as.data.frame(doi_pub_complete_log_data)

# top 10 donwload publication
count_pub <- table(doi2_pub_complete_log_data$doi2)
count_pub <- as.data.frame(count_pub)
top10pub <-head(count_pub[order(-count_pub$Freq),],10)

# doi matching
install.packages("rcrossref")
library(rcrossref)
doi_base <- "10.5524"
doi_url <- data.frame(paste(doi_base,top10pub$Var1,sep="/"))
colnames(doi_url) <- c ('doi')
doi_url_list <- cr_cn(dois = doi_url$doi, "text", "apa")

###########average period of download before release(all datasets)##########
# merging year, month and day, display with the date format
dates <-as.Date(paste(pub_complete_log_data$yy, pub_complete_log_data$mm, pub_complete_log_data$dd, sep="-"), format="%Y-%m-%d")

list_combine_dates <- data.frame(doi2_pub_complete_log_data$doi2, dates)

# find the earliest date of downlod for the publications
library(dplyr)
list_combine_min_dates <- list_combine_dates %>% 
  group_by(doi2_pub_complete_log_data.doi2) %>%
  filter(dates == min(dates))
list_min_dates_aggregate <- aggregate(rep(1, nrow(list_combine_min_dates)), by = list(doi = list_combine_min_dates$doi2_pub_complete_log_data.doi2, dates = list_combine_min_dates$dates), sum) 

# read the table containing doi and publish date
dataset_date <- read.table("dataset_date", header = TRUE, fill = TRUE, sep = '|')
colnames(dataset_date) <- c("doi", "publish_date")

dataset_date$publish_date <- as.Date(dataset_date$publish_date, format= "%Y-%m-%d")

# remove the doi with date > 2018-05-07 
dataset_date_subset <- subset(dataset_date, publish_date < "2018-05-07")


list_min_publish_dates <- merge(list_min_dates_aggregate, dataset_date_subset, by = "doi")

time_diff_all <- as.Date(list_min_publish_dates$dates) - as.Date(list_min_publish_dates$publish_date)

mean(time_diff_all)

###########find the datasets without being donwloaded##########
doi_without_dl <- data.frame(setdiff(dataset_date_subset$doi, list_min_dates_aggregate$doi))

# doi matching for doi without being downloaded
install.packages("rcrossref")
library(rcrossref)
doi_without_dl_url <- data.frame(paste(doi_base,doi_without_dl$setdiff.dataset_date_subset.doi..list_min_dates_aggregate.doi.,sep="/"))
colnames(doi_without_dl_url) <- c ('doi')
doi_without_dl_url_list <- cr_cn(dois = doi_without_dl_url$doi, "text", "apa")
doi_without_dl_url_list

###########average period of download before release(top 10 popular datasets)##########
top10_data_list <- data.frame(pub_complete_log_data,doi2_pub_complete_log_data, dates)
top10_list <- top10_data_list[top10_data_list$doi2 %in% top10pub$Var1,]
top10_list_aggregate_dates <- aggregate(rep(1, nrow(top10_list)), by = list(doi = top10_list$doi2, dates = top10_list$dates), sum)

# check the min date to download
library(dplyr)
top10_min_date <- top10_list_aggregate_dates %>% 
  group_by(doi) %>%
  filter(dates == min(dates))
pd <- c("2014-06-30", "2014-12-17", "2014-05-16", "2013-07-22", "2016-09-09", "2015-11-24", "2015-03-25", "2015-02-16", "2016-11-14", "2017-06-12")
top10_date <- data.frame(top10_min_date,pd)

# publish date minus first download
time_diff <- as.Date(top10_date$dates) - as.Date(top10_date$pd)
mean(time_diff)

###########Monthly download by top 10 popular datasets##########
#set the local time to English
Sys.setlocale("LC_TIME", "English")

top10_list_aggregate_dates$Year <- format(top10_list_aggregate_dates$dates, "%Y")
top10_list_aggregate_dates$Month <- format(top10_list_aggregate_dates$dates, "%b")
top10_list_aggregate_dates$Day <- format(top10_list_aggregate_dates$dates, "%d")

top10_list_aggregate_dates$MonthDay <- format(top10_list_aggregate_dates$dates, "%d-%b")

ggplot(data = top10_list_aggregate_dates, mapping = aes(x = Month, y = x, shape = Year, colour = doi)) + geom_point() + geom_line() + facet_grid(facets = Year ~ .) + scale_x_discrete(limits = month.abb)+ ylab("Freq") + ggtitle("Monthly completed downloads by top 10 popular datasets")

##########monthly download##########

# date of the first file download, remove NA value
min(dates, na.rm=TRUE)
# date of the last file download, remove NA value
max(dates, na.rm=TRUE)

# create a function to count no. of row per month
count_no <-function(month,year){
  
  # change the value to factor for counting
  mm_factor <- factor(pub_complete_log_data$mm)
  count_month <- sum(mm_factor == month & pub_complete_log_data$yy == year)
  return(count_month)
}

# data between 2017 and 2018
y_2018 <- c(count_no('1', 2018), count_no('2', 2018), count_no('3', 2018), count_no('4', 2018), count_no('5', 2018), count_no('6', 2018), count_no('7',2018), count_no('8',2018), count_no('9',2018), count_no('10',2018), count_no('11',2018), count_no('12',2018))
y_2017 <- c(count_no('1', 2017), count_no('2', 2017), count_no('3', 2017), count_no('4', 2017), count_no('5', 2017), count_no('6', 2017), count_no('7',2017), count_no('8',2017), count_no('9',2017), count_no('10',2017), count_no('11',2017), count_no('12',2017))

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
p <-ggplot(dl, aes(x1, values)) + geom_bar(stat = "identity", position=position_dodge(), aes(fill = type)) + xlab("Month") + ylab("Freq") + ggtitle("Monthly completed downloads") + theme_bw()
p

ggsave("montly_dl.png", width=8, dpi=100)


##########dissecting data for analysis##########
# to count the row contain "/pub"
# sum(grepl("/pub",log_data_2017Dec$doi))

# Create a function to find the popular dataset by year and month
top10pub_year_month <- function(month,year){
  
  # locate the data with corresponding year and month
  log_data_year_month <- pub_complete_log_data[which(pub_complete_log_data$yy == year & pub_complete_log_data$mm == month),]
  
  # extract doi
  library(stringr)
  doi_log_data_year_month <- str_split_fixed(log_data_year_month$doi, "/", 6)
  colnames(doi_log_data_year_month) <- c("V1", "V2", "doi1", "V4", "doi2", "V6")
  doi2_log_data_year_month <- as.data.frame(doi_log_data_year_month)
  
  # Top 10 list for publication download
  Freq_year_month <- table(doi2_log_data_year_month$doi2)
  Freq_year_month <- as.data.frame(Freq_year_month)
  Freq_year_month <- head(Freq_year_month[order(-Freq_year_month$Freq),],10)
  
  year_month <- paste(year, month, sep = "-")
  colnames(Freq_year_month) <- c(year_month, "Freq")
  return(Freq_year_month)
}

##########generate country names of ip using rgeolocate##########
install.packages("rgeolocate")
library(rgeolocate)
ipmmdb <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")

# matching country ip
complete_ip_country <- maxmind(pub_complete_log_data$ip, ipmmdb,"country_name")

# match result
complete_country_list <- data.frame(pub_complete_log_data$ip, complete_ip_country)

ip_result <- data.frame(table(complete_ip_country))

# to test if some ip address is not recognized by the rgeolocate
install.packages("plyr")
library(plyr)

count(complete_country_list$country_name == "NA")
# 3028 entries are NA using rgeolocate package

# display the entries that country name is NA
na_ip_url <- complete_country_list[complete_country_list$country_name %in% NA,]
na_ip <-na_ip_url$pub_complete_log_data.ip
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

# top 10 countries with most frequent complete download
head(total_ip_result[order(-total_ip_result$Freq),],10)

# export as csv
write.csv(total_ip_result, file="total_ip_result.csv")


##########barchart###########
install.packages("ggplot2")
library(ggplot2)
ggplot(total_ip, aes(x=total_ip$complete_ip_country, y=Freq)) + geom_bar(stat="identity") + labs(x="Country", y="Frequency")


##########map visualization using googleVis##########
install.packages("googleVis")
library(googleVis)
gvismap <- gvisGeoChart(total_ip_result, locationvar = "total_ip$complete_ip_country", colorvar="Freq", options=list(width="80%", height="80%"))
plot(gvismap)

# export the html coding
print(gvismap)



##########list for incomplete download##########
incomplete_log_data <- ip_log_data[which(ip_log_data$fin == "i"),]
incomplete_ip_country <- maxmind(incomplete_log_data$ip, ipmmdb,"country_name")
incomplete_country_list <- data.frame(table(incomplete_ip_country))

# show the countries with incomplete download in descending order
incomplete_country_list[order((incomplete_country_list$Freq), decreasing =TRUE),]
# China has the most incomplete downloads - 4,046,925

# export to csv
write.csv(incomplete_country_list, file="incomplete_country_list.csv")
ggsave("montly_dl.png", width=8, dpi=100)


################incomplete monthly donwload (China)####################

incomplete_log_data_ip_country <- data.frame(incomplete_log_data, incomplete_ip_country)
incomplete_log_data_china <- incomplete_log_data_ip_country[which(incomplete_log_data_ip_country$country_name == "China"),]


################Bar chart for monthly incomplete donwloads in China####################
# create a function to count no. of row per month
incomplete_log_data_china$mm <- match(incomplete_log_data_china$mm,month.abb)

count_no_china <-function(month,year){
  
  # change the value to factor for counting
  mm_factor_china <- factor(incomplete_log_data_china$mm)
  count_month_china <- sum(mm_factor_china == month & incomplete_log_data_china$yy == year)
  return(count_month_china)
}

# data between 2017 and 2018
y_china_2018 <- c(count_no_china('1', 2018), count_no_china('2', 2018), count_no_china('3', 2018), count_no_china('4', 2018), count_no_china('5', 2018), count_no_china('6', 2018), count_no_china('7',2018), count_no_china('8',2018), count_no_china('9',2018), count_no_china('10',2018), count_no_china('11',2018), count_no_china('12',2018))
y_china_2017 <- c(count_no_china('1', 2017), count_no_china('2', 2017), count_no_china('3', 2017), count_no_china('4', 2017), count_no_china('5', 2017), count_no_china('6', 2017), count_no_china('7',2017), count_no_china('8',2017), count_no_china('9',2017), count_no_china('10',2017), count_no_china('11',2017), count_no_china('12',2017))

# assign the label of x-axis
x <- rep(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

# to keep the order of x-axis in data.frame
x1 <- factor(x, levels=x)
values <- c(y_china_2017, y_china_2018)
type <-c(rep("2017", 12), rep("2018", 12))
dl_china <- data.frame(x1,values)

library(ggplot2)
p_china <-ggplot(dl_china, aes(x1, values)) + geom_bar(stat = "identity", position=position_dodge(), aes(fill = type)) + xlab("Month") + ylab("Freq") + ggtitle("Monthly incomplete downloads in China") + theme_bw()
p_china

ggsave("montly_dl_china.png", width=8, dpi=100)

###############Top 10 incomplete ip address and its value##############
incomplete_china_ip <- data.frame(table(incomplete_log_data_china$ip))
top10_incomplete_china_ip <- head(incomplete_china_ip[order(-incomplete_china_ip$Freq),],10)

#function to retrieve ip value
library("httr")
# ip_value: "region", "org"
ip_retrieve <- function (ip_list, ip_value){
  
  base <- "https://ipapi.co"
  incomplete_url <- paste(base,ip_list,ip_value, sep="/")
  
  # creating an empty vector for collecting the country names
  country_vec <- c()
  get_country_text <- c()
  
  # running a for loop to parse country names for each IP
  for(i in seq_along(incomplete_url))
  {
    # retrieve the the country name from URL
    get_country <- GET(incomplete_url[i])
    get_country_text <- content(get_country,"text")
    # pause 1s for each GET
    Sys.sleep(1)
    country_vec <- c(country_vec, get_country_text)
  }
  incomplete_list <- data.frame(country_vec)
  colnames(incomplete_list) <- c(ip_value)
  return(incomplete_list)
}

top10_incomplete_china_ip_list <- data.frame((top10_incomplete_china_ip), ip_retrieve(top10_incomplete_china_ip$Var1, "org"), (ip_retrieve(top10_incomplete_china_ip$Var1, "region")))
colnames(top10_incomplete_china_ip_list) <- c("ip", "freq", "organization", "region")

ip_variable <- top10_incomplete_china_ip_list$ip
ip_variable_list <- incomplete_log_data_china[incomplete_log_data_china$ip %in% ip_variable,]

#aggregate with doi and no. of accessing
file_size <- as.numeric(ip_variable_list$size)
file_size_aggregate <- aggregate(file_size, list(ip_variable_list$ip), max)
colnames(file_size_aggregate) <- c("ip","size")

ip_aggregate <- aggregate(rep(1, nrow(ip_variable_list)), by = list(ip = ip_variable_list$ip, doi = ip_variable_list$doi, mm = ip_variable_list$mm), sum)

#merge datasets
library(reshape)
final_incomplete_ip_list <- merge_recurse(list(top10_incomplete_china_ip_list, ip_aggregate, file_size_aggregate))
final_incomplete_ip_list <- head(final_incomplete_ip_list[order(-final_incomplete_ip_list$x),],10)
