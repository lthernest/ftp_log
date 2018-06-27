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

# create a function to count no. of row per month
  count_no <-function(month,year){
  # sum(mm_factor =='Feb')
  # change the value to factor for counting
  mm_factor <- factor(log_data$mm)
  count_month <- sum(mm_factor == month & yy == year)
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
library(ggplot2)
p <-ggplot(dl, aes(x1, values)) + geom_bar(stat = "identity", aes(fill = type)) + xlab("Month") + ylab("Count") + ggtitle("Number of files downloaded per month") + theme_bw()