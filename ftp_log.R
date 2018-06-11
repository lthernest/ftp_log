# fill = TRUE - fill in empty spaces
log_data <- read.table("ftp_log.txt", header = FALSE, fill = TRUE)

# find how many entries
dim(log_data)

# add the header names
colnames(log_data) <- c("week", "mm", "dd", "tt", "yy")

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

# create a function to count no of row per month
count <-function(month,year){
# sum(mm_factor =='Feb')
# change the value to factor for counting
  mm_factor <- factor(log_data$mm)
  count_month <- sum(mm_factor == month & yy==year)
  return(count_month)
}

# data
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
