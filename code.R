### Checking if file exists

if(!file.exists("activity.csv")){
  unzip("activity.zip")
}

### Loading data and processing data

activity_data <- read.csv("activity.csv")
activity_data <- activity_data[!is.na(activity_data[,"steps"]),]
activity_data$date <- as.Date(as.character(activity_data$date))

### Histogram of number of steps per day

Grouped_data <- aggregate(activity_data$steps, by = list(activity_data$date), sum)
names(Grouped_data) <- c("date", "total_steps")

hist(Grouped_data$total_steps, xlab = "Steps", main = "Total Daily Steps", col = "steelblue")

print(paste("Mean number of steps taken daily", mean(Grouped_data$total_steps)))
print(paste("Median number of steps taken daily", median(Grouped_data$total_steps)))

### Average Daily Activity Pattern
library(ggplot2)
Mean_Data_By_Interval <- aggregate(activity_data$steps, by=list(activity_data$interval), mean)
names(Mean_Data_By_Interval) <- c("interval", "steps")

ggplot(Mean_Data_By_Interval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="red") 

maxInterval <- Mean_Data_By_Interval[which.max(Mean_Data_By_Interval$steps),]
maxInterval
