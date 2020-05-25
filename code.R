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

### Imputing Missing values 

activity_data <- read.csv("activity.csv")
missing_values <- is.na(activity_data$steps)
print(paste("Number of Missing values are", sum(missing_values)))

clean_activity_data <- activity_data[!missing_values,]
mean_values <- tapply(clean_activity_data$steps, clean_activity_data$interval, mean, na.rm=TRUE, simplify=TRUE)
activity_data$steps[missing_values] <- mean_values[as.character(activity_data$interval[missing_values])]
print(paste("Number of Missing values after imputing", sum(is.na(activity_data$steps))))

Grouped_data <- aggregate(activity_data$steps, by = list(activity_data$date), sum)
names(Grouped_data) <- c("Date", "total_steps")
hist(Grouped_data$total_steps, xlab = "Steps", main = "Total Daily Steps", col = "steelblue")
print(paste("Mean number of steps taken daily", mean(Grouped_data$total_steps)))
print(paste("Median number of steps taken daily", median(Grouped_data$total_steps)))

### Difference between activity patterns of weekdays and weekends

activity_data$date <- as.Date(as.character(activity_data$date))
activity_data$weekday <- weekdays(activity_data$date)
activity_data$weekend <- ifelse (activity_data$weekday == "Saturday" | activity_data$weekday == "Sunday", "Weekend", "Weekday")
Grouped_data <- aggregate(activity_data$steps, by = list(activity_data$weekend, activity_data$interval), mean)
names(Grouped_data) <- c("Weekend", "Interval", "Steps")
ggplot(Grouped_data, aes(x = Interval, y=Steps, color=Weekend)) +
  geom_line() +
  facet_grid(Weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")