x <- read.csv("TripAdvisor.csv", as.is = TRUE)
y
summary(x)

names(x)[2] <- "date"
x$day <- weekdays(as.Date(x$date,'%Y-%m-%d'))

boxplot(BookingPurchase ~ day, data = x)
by(data = x$BookingPurchase, INDICES = x$day, FUN = mean)
names(x)
