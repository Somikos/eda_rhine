#3
temperatures = c(3, 6, 10, 14)
weights = c(1, 0.8, 1.2, 1)

library(data.table)

multiplying = function(x, y){
  x * y
}
results <- multiplying(temperatures, weights)

#4
runoff_gs <- data.frame(time = as.Date(1:90, origin = "2020/01/01"), value = sample(c(130, 135, 140), size = 90, replace = T))

library(data.table)

runoff_dt <- data.table(runoff_gs)
runoff_dt[, mon := month(time)]
runoff_dt[, mon_mean := mean(value), by = mon]

runoff_month <- runoff_dt[, .(mon, mon_mean)]
unique_runoff <- unique(runoff_month)

percentage_change <- function(v1, v2){
  percentage <- ((v2-v1) / v1)*100
}

january_percentage <- percentage_change(unique_runoff[1, mon_mean], unique_runoff[2, mon_mean])
january_percentage

february_percentage <- percentage_change(unique_runoff[2, mon_mean], unique_runoff[3, mon_mean])
february_percentage

unique_runoff[, percentage_change := c(0, january_percentage, february_percentage)]
unique_runoff
