library(data.table)
library(ggplot2)

runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_day <- readRDS('data/runoff_day.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')
runoff_summary <- readRDS('data/runoff_summary.rds')
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
key_stations <- c('DOMA', 'BASR', 'KOEL')

#1
runoff_year_key[year <= 2010, age_range := factor('before_2010')]
runoff_year_key[year > 2010, age_range := factor('after_2010')]

ggplot(runoff_year_key, aes(age_range, value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Age Range") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

runoff_month_key[year <= 2010, age_range := factor('before_2010')]
runoff_month_key[year > 2010, age_range := factor('after_2010')]

ggplot(runoff_month_key, aes(factor(month), value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

#2
runoff_day_key <- runoff_day[sname %in% key_stations]
year_threshold <- 2010
runoff_day_key[year <= year_threshold, age_range := factor('before_2010')]
runoff_day_key[year > year_threshold, age_range := factor('after_2010')]
runoff_day_key[, quant_01 := quantile(value, 0.1), by = .(sname, month)]
runoff_day_key[, quant_09 := quantile(value, 0.9), by = .(sname, month)]

runoff_day_key[, runoff_class := factor('medium')]
runoff_day_key[value <= quant_01, runoff_class := factor('low')]
runoff_day_key[value >= quant_09, runoff_class := factor('high')]
runoff_day_key[, days := .N, .(sname, year, runoff_class, season)]

runoff_day_key_class <- unique(runoff_day_key[, .(sname, days, year, age_range, season, runoff_class)])

ggplot(runoff_day_key_class[season == 'winter' | season == 'summer'], 
       aes(season, days, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(runoff_class~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Days") +
  theme_bw()

#3
runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

#
ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

#Explorer’s questions
#1
#DOMA is not representative, because of high altitude and general differences between DOMA and rest of the stations

#2
precipitation_day <- readRDS('./data/raw/precip_day.rds')

precipitation_day[, month := month(date)]
precipitation_day[month == 12 | month == 1 | month == 2, season := 'winter']
precipitation_day[month == 3 | month == 4 | month == 5, season := 'spring']
precipitation_day[month == 6 | month == 7 | month == 8, season := 'summer']
precipitation_day[month == 9 | month == 10 | month == 11, season := 'autumn']
precipitation_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precipitation_day[, year := year(date)]
precip_winter <- precipitation_day[season == 'winter', 
                            .(value = sum(value)), by = year]
precip_summer <- precipitation_day[season == 'summer', 
                            .(value = sum(value)), by = year]

year_threshold <- 2007
to_plot <- rbind(cbind(precip_winter, season = factor('winter')), 
                 cbind(precip_summer, season = factor('summer'))) 

to_plot[year < year_threshold, period := factor('before_2007')]
to_plot[year >= year_threshold, period := factor('after_2007')]
to_plot[year < year_threshold, period := factor('before_2007')]
to_plot[year >= year_threshold, period := factor('after_2007')]

ggplot(to_plot[year >= 1977], aes(season, value, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Precitation") +
  theme_bw()

ggplot(to_plot[season == 'summer' & year >= 1977], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation Summer") +
  theme_bw()

ggplot(to_plot[season == 'winter' & year >= 1977], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation Winter") +
  theme_bw()

#3
#There are changes in the runoff and precipitation and I think we can say, that one of the main reasons is climate change

#4
#I think we should explore more data on precipitation and water pollution 