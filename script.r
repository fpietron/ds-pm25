# install required dependencies
install.packages('stringr') # string utils: interpolation, trim
install.packages('dplyr')
install.packages('con2aqi') # utils for transforming PM2.5 => AQI
install.packages("ggplot") # advanced plots
install.packages("ggpmisc") # utils for ggplot
install.packages('zoo')
install.packages('knitr')

library(stringr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(con2aqi)
library(zoo)
library(knitr)

options(digits = 2)


# utility function for use in higher order functions
# use to convert pm25 into AQI
convert_pm25_to_aqi <- function (pm25) {
  result <- con2aqi(pm25, pollutant =  "pm25")
}

# utility for running a predicate against a data frame with removing NAs
by_predicate <- function(data, predicate) {
  result <- sapply(data, predicate, na.rm = TRUE)
}

# util for reading a csv file with PM2.5 observations
read_city_csv <- function (city_name) {
  result <-
    read.csv(str_interp("pm25Desktop${city_name}.csv"),
             na.strings = c(""))
}

# main tidying function for read PM2.5 csv files
parse_pm25_csv <- function (city_name) {
  csv <- read_city_csv(city_name)
  
  # trim values in Column2, as they appear to include tab indentations
  trimmed_col2 <- str_trim(csv$Column2)
  
  # assign columns with new names to a data frame
  frame <-
    data.frame(date = csv$Column1, pm25 = na.locf(trimmed_col2))
  
  # remove info row (station name, measurement name)
  frame <- frame[-c(1),]
  
  # parse PM2.5 values to dbls
  frame$pm25 <- as.numeric(frame$pm25)
  
  #parse dates to Dates
  frame$date <- as.Date(frame$date, format = "%Y-%m-%d")
  
  # ensure date order
  frame[order(frame$date, decreasing = TRUE),]
  
  result <- frame
}

# read all considered cities data
lublin <- parse_pm25_csv('lublin')
bialystok <- parse_pm25_csv('bialystok')
bydgoszcz <- parse_pm25_csv('bydgoszcz')
gorzow_wlkp <- parse_pm25_csv('gorzow-wielkopolski')
katowice <- parse_pm25_csv('katowice')
lodz <- parse_pm25_csv('lodz')
opole <- parse_pm25_csv('opole')
rzeszow <- parse_pm25_csv('rzeszow')
szczecin <- parse_pm25_csv('szczecin')
torun <- parse_pm25_csv('torun')
warszawa <- parse_pm25_csv('warszawa')
wroclaw <- parse_pm25_csv('wroclaw')
zielona_gora <- parse_pm25_csv('zielona-gora')

# aggregate cities data to a single data frame
# with dates as row names
base <- data.frame(
  lublin = lublin$pm25,
  bialystok = bialystok$pm25,
  bydgoszcz = bydgoszcz$pm25,
  gorzow_wlkp = gorzow_wlkp$pm25,
  katowice = katowice$pm25,
  lodz = lodz$pm25,
  opole = opole$pm25,
  rzeszow = rzeszow$pm25,
  szczecin = szczecin$pm25,
  torun = torun$pm25,
  warszawa = warszawa$pm25,
  zielona_gora = zielona_gora$pm25,
  row.names = lublin$date
)

cities <- colnames(base)

# util function, can be used for dynamic bar width based on city name
city_name_to_column_width <- sapply(colnames(base), nchar)

dates <- as.Date(row.names(base))

# base aggregated by month
avg_pm25_by_month <-
  base %>% mutate(month_year = format(dates, "%Y-%m"), ) %>% group_by(month_year) %>% summarise_all(function (data)
    mean(data, na.rm = TRUE))

avg_pm25_by_month <- arrange(avg_pm25_by_month, month_year)

avg_aqi_by_month <-
  avg_pm25_by_month %>% mutate_at(vars(-month_year), convert_pm25_to_aqi)

base_aqi <- subset(avg_aqi_by_month, select = -c(month_year))

mean_aqi <- sapply(base_aqi, mean)

aqi_df <- as.data.frame(mean_aqi)

aqi_df$city = rownames(aqi_df)

aqi_df$color = ifelse(aqi_df$mean_aqi <= 50,
                      'green',
                      ifelse(
                        aqi_df$mean_aqi <= 100,
                        'yellow',
                        ifelse(
                          aqi_df$mean_aqi <= 150,
                          'orange',
                          ifelse(
                            aqi_df$mean_aqi <= 200,
                            'red',
                            ifelse(aqi_df$mean_aqi <= 300, '#a10649', '#7e0023')
                          )
                        )
                      ))

avg_aqi_by_year <-
  avg_aqi_by_month %>% mutate(year = as.numeric(substr(month_year, 1, 4))) %>% group_by(year) %>% summarise_all(function (data)
    mean(data, na.rm = TRUE))

avg_aqi_by_year <- subset(avg_aqi_by_year, select = -c(month_year))

avg_aqi_by_month <-
  avg_aqi_by_month %>% mutate(month_year = as.POSIXct(paste(month_year, "01", sep = "-")))
