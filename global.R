rm(list=ls())

# Load data
data.confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data.deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data.recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# Compute statistics
total <- list()
total$confirmed <- colSums(data.confirmed[, 5:ncol(data.confirmed)])
total$deaths <- colSums(data.deaths[, 5:ncol(data.deaths)])
total$recovered <- colSums(data.recovered[, 5:ncol(data.recovered)])

library(tidyverse)
library(lubridate)
library(rvest)
library(countrycode)

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    rename(country = `Country/Region`) %>%
    mutate(iso3c = countrycode(country,
                               origin = "country.name",
                               destination = "iso3c")) %>%
    select(-country) %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise_at(vars(-group_cols()), sum) %>% 
    pivot_longer(
      -iso3c, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    ungroup() %>%
    mutate(date = mdy(date_str)) %>%
    select(iso3c, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols())
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols())
recovered_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = cols())

jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw), by = c("iso3c", "date")) %>%
  full_join(clean_jhd_to_long(recovered_raw), by = c("iso3c", "date"))

jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c))

old_jhd_countries <- tibble(
  country = unique(recovered_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c),
             ! iso3c %in% jhd_countries$iso3c)

jhd_countries <- rbind(jhd_countries, old_jhd_countries)

jh_covid19_data %>%
  left_join(jhd_countries, by = "iso3c") %>%
  select(country, iso3c, date, confirmed, deaths, recovered) -> jh_covid19_data

jh_covid19_data$country[which(jh_covid19_data$country == "Taiwan*")] <- "Taiwan"
jh_covid19_data$country[which(jh_covid19_data$country == "US")] <- "USA"
jh_covid19_data$country[which(jh_covid19_data$country == "Congo (Kinshasa)")] <- "Democratic Republic of the Congo"
jh_covid19_data$country[which(jh_covid19_data$country == "Congo (Brazzaville)")] <- "Republic of the Congo"


write_csv(jh_covid19_data, file="data/jh_covid19_ctry_level.csv")

# Get dataframe with total per day.
df.total <- data.frame(unique(jh_covid19_data$date), 
                       unique(colSums(data.confirmed[, 5:ncol(data.confirmed)])),
                       unique(colSums(data.deaths[, 5:ncol(data.deaths)])),
                       unique(colSums(data.recovered[, 5:ncol(data.recovered)])))
colnames(df.total) <- c("date", "confirmed", "deaths", "recovered")
df.total$date <- as.POSIXct(df.total$date)

library(TTR)
library(zoo)

countries <- unique(jh_covid19_data$country)

# Group by Country and get last 20 days.
by_country <- jh_covid19_data %>% group_by(country) %>% 
  slice(tail(row_number(), 20)) %>% 
  mutate(diff_confirmed = confirmed - lag(confirmed)) %>% 
  mutate(MA = SMA(diff_confirmed, n=7))
by_country <- by_country[complete.cases(by_country), ]

library(purrr)
nested_by_country <- by_country %>% nest() %>% 
  mutate(slope = purrr::map(data, ~lm(MA ~ date, data = .)$coef[2])) %>% 
  unnest(slope)
country_performance <- arrange(nested_by_country %>% ungroup(., 1), slope, )
country_performance_today <- country_performance %>% 
  unnest(data) %>% 
  group_by(country) %>% 
  slice(tail(row_number(), 1)) %>% 
  ungroup()

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Countries for Map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
colnames(world)[which(colnames(world)=="adm0_a3")]<-"iso3c"
world$iso3c[which(world$iso3c=="SDS")]<-"SSD"

# Add daily data
jh_covid19_data_daily <- jh_covid19_data %>% 
  group_by(country) %>% 
  mutate(confirmed_diff = confirmed - lag(confirmed),
         deaths_diff = deaths - lag(deaths),
         recovered_diff = recovered - lag(recovered)
  ) %>%
  ungroup()

jh_covid19_data_daily <- jh_covid19_data_daily[complete.cases(jh_covid19_data_daily), ]


  