#Note temperatures are in degrees Fahrenheit
library(tidyverse)
library(rvest)
library(readxl)

get_weather_table <- function(url)
  read_html(url) %>% 
  html_nodes("div.monthly-calendar") %>% 
  html_text2() %>%
  str_replace("N/A", "N/A N/A") %>%
  str_remove_all("°|Hist. Avg. ") %>%
  str_split(" ", simplify = TRUE) %>%
  parse_number() %>%
  matrix(ncol = 3, 
         byrow = TRUE,
         dimnames = list(NULL, c("day", "tmax", "tmin"))) %>%
  as_tibble() %>%
  filter(
    row_number() %in%
      (which(diff(day) < 0) %>% (function(x) if(length(x) == 1) seq(1, x[1], 1) else seq(x[1] + 1, x[2], 1))))

# NOTE: Kyoto code no longer works as archive.org has removed the base url.
# However, a copy of this data was stored into the kyoto_202.xlsx file, which can be used instead.

#kyoto <-
  #tibble(
    #base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/jp/arashiyama/2334469/",
    #month = month.name[1:4],
    #year = 2025,
    #url = str_c(base_url, tolower(month), "-weather/2334469?year=", year)) %>%
  #mutate(temp = map(url, get_weather_table)) %>%
  #pull(temp) %>%
  #reduce(bind_rows) %>%
  #transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            #year = parse_number(format(date, "%Y")),
            #tmax,
            #tmin,
            #temp = (tmax + tmin) / 2)

kyoto <- read_xlsx("kyoto_2025.xlsx")

#liestal march
liestal <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/ch/liestal/311994/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/311994?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

newyork <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/new-york/10021/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/349727?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)

washington <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/washington/20006/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/18-327659_1_al?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

vancouver <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/vancouver/98661/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/331419?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

# Update all tables to have their respective locations
washington <- washington %>%
  mutate(location = "washingtondc")
kyoto <- kyoto %>%
  mutate(location = "kyoto")
liestal <- liestal %>%
  mutate(location = "liestal")
vancouver <- vancouver %>%
  mutate(location = "vancouver")
newyork <- newyork %>%
  mutate(location = "newyorkcity")
