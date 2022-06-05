
# ** Analyzing Monkeypox Cases in R for Beginners **

## (1) Setup: loading packages and data ----
rm(list=ls())
ls()

install.packages("httr")
library(httr)
df_raw <- read.csv(text=content(GET("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# library(readxl)
# df_raw <- read_excel(path = paste0(getwd(),"/Monkeypox_Public_www.global.health_20220604.xlsx"),
#                             sheet = 1)


# 2) Exploring the data [dim(), names(), str()]
dim(df_raw)
str(df_raw)
head(df_raw)
View(df_raw)

is.na(df_raw)
sort(round(colMeans(is.na(df_raw)) * 100, 2), decreasing = T)

library(tidyverse)
 
table(df_raw$Status)
df_raw %>% count(Status)


# 3) Cleaning the data

library(janitor)
monkeypox <- df_raw %>% 
  janitor::clean_names() %>% 
  filter(status == "confirmed")

names(monkeypox)
monkeypox$status %>% table()
 
# 4) Cases per country barchart
table(monkeypox$country)
table(monkeypox$country, sort = T)
table(monkeypox$age, exclude = NULL)
monkeypox %>% count(country, sort = T)

# geom_bar()
monkeypox %>% ggplot(mapping = aes(y = country)) +
  geom_bar(stat = "count")

# geom_col()
monkeypox %>% count(country) %>% 
  ggplot(aes(x = n, y = country)) +
  geom_col()


# "sort = T in count does not work"
monkeypox %>% count(country, sort = T) %>% 
  ggplot(aes(x = n, y = country)) + 
  geom_col()

# sorting countries with fct_reorder
library(forcats)

monkeypox %>% 
  count(country, sort = T) %>% 
  mutate(country = as.factor(country),
         country = forcats::fct_reorder(country, n)) %>%
  ggplot(aes(x = n, y = country)) + 
  geom_col()

# lumping countries together
monkeypox %>% 
  mutate(country = as.factor(country),
         country_other = forcats::fct_lump(f = country, n = 10)) %>% 
  pull(country_other) %>% 
  table()

monkeypox %>% 
  mutate(country = as.factor(country),
         country_other = forcats::fct_lump(f = country, n = 10)) %>% ggplot(aes(y = country_other)) + geom_bar()


monkeypox %>% 
  add_count(country) %>% 
  mutate(country = as.factor(country),
         country = forcats::fct_reorder(country, -n)) %>% 
  mutate(country_other = forcats::fct_lump(f = country, n = 10, other_level = paste0(length(unique(monkeypox$country)) - 10, " other countries"))) %>% 
  count(country_other) %>% 
  ggplot(aes(x = n, y = country_other)) + 
  geom_col() +
  scale_y_discrete(limits=rev)


## pretty chart

monkeypox %>% 
  add_count(country) %>% 
  mutate(country = as.factor(country),
         country = forcats::fct_reorder(country, -n)) %>% 
  mutate(country_other = forcats::fct_lump(f = country, n = 10, other_level = paste0(length(unique(monkeypox$country)) - 10, " Other countries"))) %>% 
  count(country_other) %>% 
  ggplot(aes(x = n, y = country_other, label = n)) + 
  geom_col(aes(fill = country_other), color = "black") +
  geom_label(aes(label = n, fill = country_other), 
             color = "black", alpha = 0.5, nudge_x = 10, size = 4) +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 12)) + 
  scale_y_discrete(limits=rev) + 
  scale_x_continuous(limits = c(0, 230)) +
  labs(title = paste0(nrow(monkeypox), " Monkeypox cases in ", length(unique(monkeypox$country)), " different countries"),
       subtitle = paste0("Confirmed cases from: ", min(as.Date(monkeypox$date_confirmation), na.rm =T), " to ", max(as.Date(monkeypox$date_confirmation), na.rm =T)),
       x = "", y = "")



# 5) Cases per country map
library(maps)
world <- map_data("world") %>% 
  filter(region != "Antarctica")

world %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", fill = "lightgrey") 

monkeypox$country <- gsub(pattern = "England", replacement = "UK", x = monkeypox$country)
monkeypox$country <- gsub(pattern = "United States", replacement = "USA", x = monkeypox$country)

centroids <- world %>% 
  filter(world$region %in% monkeypox$country) %>% 
  group_by(region) %>% 
  summarize(xlong = mean(long),
            ylat = mean(lat))

centroids2 <- world %>% 
  filter(world$subregion %in% monkeypox$country) %>% 
  group_by(subregion) %>% 
  summarize(xlong = mean(long),
            ylat = mean(lat))

names(centroids2)[1] <- "region"

centroids <- bind_rows(centroids, centroids2)
#View(centroids)

cases <- monkeypox %>% count(country)
centroids$cases <- cases$n[match(centroids$region, cases$country)] 
# left_join()
centroids %>% left_join(cases, by = c("region" = "country"))

world %>% 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(color = "black", fill = "lightgrey")  +
  theme_bw() +
  geom_point(data = centroids, 
             mapping = aes(x = xlong, y = ylat,
                           size = cases, group = region),
             color = "red", alpha = 0.5) +
  coord_cartesian(xlim = c(-15, 35), ylim = c(35, 75))+ 
  scale_size(range = c(1, 10)) +
  labs(title = paste0(nrow(monkeypox), " Monkeypox cases in ", length(unique(monkeypox$country)), " different countries"),
       size = "Confirmed\ncases:", x = "", y = "",
       subtitle = paste0("Confirmed cases from: ", min(as.Date(monkeypox$date_confirmation), na.rm =T), " to ", max(as.Date(monkeypox$date_confirmation), na.rm =T)))

# 6) Age distribution
monkeypox <- monkeypox %>% 
  mutate(age_left = as.numeric(gsub(replacement = "", x = age, pattern = "-..")),
         age_right = as.numeric(gsub(replacement = "", x = age, pattern = "..-")),
         age_new = (age_left + age_right) / 2)

monkeypox %>% count(age_new)

monkeypox %>% ggplot(aes(x = age_new)) +
  geom_histogram()



















## age ----
table(monkeypox$age)
table(monkeypox$age, exclude = NULL)

monkeypox %>% count(age)

library(stringr)
monkeypox <- monkeypox %>% 
  mutate(age_left = as.numeric(gsub(replacement = "", x = age, pattern = "-..")),
         age_right = as.numeric(gsub(replacement = "", x = age, pattern = "..-")),
         age_new = (age_left + age_right) / 2)

monkeypox %>% ggplot(aes(x = age_new)) +
  geom_histogram() +
  scale_x_continuous(limits = c(20, 60))

mean(monkeypox$age_new)
mean(monkeypox$age_new, na.rm = T)
summary(monkeypox$age_new)

## gender ----
monkeypox %>% count(gender)
monkeypox %>% pull(gender) %>% tolower() %>% table()
total <- sum(.Last.value)

monkeypox %>% pull(gender) %>% tolower() %>% table() / total * 100 

monkeypox %>%
  mutate(gender = tolower(gender)) %>% 
  count(gender) %>% 
  filter(!is.na(gender)) %>% 
  mutate(percent = n / sum(n),
         percent_100 = percent * 100)

## hospitalization

monkeypox %>%
  count(hospitalised_y_n_na)

monkeypox %>%
  count(hospitalised_y_n_na) %>% 
  filter(!is.na(hospitalised_y_n_na)) %>% 
  mutate(percent = n / sum(n),
         percent_100 = percent * 100)

# hospitalization status of the top 5 countries
table(monkeypox$country, monkeypox$hospitalised_y_n_na)

## symptoms

## mapping


## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("devtools")  # Only needed if you don't have devtools installed
library(devtools)
devtools::install_github("drsimonj/ourworldindata")

library(easypackages)
easypackages::libraries("ourworldindata", "tidyverse", "lubridate", "countrycode", "maps")


#https://drsimonj.svbtle.com/ourworld-an-r-data-package
?ourworldindata

## (2) Loading and investigating the data ----



# Wales, Northern Ireland, Scotland in subregion
# before centroids, turn subregions into regions

# plotting the map (2 ways, leftjoin data to world, or adding geom point afterwards)
# matching cases to centroids

filter(region %in% monkeypox$country)
  
cases <- monkeypox %>% count(country)
centroids$cases <- cases$n[match(centroids$region, cases$country)] # left_join()

# interval_cm %>% 
#   right_join(world, by = c(country = "region")) %>% 
#   filter(country %in% owid_cm$country) %>% 
  

  

#
#  theme_linedraw() + 
  facet_wrap(~ interval) +
 +
  scale_fill_gradient2(low = "darkgreen", mid = "pink", 
                       high = "purple", midpoint = 300) +
  labs(title = "Child mortality rate: Country average over 50 year intervals",
       fill = "Deaths per\n100,000", x = "", y = "",
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: library(ourworldindata); child_mortality") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())
  
  
## Reference:
  
  # Data compiled here are exclusively from publicly available sources which are linked to each entry.
  # They are collated by our team at global.health (https://www.global.health/).
  # We currently update the file as new data is being reported.
  # Sources for each case are listed in columns Z and AA and some additional information may be found in a field.
  # 
  # Please check permissions before publishing these data and refer back to each source & our team if you decide to so: Global.health Monkeypox (accessed on YYYY-MM-DD)
  # 
  # Should you identify any inconsistencies in the data or have additional information or questions please get in touch.
  # 
  # A data dictionary and machine readable files are available from our Github: https://github.com/globaldothealth/monkeypox


         
