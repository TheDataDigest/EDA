

## 0) Setup-up, load packages, clean data, create data frames ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(forcats)
library(janitor)
library(ggpubr)
library(plotly)

#0) load and merge data ----
list_csv_files <- paste0("2023_raw_data/", list.files(path = "2023_raw_data/"))
length(list_csv_files); head(list_csv_files); tail(list_csv_files)
# not in order, make sure to arrange final TT after date, early late, and number

TT_df_backup <- readr::read_csv(list_csv_files, id = "file_name")

#0a) basic mutates ----
# Input date string

TT_df <- TT_df_backup %>% 
  mutate(time = gsub(".*/(\\w+)-.*", "\\1", file_name),
         date_string = str_extract(file_name, "[a-zA-Z]+-\\d+-\\d+"))


# # Define a custom month name translation table (because of German system)
# month_translation <- c("january" = "Januar", "february" = "Februar", 
#                        "march" = "März", "april" = "April", 
#                        "may" = "Mai", "june" = "Juni", 
#                        "july" = "Juli", "august" = "August", 
#                        "september" = "September", "october" = "Oktober", 
#                        "november" = "November", "december" = "Dezember")
# 
# # Replace English month names with German month names
# for (eng_month in names(month_translation)) {
#   ger_month <- month_translation[eng_month]
#   TT_df$date_string <- gsub(eng_month, ger_month, TT_df$date_string, ignore.case = TRUE)
# }

TT_df$date <- as.Date(x = TT_df$date_string, format = "%b-%d-%Y") 

# data frame modifications/additions
#dim(TT_df); str(TT_df); #View(TT_df)


# games_played
TT_df %>% select(Username, contains("RND")) #W111B = Win with Black against number 111
rowSums(TT_df[ ,9:19] != "U--") # some players arrive late or finish early and not play all 11 games
TT_df$games_played <- rowSums(TT_df[ ,9:19] != "U--")
TT_df %>% count(games_played) %>% 
  mutate(percent = n/sum(n)) # , some even play no game at all

#TT_df %>% ggplot(aes(x = games_played)) + geom_histogram()

TT <- TT_df
names(TT); #View(TT)
# not in order, make sure to arrange final TT after date, early late, Number or Rank
TT <- TT %>% janitor::clean_names()
TT <- TT %>% arrange(date, time, number)
names(TT)


TT$title_ordered <- factor(TT$title, levels = c("GM", "IM", "FM", "CM", "NM", "WGM", "WIM", "WFM", "WCM", "WNM"))
TT <- TT %>% mutate(gender = ifelse(str_detect(string = TT$title, pattern = "W"), "Women", "Men"))


## P2) player_df ----
# filter out times, a player did not participate at all
TT <- TT %>% filter(games_played > 0)

TT$fed[TT$name == "Nihal Sarin"] <- "IND"

# fix multiple fed, title and missing name for usernames with n>1

## FIX 509 cases with usernames having 2 average ratings
## must come from a combination of unsername and name? or title differences and fed

username_name <- TT %>% count(username, name) %>% count(username) %>% filter(n > 1) %>% pull(username)
# yes 404 cases

# function, fix name/username, user fed, that is most common? add_count()
# TT %>% count(username) %>% filter(username %in% username_tests) %>% arrange(desc(n))

name_fix <- TT %>% filter(username %in% username_name) %>% 
  count(username, name) %>%  
  mutate(name_length = nchar(name)) %>% 
  arrange(username, desc(name_length)) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup() %>% select(-n, -name_length)

names(name_fix)[2] <- "name2"

TT <- TT %>% left_join(name_fix, by = "username")
TT$name2[is.na(TT$name2)] <- TT$name[is.na(TT$name2)]

#TT %>% filter(is.na(name)) %>% select(username,name, name2)
#TT$name[is.na(TT$name) & !is.na(TT$name2)] <- TT$name2[is.na(TT$name) & !is.na(TT$name2)]

#3645 name NAs remaining, overwrite with username?
#username_where_name2_is_NA <- TT %>% filter(is.na(name2)) %>% pull(username) %>% unique()
# TT %>% filter(username %in% username_where_name2_is_NA) %>% 
#  count(name2) # all NA

# TT %>% filter(username %in% username_where_name2_is_NA) %>% 
#  count(name)

TT$name2[is.na(TT$name2)] <- TT$username[is.na(TT$name2)]
TT %>% count(username, name2) %>% count(username) %>% filter(n > 1) # no cases left

# continue with name2 replace later
sum(is.na(TT$name2)) # 0

# check results, fix title and fed
username_title <- TT %>% count(username, title_ordered) %>% count(username) %>% filter(n > 1) %>% pull(username) # 76 cases

title_fix <- TT %>% filter(username %in% username_title) %>% 
  group_by(username, title_ordered) %>% 
  select(username, title_ordered) %>% distinct() %>% 
  ungroup() %>% 
  arrange(username, title_ordered) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup()

names(title_fix)[2] <- "title2"

TT <- TT %>% left_join(title_fix, by = "username")
TT$title2[is.na(TT$title2)] <- TT$title[is.na(TT$title2)]

#fix fed (sort by most common, add_count)
username_fed <- TT %>% count(username, fed) %>% count(username) %>% filter(n > 1) %>% pull(username) # 112 cases

fed_fix <- TT %>% filter(username %in% username_fed) %>% 
  count(username, fed) %>% 
  arrange(username, desc(n)) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup() %>% select(-n)

names(fed_fix)[2] <- "fed2"

TT <- TT %>% left_join(fed_fix, by = "username")
TT$fed2[is.na(TT$fed2)] <- TT$fed[is.na(TT$fed2)]

TT$name <- TT$name2
TT$name2 <- NULL

TT$title <- TT$title2
TT$title2 <- NULL

TT$fed <- TT$fed2
TT$fed2 <- NULL

same_name_examples <- TT %>% count(username, name, title, fed) %>% group_by(name) %>% 
  add_count() %>% filter(nn > 1) %>% arrange(name)
# 128 cases still

TT$title_ordered <- factor(TT$title, levels = c("GM", "IM", "FM", "CM", "NM", "WGM", "WIM", "WFM", "WCM", "WNM")) 

prize_money <- data.frame(number = 1:5,
                          prize = c(1000, 750, 350, 200, 100))

TT <- TT %>% left_join(prize_money, by = "number")

# TT %>% group_by(username, name) %>% 
#   summarize(total_prize = sum(prize, na.rm = TRUE)) %>% 
#   arrange(desc(total_prize))

# There was only one tournament this week as the early event was cancelled, but it will be made up with a doubled prize fund (including $2,000 for first place) in the early event on January 31.

# TT[TT$date == "2023-01-31" & TT$time == "early", ]   # Hikaru won that day!
# TT[TT$date == "2023-01-31" & TT$time == "early", "prize"]  

TT$prize[TT$date == "2023-01-31" & TT$time == "early"][1:5]  <- c(2000, 1500, 700, 400, 200)

TT$prize2 <- TT$prize
TT$prize2[is.na(TT$prize2)] <- 0

# TT <- TT %>% filter(games_played > 0) # already done before

TT_df_backup %>% count(Username, Name, Fed, Title) %>% 
  group_by(Username) %>% add_count() %>% filter(nn > 1) %>% 
  arrange(desc(nn))

TT %>% select(username, name, fed, title) %>% distinct() # 4129 players remaining

player_df <- TT %>% group_by(username, name, fed, title) %>% 
  summarize(N_participations = length(unique(file_name)),
            min_score = min(score), 
            avg_score = mean(score),
            median_score = median(score), 
            max_score = max(score), 
            best_Place = min(number), 
            avg_Place = mean(number),
            median_Place = median(number), 
            lowest_Place = max(number),
            winning_chance = mean(number == 1),
            N_1st = sum(number == 1),
            N_Top3 = sum(number %in% 1:3),
            N_Top5 = sum(number %in% 1:5),
            N_Top10 = sum(number %in% 1:10),
            N_Top25 = sum(number %in% 1:25), 
            rating_avg = mean(rating),
            rating_best = max(rating),
            rating_lowest = min(rating),
            avg_prize = mean(prize2),
            total_prize = sum(prize2),
            N_all_games_played = sum(games_played == 11),
            avg_games_played = mean(games_played), 
            median_games_played = median(games_played), 
            N_majority_played = sum(games_played > 5),
            avg_majority_played = mean(games_played > 5)) %>% ungroup()

dim(player_df) # 4129 player (keeping, username, name, fed and title due to initial grouping)

player_df$title_ordered <- factor(player_df$title, levels = c("GM", "IM", "FM", "CM", "NM", "WGM", "WIM", "WFM", "WCM", "WNM"))

table(is.na(player_df$name)) #4129 FALSE

# add time columns (participation early and late Ns)
time_df <- TT %>% group_by(username, time) %>% 
  summarize(N = length(unique(file_name))) %>% 
  pivot_wider(names_from = time, values_from = N)

player_df <- player_df %>% left_join(time_df, by = "username")

# results_df and games ----
games <- TT %>% select(username, contains("rnd")) 
games$N_played <- rowSums(games[, -1] != "U--")

results_df <- games %>% select(-N_played) %>% 
  pivot_longer(cols = -username) %>% 
  mutate(value_short = gsub(x = value, pattern = "[[:digit:]]", replacement = '')) %>% 
  group_by(username) %>% 
  summarize(win_White = sum(value_short == "WW"),
            draw_White = sum(value_short == "DW"),
            lose_White = sum(value_short == "LW"),
            win_Black = sum(value_short == "WB"),
            draw_Black = sum(value_short == "DB"),
            lose_Black = sum(value_short == "LB"))

results_df$games_w_White <- rowSums(results_df[, 2:4])
results_df$games_w_Black <- rowSums(results_df[, 5:7])




## X) Thumbnail ----
# avg score vs Rating (all grey, but GM orange, IM yellow) arrows to Hikaru, Carlsen, Levy
player_df %>% 
  filter(N_participations >= 10,
         title_ordered %in% c("GM", "IM", "FM", "CM", "NM")) %>% 
  ggplot(aes(x = rating_best, y = avg_score)) +
  geom_point(color = "grey", alpha = 0.5) +
  geom_point(data = player_df %>% filter(N_participations >= 10, title_ordered %in% c("IM")),
             mapping = aes(x = rating_best, y = avg_score), color = "#E69F00", alpha = 0.6) + 
  geom_point(data = player_df %>% filter(N_participations >= 10, title_ordered %in% c("GM")),
             mapping = aes(x = rating_best, y = avg_score), color = "#0072B2", alpha = 0.7) + 
  geom_point(data = player_df %>% filter(username %in% c("GothamChess", "Hikaru", "MagnusCarlsen")),
             mapping = aes(x = rating_best, y = avg_score), color = "red", alpha = 1) + 
  theme_light() +
  scale_y_continuous(breaks = c(0,2,4,6,8,10), limits = c(0, 9)) + 
  labs(x="", y="")

install.packages("thematic");
library(thematic)
thematic::okabe_ito(8)

c("#E69F00", "#009E73", "#0072B2", "#CC79A7", "#999999", "#D55E00", "#F0E442", "#56B4E9")


# Y) Save environment ----
TT$fed[TT$name == "Anish Giri"] <- "NED"
TT$fed[TT$name == "Christopher Woojin Yoo"] <- "USA"
TT$fed[TT$name == "Maxim Omariev"] <- "RUS"
TT$fed[TT$name == "Nihal Sarin"] <- "IND"
TT$fed[TT$name == "Vladimir Fedoseev"] <- "SVN"
TT$fed[TT$name == "Hovhannes Gabuzyan"] <- "ARM"
TT$fed[TT$name == "Pranesh M"] <- "IND"

player_df$fed[player_df$name == "Anish Giri"] <- "NED"
player_df$fed[player_df$name == "Christopher Woojin Yoo"] <- "USA"
player_df$fed[player_df$name == "Maxim Omariev"] <- "RUS"
player_df$fed[player_df$name == "Nihal Sarin"] <- "IND"
player_df$fed[player_df$name == "Vladimir Fedoseev"] <- "SVN"
player_df$fed[player_df$name == "Hovhannes Gabuzyan"] <- "ARM"
player_df$fed[player_df$name == "Pranesh M"] <- "IND"

save.image(file = "TitledTuesday.RData")
