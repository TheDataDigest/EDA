
load(file = "TitledTuesday.RData")

player_df %>% filter(name %in% c("Dmitry Andreikin", 
                                 "Daniel Naroditsky", 
                                 "Levy Rozman")) %>% View()

player_df %>% filter(name %in% c("Dmitry Andreikin", 
                                 "Daniel Naroditsky", 
                                 "Levy Rozman")) %>% t()
                     
                     
# How many unique players are there
nrow(player_df)

player_df %>% count(username) %>% count(n)
player_df %>% count(name) %>% count(n)

player_df %>% count(name, username) %>% 
  group_by(name) %>% add_count() %>% 
  filter(nn > 1)

TT %>% filter(name == "Alexandru Banzea") %>% View()
TT %>% filter(name == "Aleksandr Shimanov") %>% View()


# How often did they participate in a tournament
summary(player_df$N_participations)

# Which grandmaster participated the least often
player_df %>% filter(title == "GM") %>% arrange(N_participations) %>% View()

# # Anish Giri twice with different names, Nordibek really just once?

# 1) Which player won the most tournaments ----
player_df %>% arrange(desc(N_1st)) %>% 
  select(name, N_1st, title, fed, N_participations, winning_chance, median_score, avg_score, username)

player_df %>% filter(N_1st >= 1) %>% 
  ggplot(aes(x = fct_reorder(name, N_1st), y = N_1st, fill = fed)) +
  geom_col() +
  coord_flip()

# TT_df_backup %>% filter(Name == "Nihal Sarin") %>% count(Fed)
# TT_df_backup %>% filter(Name == "Oleksandr Bortnyk") %>% count(Fed)

library(RColorBrewer)
player_df %>% filter(N_1st >= 2) %>% 
  ggplot(aes(x = fct_reorder(name, N_1st), y = N_1st, fill = fed)) +
  geom_col() +
  coord_flip() +
  labs(fill = "Federation:", y = "# of wins", x = "", title = "Winners of the Titled Tuesday Tournaments",
       subtitle = "Filtered for multiple wins") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,19)) +
  scale_fill_brewer(palette = "Set3")


# With what score?
library(forcats)
TT %>% filter(number == 1) %>% count(name, sort = TRUE)

TT_1st_place <- TT %>% filter(number == 1) %>% 
  mutate(name_cat = fct_lump_n(name, n = 7), # without the next line, players are sorted alphabetically
         name_cat = factor(name_cat, levels = c("Hikaru Nakamura", "Magnus Carlsen", "Dmitry Andreikin", "Alireza Firouzja", "Daniil Dubov", "Maxime Vachier-Lagrave", "Nihal Sarin", "Other"), ordered = TRUE))

TT_1st_place %>% 
  count(name_cat, score) %>% 
  group_by(name_cat) %>% 
  add_count(name = "participations", wt = n) %>% 
  mutate(pct_score = n / participations,
         score_cat = factor(score, levels = c("11","10.5","10","9.5","9"), ordered = TRUE),
         name_N = paste0(name_cat, " (", participations, ")")) %>% 
  arrange(name_cat) %>% 
  mutate(name_N = factor(name_N, levels = unique(name_N))) %>%  
  ggplot(aes(x = name_N, y = pct_score, fill = score_cat)) +
  geom_col(position = "stack", width = .8) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(pct_score*100, 0),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "black") +
  theme_light() +
  labs(fill = "score when\nwinning\nfirst place:",
       subtitle = "Proportion of scores when winning the Titled Tuesday Event",
       y = "Winning score Percentage", x = "") +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())

# you always win with 11 or 10.5 points and you never win with 8.5
TT %>% filter(score == 10) %>% 
  summarize(winning_chance = mean(number == 1),
            sample_size = length(number),
            lowest_place = max(number))

TT %>% filter(score == 9.5) %>% 
  summarize(winning_chance = mean(number == 1),
            sample_size = length(number),
            lowest_place = max(number),
            avg_place = mean(number))

TT %>% filter(score == 9) %>% 
  summarize(winning_chance = mean(number == 1),
            sample_size = length(number),
            lowest_place = max(number),
            avg_place = mean(number))


# 2) Which player participated in the most tournaments ----
player_df %>% arrange(desc(N_participations)) %>% 
  select(name, title, fed, N_participations, best_Place, median_Place, median_score, rating_avg)

player_df %>% arrange(desc(N_participations)) %>% head(20) %>% 
  select(username:N_participations, avg_games_played, N_majority_played, N_all_games_played, avg_score)

player_df %>% count(N_participations) %>% 
  ggplot(aes(x = N_participations, y = n)) +
  geom_col() + 
  labs(y = "Number of players", x = "Number of participations",
       subtitle = "Tournament participations (1 or more games played)")

# what proportion participated more than 52 times (> weekly)
player_df %>% 
  summarize(percent_52_or_more = mean(N_participations >= 52),
            'percent_80+' = mean(N_participations >= 80),
            percent_1 = mean(N_participations == 1),
            percent_2 = mean(N_participations == 2),
            'percent_>12' = mean(N_participations >= 12))

# what proportion participated only once and how many games did they play?
player_df %>% filter(N_participations == 1) %>% 
  ggplot(aes(x = avg_games_played)) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:11)
  
# surprising. Most of the one time participants played all the games
player_df %>% filter(N_participations == 1) %>% 
  count(avg_games_played) %>% 
  mutate(pct = n /sum(n),
         pct_label = paste0(round(100 * n /sum(n), 1), "%"))

player_df %>% filter(N_participations == 1) %>% 
  count(avg_games_played) %>% 
  mutate(pct = n /sum(n),
         pct_label = paste0(round(100 * n /sum(n), 1), "%")) %>% 
  ggplot(aes(x = avg_games_played, y = pct, label = pct_label)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:11) + 
  geom_label(vjust = 1.2, size = 4) +
  labs(x = "Number of games played", y = "", subtitle = "Games played of players, that participated only once")

# 2b) Majority games played ----
player_df %>% count(N_majority_played) %>% 
  ggplot(aes(x = N_majority_played , y = n)) +
  geom_col() + 
  labs(y = "Number of players", x = "Number of participations",
       subtitle = "Tournament participations (6 or more games played)")

# 2c) All games played ----
player_df %>% count(N_all_games_played) %>% 
  ggplot(aes(x = N_all_games_played, y = n)) +
  geom_col() + 
  labs(y = "Number of players", x = "Number of participations",
       subtitle = "Tournament participations (all games played)")


# players at least playing one game, vs 50% games, vs all games (color coded, split by time) stacked bar chart
TT <- TT %>% 
  mutate(played_cat = factor(case_when(
    games_played == 11 ~ "all 11",
    games_played %in% 6:10 ~ "6-10",
    games_played %in% 1:5 ~ "1-5",
    games_played == 0 ~ "none"), levels = c("none", "1-5", "6-10", "all 11")))

TT %>% count(time, date, played_cat) %>%  
  ggplot(aes(x = date, fill = played_cat, y = n)) +
  geom_col(position = "stack") +
  facet_wrap(~time, nrow = 2) +
  theme_light()

TT %>% filter(games_played > 0) %>%
  count(time, date, played_cat) %>%  
  ggplot(aes(x = date, fill = played_cat, y = n)) +
  geom_col(position = "stack") +
  facet_wrap(~time, nrow = 2) +
  theme_light()


# 3) Who achieved a perfect score (11/11) ----
TT %>% filter(score == 11) %>% 
  select(name, fed, title, score, date, time)

TT %>% filter(date == "2023-07-04", time == "late",
              number %in% 1:10) %>% 
  select(name, fed, title, score, date, time, buchholz_cut_1)

TT %>% filter(date == "2023-08-22", time == "late",
              number %in% 1:10) %>% 
  select(name, fed, title, score, date, time, buchholz_cut_1)

## 3b) Average score ----
player_df %>% arrange(desc(avg_score)) %>% 
  select(name, fed, title, avg_score, N_participations)

# filter for 10 or more participations
player_df %>% 
  filter(N_participations >= 10) %>% 
  arrange(desc(avg_score)) %>% 
  select(name, fed, title, avg_score, N_participations, avg_games_played, rating_best)

View(.Last.value%>% slice_head(n = 20))

player_df %>% 
  filter(N_participations <10 & N_participations >=3 ) %>% 
  arrange(desc(avg_score)) %>% 
  select(name, fed, title, avg_score, N_participations, avg_games_played, rating_best)

View(.Last.value%>% slice_head(n = 20))


# 4) Who won both tournaments (early/late) on the same day ----
TT %>% filter(number == 1) %>% 
  count(name, date)

TT %>% filter(number == 1) %>% 
  count(name, date) %>% 
  filter(n == 2) %>% 
  arrange(date)

temp_dates <- .Last.value$date

TT %>% filter(date %in% temp_dates,
              number %in% 1) %>% 
  arrange(date, time) %>% 
  select(date, time, number, fed, title, name, username, score)

temp_names <- unique(.Last.value$name)
# order by same day participations
TT %>% filter(name %in% temp_names) %>% 
  count(name, date) %>% # show line by line
  filter(n == 2) %>% count(name, sort = TRUE)

# How often participated on both events on the same day
# Wesley, MVL and Magnus take the cake on this one! 
# But also consider how close someone came (Hikaru, Carlsen)

## 4b) winning on the same day  ----
TT %>% filter(number == 1) %>% 
  count(date, username, name) %>% 
  filter(n == 2)

win_both <- .Last.value$date

# these players came close by winning one and placing 2nd in the other tournament
TT %>% filter(number %in% 1:2) %>% 
  count(date, username, name) %>% 
  filter(n == 2) %>% arrange(date)

win_both2 <- .Last.value$date

came_close <- win_both2[!win_both2 %in% win_both]

TT %>% filter(date %in% came_close, number %in% 1:2) %>% 
  select(file_name:rating, score, buchholz_cut_1, time) %>% View()


# 5) __Streak__ ----
# Who had the longest participation streak
# by weekly participations (regarless of once or twice)
# and one time by strict streak early-late-early-late etc.!

## 5a) longest streak ( participations one after another (daywise, regardles of time)) ----
library(lubridate)

TT$week = week(TT$date)
#TT$Year = year(TT$date)
#TT$Year_Week = paste(TT$Year, TT$Week, sep = "_")

# filter out players that only participated once
users_multi_part <- player_df %>% filter(N_participations > 1) %>% pull(username)

#TT <- TT %>% left_join(week_df, by = "date")

## create running tournament numbering
TT$date_time <- paste(TT$date, TT$time, sep = "_")
tournament_df <- data_frame(date_time = TT %>% pull(date_time) %>% unique() %>% sort(),
                            date_time_ID = 1:103)

TT <- TT %>% left_join(tournament_df, by = "date_time")

## calculate streak
# Oliver Stork is a good example that should lead to a week streak of 4 (29-32)
week_streak <- TT %>% 
  filter(games_played > 0,
         username %in% users_multi_part) %>% 
  select(username, name, week) %>% 
  distinct() %>% 
  arrange(username, week) %>%
  group_by(username) %>%    # ?
  mutate(Streak = c(0,diff(week)),
         a = Streak == 1) 


 
# tournament id
file_name <- TT %>% arrange(date, time) %>% pull(file_name) %>% unique()
tt_id <- 1:103
tour_ID <- data.frame(file_name, tt_id)
TT <- TT %>% left_join(tour_ID)

tournament_streak <- TT %>% 
  filter(games_played > 0,
         username %in% users_multi_part) %>% 
  select(username, name, tt_id) %>% 
  arrange(username, tt_id) %>%
  group_by(username) %>%
  mutate(Streak = c(0, diff(tt_id)),
         a = Streak == 1) 

## 5b) streak function ----
longest_sequence <- function(a) {
  max_length <- 0
  current_length <- 0
  
  for (i in seq_along(a)) {
    if (a[i]) {
      current_length <- current_length + 1
      if (current_length > max_length) {
        max_length <- current_length
      }
    } else {
      current_length <- 0
    }
  }
  return(c(length = max_length))
}  

# results
week_streak %>% group_by(username) %>% 
  summarize(longest_week_streak = longest_sequence(a)) %>% # 03revilo = 4
  arrange(desc(longest_week_streak)) %>% left_join(TT %>% select(username, name) %>% distinct())
# 3rd place "Guillermo Baches (Garcia)"

tournament_streak %>% group_by(username) %>% 
  summarize(longest_tour_streak = longest_sequence(a)) %>% # 03revilo = 4
  arrange(desc(longest_tour_streak)) %>% left_join(TT %>% select(username, name) %>% distinct())


res_tournament <- tournament_streak %>% 
  summarize(longest_tournament_streak = longest_sequence(a)) %>% 
  arrange(desc(longest_tournament_streak)) %>% left_join(TT %>% select(username, name))

# results

# Hikaru only 14 and 8 
TT %>% select(username, date, time, week, date_time, tt_id, name) %>% arrange(tt_id) %>% 
  filter(username == "Hikaru") %>% View()
# 74 participations but not the longer streaks

## 5c) winning streaks ----
streak_df <- TT %>% select(file_name, username, name, title, rnd1:rnd11) %>% 
  pivot_longer(cols = -c(file_name, username, name, title), names_repair = "unique") %>% 
  mutate(result = substr(x =value, start = 1, stop = 1),
         win = result == "W")

names(streak_df)[c(3,5)] <- c("name", "round")

streak_df %>% group_by(username, title, name) %>% 
  summarize(winning_streak = longest_sequence(win)) %>% 
  arrange(desc(winning_streak))

streak_res <- .Last.value 


streak_res %>% ungroup() %>% 
  count(winning_streak) %>% 
  ggplot(aes(x = winning_streak, y = n)) + 
  geom_col(color = "black", fill = "darkgrey")

# some player could not even establish a winning streak
# Hikaru and Magnus clearly stand out

# fact check
streak_df %>% filter(name == "Magnus Carlsen") %>% View() # 219-235 rows
# including the perfect game on late-titled-tuesday-blitz-july-04-2023

streak_df %>% filter(name == "Hikaru Nakamura") %>% View() # 561-575 rows
# 13 wins: 365-378
# 15 wins: rows 561-575, including the perfect game on late-titled-tuesday-blitz-august-22-2023

## 5d) winning streaks by titles ----
streak_res %>% 
  ggplot(aes(x = winning_streak)) +
  geom_histogram(binwidth = .5)

# show histograms for GMs, vs IMs etc facet_wrap, use title ordered
streak_res %>% 
  filter(title %in% c("GM", "IM", "NM")) %>% 
  ggplot(aes(x = winning_streak, fill = title)) +
  geom_histogram(binwidth = .5) +
  facet_wrap(~title, ncol = 1, scales = "free_y") +
  theme(legend.position = "none") +
  theme_linedraw()
  theme_cleveland()


# 6) __Prize money__ ----
#https://www.chess.com/article/view/titled-tuesday#prizes

prize_money <- data.frame(number = 1:5,
                          prize = c(1000, 750, 350, 200, 100))

TT <- TT %>% left_join(prize_money, by = "number")

TT %>% group_by(username, name) %>% 
  summarize(total_prize = sum(prize, na.rm = TRUE)) %>% 
  arrange(desc(total_prize))

# There was only one tournament this week as the early event was cancelled, but it will be made up with a doubled prize fund (including $2,000 for first place) in the early event on January 31.

TT[TT$date == "2023-01-31" & TT$time == "early", ]   # Hikaru won that day!
TT[TT$date == "2023-01-31" & TT$time == "early", "prize"]  

TT$prize[TT$date == "2023-01-31" & TT$time == "early"][1:5]  <- c(2000, 1500, 700, 400, 200) # doubeling the prize money manually

## 6a) average prize per tournament ----
TT %>% group_by(username, name) %>% 
  summarize(mean = mean(prize, na.rm = TRUE),
            median = median(prize, na.rm = TRUE),
            N_prize = length(na.omit(prize)),
            N = length(prize)) %>% 
  arrange(desc(mean))

# problem with NAs being not counted
TT$prize2 <- TT$prize
TT$prize2[is.na(TT$prize2)] <- 0

TT %>% group_by(username, name) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_in_top5 = length(na.omit(prize)),
            N_participations = n()) %>% 
  arrange(desc(mean)) %>% 
  filter(mean > 0)

View(.Last.value)

## 6b) total prize money ----
TT %>% group_by(username, name) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_in_top5 = sum(prize2 > 0), 
            N_participations = n(),
            total_prize = sum(prize2)) %>% 
  arrange(desc(N_participations)) %>% 
  filter(mean > 0)

View(.Last.value)

# Sergei Zhigalko particpiated in almost all the tournaments and made $1450
# 129 players cashed in

### chart total ----
# total
TT %>% group_by(username, name) %>% 
  summarize(total_prize = sum(prize, na.rm = TRUE)) %>% 
  arrange(desc(total_prize)) %>% ungroup() %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(x = name, y = total_prize)) + 
  geom_col(aes(fill = total_prize)) 

# improvements
TT %>% group_by(username, name) %>% 
  summarize(total_prize = sum(prize, na.rm = TRUE)) %>% 
  arrange(desc(total_prize)) %>% ungroup() %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(x = fct_reorder(name, total_prize), y = total_prize)) + # perfect to do the reordering in here
  geom_col(aes(fill = total_prize), show.legend = FALSE) + # no legend
  xlab(label = "") + ylab(label = "") + labs(title = "Total prize money won per player in USD") + 
  scale_y_continuous(labels = scales::dollar, limits = c(0, 31000), expand = c(0, 0)) +
  coord_flip()

### chart average ----
# mean with glue
library(glue)
TT %>% group_by(username, name) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_participations = n()) %>% 
  arrange(desc(mean)) %>% ungroup() %>% 
  slice_head(n = 20) %>% 
  mutate(x_label = glue('{name}, (N={N_participations})'))

TT %>% group_by(username, name) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_participations = n()) %>% 
  arrange(desc(mean)) %>% ungroup() %>% 
  slice_head(n = 20) %>% 
  mutate(x_label = glue('{name} ({N_participations})')) %>% 
  ggplot(aes(x = fct_reorder(x_label, mean), y = mean)) + # perfect to do the reordering in here
  geom_col(aes(fill = mean), show.legend = FALSE) + # no legend
  scale_y_continuous(labels = scales::dollar, limits = c(0, 440), expand = c(0, 0),
                     name = "") +
  labs(title = "Average prize money won per player in USD", subtitle = "with (N participations)") +
  scale_x_discrete(name = "") +
  coord_flip()

## 6c) average prize split by time ----

# reorder within 
library(tidytext)
TT %>% group_by(username, name, time) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_participations = n()) %>% 
  arrange(desc(mean)) %>% ungroup() %>% 
  filter(mean > 0) %>% 
  slice_head(n = 40) %>% 
  mutate(x_label = glue('{name} ({N_participations})')) %>% 
  mutate(name_order = reorder_within(x_label, mean, time)) %>%
  ggplot(aes(x = name_order, y = mean, fill = mean)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ time, scales = "free_y") + 
  scale_x_reordered() +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 550), expand = c(0, 0),
                     name = "Average prize money won per player participation") + 
  coord_flip() + xlab(label = NULL)

# solution group and top_n()
TT %>% group_by(username, name, time) %>% 
  summarize(mean = mean(prize2, na.rm = TRUE),
            N_participations = n()) %>% 
  arrange(desc(mean)) %>% ungroup() %>% 
  filter(mean > 0) %>% 
  group_by(time) %>% 
#  top_n(n = 20) %>% # top_n will sort by participation not by mean
  top_n(20, wt = mean) %>% # use weight on mean variable
  mutate(x_label = glue('{name} ({N_participations})')) %>% 
  mutate(name_order = reorder_within(x_label, mean, time)) %>%
  ggplot(aes(x = name_order, y = mean, fill = mean)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ time, scales = "free_y") + 
  scale_x_reordered() +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 550), expand = c(0, 0),
                     name = "Average prize money won per player participation") + 
  coord_flip() + xlab(label = NULL)


## 6d) repeat for total ----
TT %>% group_by(username, name, time) %>% 
  summarize(total = sum(prize, na.rm = TRUE),
            N_participations = n()) %>% 
  arrange(desc(total)) %>% ungroup() %>% 
  filter(total > 0) %>% 
  slice_head(n = 40) %>% 
  mutate(x_label = glue('{name} ({N_participations})')) %>% 
  mutate(name_order = reorder_within(x_label, total, time)) %>%
  ggplot(aes(x = name_order, y = total, fill = total)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ time, scales = "free_y") + 
  scale_x_reordered() +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 17000), expand = c(0, 0)) + 
  coord_flip() + xlab(label = NULL) +
  labs(subtitle = "Total prize money won per player (N participation)")

# Daniil Dubov is absent in late tournaments, Aram Hakobyan (ARM), Matthias Bluebam and Dimitrij Kollars (German players) is absent in early (top 20 winners that is)

## 6e) lowest title/rating winning money  ----
TT %>% filter(number %in% 1:5) %>% count(title)
TT %>% filter(is.na(title), number %in% 1:5) %>% select(date, time, number, username, name, fed, rating, score)

player_df %>% filter(fed == "URY") %>% arrange(desc(rating_avg)) %>% 
  select(username, name, title, fed, N_participations, avg_score, rating_avg, total_prize, N_Top10)

#https://ratings.fide.com/topfed.phtml?ina=1&country=uru

TT %>% filter(number %in% 1:5,
              title != "GM") %>%
  select(date, time, number:title, name, rating, score, time, prize) %>% 
  arrange(title, name) %>% View()
# Oleksandr Bortnyk has an older brother, Mykola Bortnyk (IM)

# lowest rating winning money
# calculate average rating per player over the last 100 tournaments
TT %>% count(name, rating) %>%  
  count(name, sort = TRUE) 

# Arad Nazari (8 different ratings)
TT %>% filter(!is.na(prize)) %>% arrange(rating) %>% 
  select(file_name:rating, score, time, prize) %>% View()

TT %>% filter(!is.na(prize)) %>% arrange(rating) %>% 
  select(date, time, number, fed, title, name, rating, score, time, prize) %>% 
  distinct(number, name, rating, .keep_all = T) %>% View()

player_df %>% filter(total_prize > 0) %>% 
  arrange(rating_avg) %>% select(username, name, title, fed, rating_avg, total_prize, N_participations, best_Place, Rating_best)

# 7) __Winning % with white/black__ ----
games_color <- results_df %>% pivot_longer(cols = -c(username, games_w_White, games_w_Black)) %>% 
  mutate(percent_white = value / games_w_White,
         percent_black = value / games_w_Black,
         color = factor(gsub(pattern = ".+_", x = name, replacement = ""), levels = c("White", "Black")),
         result = factor(gsub(pattern = "_.+", x = name, replacement = ""), levels = c("lose", "draw", "win"))) 
names(games_color)[4] <- "result_color"

# make a stacked barchart for top 10 players (how common is it in this tournament)
top20 <- player_df %>% filter(N_participations > 25) %>% arrange(desc(avg_score)) %>% head(20) %>% pull(username)

## 7a) wins with white ----
games_color_N20 <- games_color %>% filter(username %in% top20)
games_color_N20$name <- player_df$name[match(games_color_N20$username, player_df$username)]

player_white_win <- games_color_N20 %>% filter(color == "White", result == "win") %>% arrange(percent_white) %>% pull(name)
player_white_loss <- games_color_N20 %>% filter(color == "White", result == "lose") %>% arrange(percent_white) %>% pull(name)

games_color_N20$name_win_White <- factor(games_color_N20$name, levels = player_white_win)
games_color_N20$result_win <- factor(games_color_N20$result, levels = c("lose", "draw", "win"))
# sort by lowest loss-% and by highest win-%

p_white_win <- games_color_N20 %>% filter(color == "White") %>% 
  ggplot(aes(x = percent_white, fill = result_win, y = name_win_White)) +
  geom_col(position = "stack") +  
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent_white*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "", y = "", fill = "Result:")


games_color_N20$name_lose_White <- factor(games_color_N20$name, levels = rev(player_white_loss))
games_color_N20$result_lose <- factor(games_color_N20$result, levels = c("win", "draw", "lose"))
p_white_loss <- games_color_N20 %>% filter(color == "White") %>% 
  ggplot(aes(x = percent_white, fill = result_lose, y = name_lose_White)) +
  geom_col(position = "stack") +  
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent_white*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  theme_light() +
  scale_fill_manual(values = c("#00BA38", "#619CFF", "#F8766D")) +
  labs(x = "", y = "", fill = "Result:")

library(gridExtra)
grid.arrange(p_white_win + theme(legend.position = "none"), 
             p_white_loss + theme(legend.position = "none"), ncol=2)

library(patchwork)
p_white_win + p_white_loss

## 7b) repeat for black ----
player_black_win <- games_color_N20 %>% filter(color == "Black", result == "win") %>% arrange(percent_black) %>% pull(name)
player_black_loss <- games_color_N20 %>% filter(color == "Black", result == "lose") %>% arrange(percent_black) %>% pull(name)

games_color_N20$name_win_Black <- factor(games_color_N20$name, levels = player_black_win)
games_color_N20$result <- factor(games_color_N20$result, levels = c("lose", "draw", "win"))
# sort by lowest loss-% and by highest win-%
p_black_win <- games_color_N20 %>% filter(color == "Black") %>% 
  ggplot(aes(x = percent_black, fill = result, y = name_win_Black)) +
  geom_col(position = "stack") +  
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent_black*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  theme_light() +
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "", y = "", fill = "Result:")

games_color_N20$name_lose_Black <- factor(games_color_N20$name, levels = rev(player_black_loss))
games_color_N20$result <- factor(games_color_N20$result, levels = c("win", "draw", "lose"))
p_black_loss <- games_color_N20 %>% filter(color == "Black") %>% 
  ggplot(aes(x = percent_black, fill = result, y = name_lose_Black)) +
  geom_col(position = "stack") +  
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent_black*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  theme_light() +
  scale_fill_manual(values = c("#00BA38", "#619CFF", "#F8766D")) +
  labs(x = "", y = "", fill = "Result:")
  


(p_black_win + theme(legend.position = "none")) / (p_black_loss + theme(legend.position = "none") )

p_black_win | p_black_loss

## stacked bar charts wins, draws, losses in percent, facet_wrap black and white or by player split (facet grid)
# top 10 players black and white side by side (grid)
games_color_N20$percent <- games_color_N20$percent_white
games_color_N20$percent[games_color_N20$color == "Black"] <- games_color_N20$percent_black[games_color_N20$color == "Black"]

player_winning_chance <- player_df %>% filter(name %in% games_color_N20$name) %>% arrange(desc(winning_chance)) %>%   pull(name) %>% unique()
games_color_N20$name_best <- factor(games_color_N20$name, levels = rev(player_winning_chance))

games_color_N20 %>% 
  mutate(result2 = factor(result, levels = c("lose", "draw", "win"))) %>% 
  ggplot(aes(x = percent, fill = result2, y = name_best)) +
  geom_col(position = "stack") +  
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  facet_wrap(~color) +
  theme_light() +
  scale_fill_manual(values = rev(c("#00BA38", "#619CFF", "#F8766D"))) +
  labs(fill = "Result:", x= "", y="")

