
load(file = "TitledTuesday.RData")

## 1a) How many tournaments (early vs late) ----
dim(TT)

unique(TT$file_name)
length(unique(TT$file_name))
dplyr::n_distinct(TT$file_name)
TT %>% count(date, time)
TT %>% count(date, time) %>% nrow()
52*2

TT %>% count(file_name, time, date) %>% arrange(date) # we can see only late for 24.1. (lucky happened in the top ten)
TT %>% count(time, file_name) %>% count(time)
TT[!duplicated(TT$file_name), "time"] %>% table()

# Which early tournament was missing?
TT %>% count(date, time)
TT %>% count(date, time) %>% 
  count(date) %>% filter(n == 1)

TT %>% count(date, time) %>% pivot_wider(names_from = time, values_from = n)

# There was only one tournament this week as the early event was cancelled, but it will be made up with a doubled prize fund (including $2,000 for first place) in the early event on January 31.
# https://www.chess.com/blog/CHESScom/chess-is-booming-and-our-servers-are-struggling

## 1b) How many players participate ----
TT %>% count(file_name)

TT %>% count(date, time) %>% pivot_wider(names_from = time, values_from = n)

TT %>% count(date, time) %>% group_by(time) %>% 
  summarize(minimum = min(n), 
            median = median(n), mean = mean(n), 
            max = max(n))


## 1c ) Visualizing number of participants ----
TT %>% count(file_name) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(color = "black") # looks bimodal, add time variable (one outlier)

TT %>% count(date, time) %>% 
  ggplot(aes(x = n, fill = time)) +
  geom_histogram(color = "black") # visualization is so important 

TT %>% count(date, time) %>% 
  ggplot(aes(x = n, fill = time)) +
  geom_histogram(color = "black", position = "dodge") # bit better

# best are the next 3
TT %>% count(date, time) %>% 
  ggplot(aes(x = n, fill = time)) +
  geom_histogram(color = "black") + facet_wrap(~ time, nrow = 2)

# geom_density()
TT %>% count(date, time) %>% 
  ggplot(aes(x = n, fill = time)) +
  geom_density(alpha = 0.5)

# geom_col() by date
TT %>% count(date, time) %>% 
  ggplot(aes(x = date, y = n, fill = time)) +
  geom_col(color = "black") + facet_wrap(~ time, nrow = 2)


## 2a) games played ----
TT %>% select(contains("rnd"))
TT %>% filter(number == 450) %>% select(contains("rnd")) 
rowSums(TT[ ,9:19] != "U--")
grep("rnd", names(TT))
rowSums(TT[ ,grep("rnd", names(TT))] != "U--")

TT %>% select(contains("rnd")) != "U--"
rowSums(.Last.value)

TT %>% dplyr::count(games_played)
table(TT$games_played)
table(TT_df$games_played)

TT %>% ggplot(aes(x = games_played)) + geom_histogram()
TT %>% ggplot(aes(x = games_played)) + geom_bar()

TT %>% dplyr::count(games_played) %>% 
  ggplot(aes(x = games_played, y = n)) +
  geom_col()

## 2b) percentage participating in each round ----
colMeans(TT[ ,9:19] != "U--") # nice way to show that 87% of first games are played, vs 53% of last game,

data.frame(round = 1:11, pct_playing = colMeans(TT[ ,9:19] != "U--"))
data.frame(round = 1:11, pct_playing = colMeans(TT[ ,9:19] != "U--")) %>% 
  ggplot(aes(x = factor(round),
             y = pct_playing,
             label = scales::label_percent()(pct_playing))) +
  geom_col() +
  labs(subtitle = "% of players participating in each round",
       x = "round", y = "percent playing") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(vjust = 1.5, color = "white")

# some player miss the first round (2nd round is most participated)
  
## 2c) games played charts ----
# games played by each player
table(rowSums(TT[ ,9:19] != "U--")) %>% as.data.frame()
table(rowSums(TT[ ,9:19] != "U--")) %>% as.data.frame() %>% 
  mutate(percent = Freq/nrow(TT),
         cum_sum = cumsum(percent))
paste0(round(table(rowSums(TT[ ,9:19] != "U--")) /nrow(TT)*100,2)," %")

TT %>% ggplot(aes(x = games_played)) +
  geom_histogram(stat = "count")

TT %>% ggplot(aes(x = factor(games_played))) +
  geom_histogram(stat = "count")

TT %>% count(games_played)
TT %>% count(games_played) %>% 
  ggplot(aes(x = factor(games_played), y = n)) +
  geom_col()

library(scales)
TT %>% ggplot(aes(x = factor(games_played))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) 

TT %>% ggplot(aes(x = factor(games_played))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent(round(..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25)

# 2d) How often are all games played ----
TT %>% group_by(date, time) %>% 
  summarize(mean = mean(games_played),
            median = median(games_played),
            percent_11_games = mean(games_played == 11))

# not so useful (hard to compare)
TT %>% group_by(date, time) %>% 
  summarize(mean = mean(games_played),
            median = median(games_played),
            percent_11_games = mean(games_played == 11)) %>% 
  ggplot(aes(x = date, y = mean, fill = time)) + 
  geom_col() + facet_wrap(~ time, nrow = 2)

# bit better, but still tricky
TT %>% group_by(date, time) %>% 
  summarize(mean = mean(games_played),
            median = median(games_played),
            percent_11_games = mean(games_played == 11)) %>% 
  ggplot(aes(x = date, y = percent_11_games, fill = time)) + 
  geom_col() + facet_wrap(~ time, nrow = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))

# geom_line
TT %>% group_by(date, time) %>% 
  summarize(mean = mean(games_played),
            median = median(games_played),
            percent_11_games = mean(games_played == 11)) %>% 
  ggplot(aes(x = date, y = percent_11_games, color = time)) + 
  geom_line(size = 1.3) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                     name = "What proportion of players finish all 11 games")


# 3a) What is the average score to win ---- 
TT %>% filter(number == 1) # 103 tournaments and winners, name, score etc.

TT %>% filter(number == 1)  %>% select(date, time, number:rating, score, games_played)
# Hikaru winning 6,7,8

TT %>% filter(number == 1) %>% 
  ggplot(aes(x = score)) + geom_histogram(color = "black")

TT %>% filter(number == 1) %>% 
  ggplot(aes(x = score, fill = time)) + 
  geom_histogram(position = "dodge", color = "black", binwidth = 0.2)

TT %>% filter(number == 1) %>% pull(score) %>% mean() # show by step
TT %>% filter(number == 1) %>% group_by(time) %>% 
  summarize(mean(score))

# perfect score happend only twice (in a late tournament)
# early tournament might be harder to beat (more competitive), check percent grandmasters!

# 3b) Lowest score to win ----
TT %>% filter(number == 1,
              score == 9) %>% 
  select(name, fed, title, score, date, time) # these were big draws

TT %>% filter(date == "2023-04-18", time == "late",
              number %in% 1:10) %>% 
  select(username, name, fed, title, score, date, time, buchholz_cut_1, rating, games_played)
# 8 player draw

TT %>% filter(date == "2023-08-08", time == "early",
              number %in% 1:14) %>% 
  select(username, name, fed, title, score, date, buchholz_cut_1, rating, games_played)
# 12 player draw
# meti force/Mahdi Gholami Orimi only played 10 games

# 3c) How common are Ties ----
# How often is the first place tied, and what was the most common tie?

# different way (even better)
TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name) 

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name) %>% 
  ungroup() %>% count(n)

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name) %>% ungroup() %>% 
  count(n) %>% 
  mutate(percent = round((nn / sum(nn) * 100),1)) # unnecessary given that we have 103 tournaments
# 27% of the time 2 player have the same score and need a tie breaker (Sonneborn-Berger)
# average unique winner score probably higher than the tie (2) or tie(3) scores

# chart
TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name) %>% 
  ggplot(aes(x = n)) + geom_histogram() + # histogram intervalls in a weird way
  scale_x_continuous(breaks=1:13) +
  xlab(label = "Number of players with the highest score in one tournament")

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name) %>% ungroup() %>% 
  count(n) %>% 
  ggplot(aes(x = n, y = nn)) + geom_col(width = 0.5) +
  scale_x_continuous(breaks = 1:13) + 
  xlab(label = "Number of players with the highest score in one tournament")

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name, score) %>% arrange(desc(n))

# both 9 score win ties already shown

# 3d) Ties early vs late ----
TT %>% group_by(date, time) %>% 
  filter(score == max(score)) %>% 
  count(file_name)

# needs new grouping by time
TT %>% group_by(date, time) %>% 
  filter(score == max(score)) %>% 
  count(file_name) %>% group_by(time) %>% 
  count(n) %>% 
  mutate(percent = round((nn / sum(nn) * 100),1))

# 3e) analyze average score for no-tie, 2-tie, 3-tie etc! ----
TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name, score) %>% arrange(desc(n))

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name, score) %>% arrange(desc(n)) %>% 
  #filter(score > 9) %>% 
  group_by(n) %>% 
  summarize(min = min(score), mean = mean(score), max = max(score),
            N = n())

# 10 points is usually the average to win without a tie
# 9.5 is the score for 2-4 players tieing
# 9 points are necessary for rare exceptions (tie 8 & 12 player)

# find the cases where a single player won with a score of 9.5
TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  count(file_name, score) %>% 
  ungroup() %>% filter(score == 9.5, n == 1) # happend 14 times

temp_filenames <- .Last.value$file_name
TT %>% filter(file_name %in% temp_filenames, number %in% 1:2) %>% 
  select(date, time, name, number, score, title, rating) %>% View()

# 3f) Perfect score, really rare ----
TT %>% filter(score == 11) %>% 
  select(name, fed, title, score, date, time)

TT %>% filter(date == "2023-07-04", time == "late",
              number %in% 1:10) %>% 
  select(name, fed, title, score, date, time, buchholz_cut_1)

TT %>% filter(date == "2023-08-22", time == "late",
              number %in% 1:10) %>% 
  select(name, fed, title, score, date, time, buchholz_cut_1)


## 4) Countries / Fed ----

# Where do they come from
# difference of unique players, and total number of players over all tournaments

## 2 pie charts for unique players (with other if below 1%), split by early and late
player_df %>% 
  count(fed, sort = TRUE) %>% 
  mutate(percent = n/sum(n))

pie_top30 <- player_df %>% 
  mutate(fed_other = fct_lump(fed, n = 30, other_level = "147 other\ncountries")) %>% 
  count(fed_other, sort = TRUE) %>% 
  mutate(percent = n/sum(n)) 

pie_top30 <- pie_top30 %>% 
  mutate(fed2 = factor(fed_other, levels = c(as.character(pie_top30$fed_other[-1]), "147 other\ncountries")))

library(countrycode)
?countrycode

pie_top30$country <- countrycode(sourcevar = pie_top30$fed2, origin = 'iso3c', destination = 'country.name')
pie_top30$country[1] <- "147 other\ncountries"
pie_top30$country[12] <- "International"

library(ggpubr)
ggpie(data = pie_top30, x = "percent", label = "fed2", fill = "fed2")

library(plotly)
plotly::plot_ly(data = pie_top30, 
                labels = ~country, 
                values = ~percent,
                type = "pie", 
                sort = TRUE,
                marker = list(colors = colors,
                              line = list(color = "black", width = 1))) %>% 
  layout(title = "Pie chart with polt_ly()")


## 4a) early late split, do based on TT, for top 10 countries, over 50 days, average % (area chart) ----
TT %>% count(fed, sort = TRUE)

top15_fed <- TT %>% filter(fed != "INT") %>% count(fed, sort = TRUE) %>% pull(fed) %>% head(15) # excluding INT
top15_fed
top15_countries <- countrycode::countrycode(sourcevar = top15_fed, origin = 'iso3c', destination = 'country.name')
top15_countries

unique_tournaments <- TT %>% select(file_name, date, time) %>% distinct()

# show line by line
top15_area <- TT %>% count(file_name, fed) %>% 
  group_by(file_name) %>% 
  add_count(wt = n) %>% # important to use wt
  mutate(percent = n /nn) %>% # percent per trounament
  filter(fed %in% top15_fed) %>% 
  left_join(unique_tournaments, by = "file_name")

top15_area$country <- countrycode::countrycode(sourcevar = top15_area$fed, origin = 'iso3c', destination = 'country.name')

dim(top15_area)
15*103


# reorder by mean(percentage per country, bottom to top)
country_order_early <- top15_area %>% filter(time == "early") %>% group_by(country) %>% 
  summarize(avg = mean(percent)) %>% arrange(desc(avg)) %>% 
  pull(country)
country_order_late <- top15_area %>% filter(time == "late") %>% group_by(country) %>% 
  summarize(avg = mean(percent)) %>% arrange(desc(avg)) %>% 
  pull(country)
country_order_total <- top15_area %>% group_by(country) %>% 
  summarize(avg = mean(percent)) %>% arrange(desc(avg)) %>% 
  pull(country)

top15_area$country_early <- factor(x = top15_area$country, levels = country_order_early)
top15_area$country_late <- factor(x = top15_area$country, levels = country_order_late)
top15_area$country_total <- factor(x = top15_area$country, levels = country_order_total)

# patch together side by side to show india higher in early (time zone)
top15_area %>% 
  filter(time == "early") %>% 
  ggplot(aes(x = date, y = percent, fill = country_early)) +
  geom_area(alpha = 0.8, size = 0.5, color = "white", position = "stack")

top15_area %>% 
  filter(time == "late") %>% 
  ggplot(aes(x = date, y = percent, fill = country_late)) +
  geom_area(alpha = 0.8, size = 0.5, color = "white", position = "stack")

# fix color so it is the same across images
library(scales)
rainbow_15 <- hue_pal()(15)                             # Identify hex codes
show_col(rainbow_15)  

names(rainbow_15) <- country_order_total
# use to reorder
rainbow_15
rainbow_15[country_order_early]
rainbow_15[country_order_late]

top15_area %>% 
  filter(time == "early") %>% 
  ggplot(aes(x = date, y = percent, fill = country_early)) +
  geom_area(alpha = 0.8, size = 0.5, color = "white", position = "stack") +
  scale_fill_manual(values = rainbow_15[country_order_early]) +
  scale_y_continuous(labels = scales::percent)

top15_area %>% 
  filter(time == "late") %>% 
  ggplot(aes(x = date, y = percent, fill = country_late)) +
  geom_area(alpha = 0.8, size = 0.5, color = "white", position = "stack") +
  scale_fill_manual(values = rainbow_15[country_order_late]) +
  scale_y_continuous(labels = scales::percent)

# biggest change is India moving down from 3 to last
# and Russia moving from 1 to 2 (early to late)
# the y-axis shows that top 15 countries cover roughly 60% of all participants
# mention, that one could calculate monthly averages, to make it more smooth

## 4b) map ----
## color code world-map, split, early late and by overall average, make it median N
# make it interactive so I can hover over it

# check spelling of country names for OECD members availability in world maps
library(maps)
world <- map_data("world") %>% 
  filter(region != "Antarctica")

# early vs late maps
TT %>% count(file_name, fed, time) %>% 
  group_by(fed, time) %>% 
  summarize(median_N = median(n)) %>% 
  pivot_wider(names_from = time, values_from = median_N) %>% View() # show results, differences in time
# also important to mention that median is most stable across these tournaments
country_N <- TT %>% count(file_name, fed, time) %>% 
  group_by(fed, time) %>% 
  summarize(median_N = median(n)) %>% 
  pivot_wider(names_from = time, values_from = median_N) %>% 
  filter(!fed %in% c("INT", "BSQ", "EUR")) 

country_N$country <- countrycode(sourcevar = country_N$fed, origin = 'iso3c', destination = 'country.name')
country_N$country[!country_N$country %in% world$region]

country_N <- country_N %>% 
  mutate(country2 = case_when(
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    country == "Antigua & Barbuda" ~ "Antigua",
    country == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    country == "Czechia" ~ "Czech Republic",
    country == "Trinidad & Tobago" ~ "Trinidad",
    TRUE ~ country)) 

colors <-  colorRampPalette(c("darkgreen", "pink", "purple"))(5)
colors <- hue_pal()(6)                             # Identify hex codes
show_col(colors) 

map_early <- country_N %>% 
  right_join(world, by = c(country2 = "region")) %>% 
  ggplot(aes(long, lat, group = group, fill = early, label = country2)) + 
  geom_polygon(color = "grey", size = 0.2) +
  theme_bw() + 
  scale_fill_stepsn(colours = colors, 
                    limits = c(1, 60), 
                    breaks = c(1, 2, 5, 10, 25, 60),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE)) +
  labs(subtitle = "Where players come from that participate in the early title Tuesday event.",
       fill = "Median\nparticipants", x = "", y = "") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) # + coord_fixed()

map_late <- country_N %>% 
  right_join(world, by = c(country2 = "region")) %>% 
  ggplot(aes(long, lat, group = group, fill = late, label = country2)) + 
  geom_polygon(color = "grey", size = 0.2) +
  theme_bw() + 
  scale_fill_stepsn(colours = colors, 
                    limits = c(1, 60), 
                    breaks = c(1, 2, 5, 10, 25, 60),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE)) +
  labs(subtitle = "Where players come from that participate in the late title Tuesday event.",
       fill = "Median\nparticipants", x = "", y = "") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) # + coord_fixed()

map_early
map_late


# 4c) winnings per fed ----
# Like in the olympics (gold decides first)

TT %>% filter(number %in% 1:3) %>% 
  count(number, fed) %>% 
  pivot_wider(values_from = n, names_from = number) %>% 
  arrange(desc(`1`), desc(`2`), desc(`3`)) %>% View()

# stacked barchart, nations/federations most wins
TT %>% filter(number %in% 1:3) %>% 
  count(number, fed, time) %>% 
  ggplot(aes(x = fed, y = n , fill = as.factor(number))) +
  geom_col(position = "stack") +
  coord_flip()

# split by early vs late (reorder within)
# add text

TT %>% filter(number %in% 1:3) %>% 
  count(number, fed, time) %>% 
  ggplot(aes(x = fed, y = n, fill = as.factor(number),
             label = n)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(size = 2, hjust = 1.5, position = position_stack(reverse = TRUE)) + 
  facet_wrap( ~ time) + 
  coord_flip()

library(ggpp)
TT %>% filter(number %in% 1:3) %>% 
  count(number, fed, time) %>% 
  ggplot(aes(x = fed, y = n, fill = as.factor(number),
             label = n)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(size = 2, hjust = 1.5, position = position_stacknudge(vjust = 1, reverse = TRUE)) + 
  facet_wrap( ~ time) + 
  coord_flip()

# reorder within
library(tidytext)
TT %>% filter(number %in% 1:3) %>% 
  count(number, fed, time) %>% 
  mutate(fed = reorder_within(x = fed, by = n, within = list(time, fed))) %>%
  #mutate(fed = reorder_within(x = fed, by = n, within = time)) %>%
  ggplot(aes(x = fed, y = n, fill = as.factor(number),
             label = n)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(size = 2.5, hjust = 1.2, position = position_stacknudge(vjust = 1, reverse = TRUE)) + 
  facet_wrap( ~ time, scales = "free_y") + 
  scale_x_reordered() +
  coord_flip()

# gold > silver > bronze  
medals <- TT %>% filter(number %in% 1:3) %>% 
  count(number, fed, time) %>% 
  mutate(multiplier = if_else(number == 1, 1000, if_else(number == 2, 100, 1))) %>% 
  mutate(score = n * multiplier) %>% 
  mutate(fed = reorder_within(x = fed, by = score, within = time))


medals %>% ggplot(aes(x = fed, y = n, fill = as.factor(number),
                        label = n)) +
  #geom_col(position = "stack", position_fill(reverse = TRUE)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(size = 2.5, hjust = 1.2, position = position_stacknudge(vjust = 1, reverse = TRUE)) + 
  facet_wrap( ~ time, scales = "free_y") + 
  scale_x_reordered() +
  coord_flip()


TT %>% filter(number %in% 1:3, fed == "DEU") %>% select(date, time, number, fed, name, score) %>%  
  arrange(time, number) %>% View()
TT %>% filter(number %in% 1:3, fed == "IND") %>% select(date, time, number, fed, name, score) %>%  
  arrange(time, number) %>% View()
TT %>% filter(number %in% 1:3, fed == "INT") %>% select(username, name, fed) %>% distinct()
TT %>% filter(number %in% 1:3, fed == "IDN") %>% select(date, number, score, username, name, fed, rating)

medals_early_fed <- TT %>% filter(number %in% 1:3, time == "early") %>% 
  count(number, fed) %>% 
  pivot_wider(values_from = n, names_from = number) %>% 
  arrange(desc(`1`), desc(`2`), desc(`3`)) %>% pull(fed)

medals_late_fed <- TT %>% filter(number %in% 1:3, time == "late") %>% 
  count(number, fed) %>% 
  pivot_wider(values_from = n, names_from = number) %>% 
  arrange(desc(`1`), desc(`2`), desc(`3`)) %>% pull(fed)

# stacked barchart, nations/federations most wins
library(ggtext)
early_plot <- TT %>% filter(number %in% 1:3, time == "early") %>% 
  count(number, fed) %>% 
  mutate(fed2 = factor(fed, levels = rev(medals_early_fed))) %>% 
  ggplot(aes(x = fed2, y = n, fill = as.factor(number),
           label = n)) +
  geom_col(position = position_stack(reverse = TRUE), show.legend = FALSE) +
  geom_text(size = 2.5, hjust = 1.2, position = position_stacknudge(vjust = 1, reverse = TRUE)) + 
  scale_x_reordered() +
  coord_flip() + xlab(label = "") + 
  labs(subtitle = "**Early** Titled Tuesday: <span style='color:#F8766D;'>**1st**</span>,
  <span style='color:#00BA38;'>**2nd**</span>, and
  <span style='color:#619CFF;'>**3rd**</span> Places") +
  theme(plot.subtitle = element_markdown())
  
late_plot <- TT %>% filter(number %in% 1:3, time == "late") %>% 
  count(number, fed) %>% 
  mutate(fed2 = factor(fed, levels = rev(medals_late_fed))) %>% 
  ggplot(aes(x = fed2, y = n, fill = as.factor(number),
             label = n)) +
  geom_col(position = position_stack(reverse = TRUE), show.legend = FALSE) +
  geom_text(size = 2.5, hjust = 1.2, position = position_stacknudge(vjust = 1, reverse = TRUE)) + 
  scale_x_reordered() +
  coord_flip() + xlab(label = "") + 
  labs(subtitle = "**Late** Titled Tuesday: <span style='color:#F8766D;'>**1st**</span>,
  <span style='color:#00BA38;'>**2nd**</span>, and
  <span style='color:#619CFF;'>**3rd**</span> Places") +
  theme(plot.subtitle = element_markdown())

early_plot | late_plot

## 4d) fed early/late ratio split ----
TT %>% count(fed, sort = T)
TT %>% group_by(time) %>% count(fed, sort = T) %>% 
  pivot_wider(names_from = time, values_from = n) %>% 
  mutate(ratio = late / early,
         percent_late = late / (late + early),
         percent_early = early / (early + late),
         total = late + early) %>% 
  arrange(desc(total)) %>% 
  View()

# show line by line
TT %>% filter(date != "2023-01-24") %>% # because of only one event
  group_by(time) %>% count(fed, sort = T) %>% 
  pivot_wider(names_from = time, values_from = n) %>% 
  mutate(ratio = late / early,
         percent_late = late / (late + early),
         percent_early = early / (early + late),
         total = late + early) %>% 
  filter(total > 250) %>% # 40 countries
  ggplot(aes(x = fct_reorder(fed, (percent_early)), y = percent_early)) + 
  geom_col(aes(fill = percent_early), show.legend = FALSE) +
  coord_flip() + 
  geom_hline(yintercept = 0.5, size = 1) +
  scale_y_continuous(label = scales::percent) + 
  labs(x = "", subtitle = "What proportion of players from each country participate in the early event",
       y = "")


## 5a) Titles ----

TT %>% count(title, sort = T)

TT$title_ordered <- factor(TT$title, levels = c("GM", "IM", "FM", "CM", "NM", "WGM", "WIM", "WFM", "WCM", "WNM"))
TT %>% count(title_ordered)

TT %>% filter(!is.na(title)) %>% 
  count(title_ordered) %>% 
  mutate(percent = n /sum(n) * 100)

# 5a) Proportion of titles per tournament ----

TT %>% filter(!is.na(title), gender == "Men") %>% 
  group_by(date, time, gender) %>% # because of grouping "/ sum(n)" works
  count(title_ordered) %>% 
  mutate(percent = n / sum(n)) # each date/time adds up to 1

percent_titles_men <- .Last.value

ggplot(data = percent_titles_men, 
       aes(x = date, y = percent, fill = title_ordered)) + 
  geom_area(size=0.5, color = "white") +
  #  facet_wrap(~time)
  facet_wrap(~time, nrow = 2) +
  labs(fill = "Title:") + # because fill is based on title_ordered
  scale_y_continuous(label = scales::percent)

# pretty stable distribution of mostly FM participating

# highest percent of grandmasters when and how high was it?
# good summary with enough details, easier to judge than area plots
ggplot(data = percent_titles_men, aes(x = title_ordered, y = percent, 
                                      color = title_ordered, fill = title_ordered)) + 
  geom_boxplot(alpha=0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.8) + 
  facet_wrap(~time) +
  scale_y_continuous(label = scales::percent)

library(plotly)
p <- ggplot(data = percent_titles_men, aes(x = title_ordered, y = percent, 
                                           color = title_ordered, fill = title_ordered,
                                           label = date)) + 
  geom_boxplot(alpha=0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.8) + 
  facet_wrap(~time) 

ggplotly(p) # interactive, shows highest, lowest and date
# march 21 an 14 in the early tournament ~24% of players were GMs

# table result
percent_titles_men %>% filter(title_ordered == "GM") %>% 
  arrange(desc(percent))


## 5b) Average score/category by Titles ----
TT %>% 
  filter(!is.na(title)) %>% 
  group_by(title) %>% 
  summarize(avg_score = mean(score)) %>% 
  arrange(desc(avg_score))

# boxplots
TT %>% 
  filter(!is.na(title)) %>% 
  ggplot(aes(x = title_ordered, y = score, fill = title_ordered)) +
  geom_boxplot(show.legend = FALSE)

player_df %>% 
  filter(!is.na(title)) %>% 
  ggplot(aes(x = title_ordered, y = avg_score, fill = title_ordered)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(alpha = 0.3, show.legend = FALSE)


## 5c) average Rating by title ----

TT %>% 
  filter(!is.na(title)) %>% 
  group_by(title) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  arrange(desc(avg_rating))

TT %>% 
  filter(!is.na(title)) %>% 
  ggplot(aes(x = title_ordered, y = rating, fill = title_ordered)) +
  geom_boxplot(show.legend = FALSE)

player_df %>% 
  filter(!is.na(title)) %>% 
  ggplot(aes(x = title_ordered, y = Rating2, fill = title_ordered)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme(legend.position = "none") +
  labs(y = "Player's Blitz Rating", x = "FIDE Title")


## 5d) titles per Fed, order by most common title ----
player_df %>% filter(title_ordered %in% c("GM", "IM", "FM", "CM", "NM"),
                     fed %in% c("USA", "RUS", "POL", "ESP", "IND")) %>% 
  count(fed, title_ordered) %>% 
  group_by(fed) %>% 
  add_count(wt = n) %>% 
  mutate(percent = n/nn) %>% 
  arrange(desc(nn)) %>% 
  ggplot(aes(x = title_ordered, y = fed, fill = percent, label = round(percent, 2))) +
  geom_tile() +
  geom_text()

player_df %>% filter(title_ordered %in% c("GM", "IM", "FM", "CM", "NM"),
                     fed %in% c("USA", "RUS", "POL", "ESP", "IND")) %>% 
  count(fed, title_ordered) %>% 
  group_by(fed) %>% 
  add_count(wt = n) %>% 
  mutate(percent = n/nn) %>% 
  arrange(desc(nn)) %>% 
  ggplot(aes(x = title_ordered, y = fed, fill = percent, label = label_percent(accuracy = 0.1)(percent))) +
  geom_tile() +
  geom_text()

# in the US there seem to be a huge player pool

# top 15 feds by title split

# show line by line
Fed_title_df <- TT %>% filter(games_played >= 9) %>% 
  count(username, fed, title_ordered) %>% 
  filter(n >= 5) %>%  # show in between steps
  filter(title_ordered %in% c("GM", "IM", "FM", "CM", "NM")) %>% 
  count(fed, title_ordered) %>% 
  group_by(fed) %>% 
  add_count(wt = n) %>% 
  mutate(percent = n/nn) %>% 
  arrange(desc(nn)) %>% # pull(Fed) %>% unique() %>% head(15) -> top15_Fed
  filter(fed %in% top15_fed) %>% 
  mutate(fed_N = paste0(fed, "\n(",nn,")"))

#Fed_title_df$title_ordered <- factor(Fed_title_df$title, levels = c("GM", "IM", "FM", "CM", "NM"))

Fed_GM_order <- Fed_title_df %>% filter(title_ordered == "GM") %>% arrange(desc(percent)) %>% pull(fed_N)
Fed_title_df$fed_N <- factor(Fed_title_df$fed_N, levels = Fed_GM_order)

Fed_title_df %>% 
  ggplot(aes(x = fed_N, y = percent, fill = title_ordered)) +
  geom_col(position = "stack", color = "black") +
  #geom_text(data = Fed_title_df %>% filter(title_ordered == "GM"),
  #          aes(x = Fed_N, y = 1-percent, label = percent), color = "white")
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  geom_text(aes(label = paste0(round(percent*100, 0),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  labs(x = "", y = "", fill = "title:", subtitle = "\nProportion of titles per Federation (Country)\n") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


## 5e) How many points get scored on average (barchart) by title ----
# stacked barchart of score (11-10, 9.5-8.5, 8-7, 6.5-5, rest) (including half points))
# average score as point (looks good enough as boxplot)
TT <- TT %>% mutate(score_cat = factor(case_when(
  score >= 9 ~ "9-11",
  score >=7 & score <9 ~ "7-8.5",
  score >=5 & score <7 ~ "5-6.5",
  score >=3 & score <5 ~ "3-4.5",
  score >=1 & score <3 ~ "1-2.5",
  score >=0 & score <1 ~ "0-0.5"),
  levels = c("9-11", "7-8.5", "5-6.5", "3-4.5", "1-2.5", "0-0.5")))

TT %>% filter(title %in% c("GM", "IM", "FM", "CM", "NM")) %>% 
  count(title_ordered, score_cat) %>% 
  ggplot(aes(x = title_ordered, y = n, fill = score_cat)) +
  geom_col()

# excluding players that did not play a single game
TT %>% filter(games_played > 0,
              title %in% c("GM", "IM", "FM", "CM", "NM")) %>% 
  count(title_ordered, score_cat) %>% 
  group_by(title_ordered) %>% 
  add_count(wt = n) %>% 
  mutate(percent = n/nn) %>% 
  ggplot(aes(x = title_ordered, y = percent, fill = score_cat)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(percent*100, 1),"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  theme_light() +
  labs(fill = "Score\nCategory:",
       subtitle = "Proportion of final scores of the Titled Tuesday tournament by FIDE title",
       y = "Score Percentage", x = "Title") +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())


# given that you need at least 9 points to win it is almost impossible for IM and FM to win
TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  ungroup() %>% count(title, sort = T)

TT %>% group_by(file_name) %>% 
  filter(score == max(score)) %>% 
  ungroup() %>% filter(title != "GM") %>% 
  select(date, time, number, fed, title, username, name, score)

## 6) blitz ratings ----
TT %>% group_by(time, date) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(x = date, y = avg_rating, color = time)) +
  geom_line() +
  facet_wrap(~time, nrow = 2)
  
TT %>% group_by(time, date) %>% 
  summarize(median_rating = median(rating)) %>% 
  ggplot(aes(x = date, y = median_rating, color = time)) +
  geom_line() 

summary(TT$rating)

summary(TT$rating[TT$time == "early"]) # slightly higher
summary(TT$rating[TT$time == "late"])

quantile(TT$rating) # 4 equal parts (based on n ratings)
# 0 and 100% mark the min and max

TT %>% ggplot(aes(x = date, y = rating)) +
  geom_quantile()

TT %>% ggplot(aes(x = date, y = rating)) +
  geom_quantile(quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile(TT$rating, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

TT %>% ggplot(aes(x = date, y = rating)) +
  geom_quantile(quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))

quantile(TT$rating, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
