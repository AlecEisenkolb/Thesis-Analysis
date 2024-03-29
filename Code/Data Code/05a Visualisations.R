# Data visualizations
# Date: 29.08.2022
# Author: Alec Eisenkolb

# For this code to work, user must have previously run the following data cleaning function in R:
# "01 Data Cleaning.R", "02 District Predictions.R", "03 Twitter Cleaning.R" &
# "04 Twitter Replies Cleaning.R", as well as the python code to scrape the Twitter data "Twitter API.py". 

# install.packages("maptools") # required for spatial visual analysis

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(tidyverse,
               ggplot2,
               ggridges,
               devtools,
               ggthemr,
               forcats,
               zoo,
               scales)

# define colour palette
green_palette <- c("#4b6043", "#658354", "#75975e", "#87ab69", "#95bb72", "#a3c585",
                   "#b3cf99", "#c7ddb5", "#ddead1")

# define colour to outline boxes
green_palette <- c("#555555", green_palette)

# remove previous theme and set new theme
ggthemr_reset()
graph_theme <- define_palette(
  swatch = green_palette, # colour for plotting points
  gradient = c(lower = green_palette[1L], upper = green_palette[2L]),
  background = '#ffffff' # define white as background
)

# define second colour scheme (used for line graphs for better contrast between groups)
colourful_palette <- c("#658354", "#b3cf99", "#47697E", "#688B9A", "#FFCC33", "#FEEB75")
colourful_palette <- c("#555555", colourful_palette)
theme2 <- define_palette(
  swatch = colourful_palette,
  gradient = c(lower = colourful_palette[1L], upper = colourful_palette[2L]),
  background = '#ffffff'
)

show_col(colourful_palette)
show_col(green_palette)

# define third colour scheme (used to match colour of official party colour)
full_party_col <- c("#0000FF", "#000000", "#373737", "#FF00FF", "#F5FF00", "#35682d", "#ff0000")
full_party_col <- c("#555555", full_party_col)
party_full <- define_palette(
  swatch = full_party_col,
  gradient = c(lower = full_party_col[1L], upper = full_party_col[2L]),
  background = '#ffffff'
)

# another colour scheme for party colours, excluding the FDP
party_wo_FDP <- c("#0000FF", "#000000", "#373737", "#FF00FF", "#35682d", "#ff0000")
party_wo_FDP <- c("#555555", party_wo_FDP)
party_noFDP <- define_palette(
  swatch = party_wo_FDP,
  gradient = c(lower = party_wo_FDP[1L], upper = party_wo_FDP[2L]),
  background = '#ffffff'
)

# another colour scheme for party colours, excluding the party CSU
party_wo_CSU <- c("#0000FF", "#000000", "#FF00FF", "#F5FF00", "#35682d", "#ff0000")
party_wo_CSU <- c("#555555", party_wo_CSU)
party_noCSU <- define_palette(
  swatch = party_wo_CSU,
  gradient = c(lower = party_wo_CSU[1L], upper = party_wo_CSU[2L]),
  background = '#ffffff'
)

# set new theme as default
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)

# define ggplot2 theme for maps (excluding axes and titles)
map_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# import master, twitter dataframes and shapefiles
df_master <- read_csv("Clean Data/master_all_cand.csv")
df_twitter <- read_csv("Clean Data/Twitter/twitter_clean.csv", col_types = "ccTccddddcccD")
df_userids <- read_csv("Clean Data/Twitter/twitter_ids.csv", col_types = "cccc")

df_shp <- read_csv("Clean Data/District_shapefiles.csv") %>%
  mutate(WKR_NR = str_pad(WKR_NR, 3, side = "left", pad = "0")) %>%
  rename(district_num = WKR_NR)

df_master %>%
  filter(party=="GRÜNE" & district_num > 295) %>%
  view()

# # # # # Graph 0: party distribution # # # # #
df_master %>%
  group_by(party) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(party, -n), y=n)) +
  geom_bar(stat="identity") +
  xlab("Party") +
  ylab("Frequency")

# # # # # Graph 1: Gender distribution # # # # #

# total data (all candidates)
df_master %>%
  mutate(gender = if_else(gender==0, "Female", "Male")) %>%
  ggplot(aes(x=gender)) + 
  geom_bar() +
  xlab("Gender of Candidates") + 
  ylab("Frequency") +
  labs(caption = "Source: Own dataset") +
  theme(text = element_text(family = "Helvetica"))

# by party
df_master %>%
  mutate(gender = if_else(gender==0, "Female", "Male")) %>%
  ggplot(aes(x=party, fill=gender)) +
  geom_bar(position = "dodge") +
  xlab("Gender Distribution by Party") + 
  ylab("Frequency") +
  labs(fill="Legend")

# by party with relative frequencies
df_master %>%
  mutate(gender = if_else(gender==0, "Female", "Male")) %>%
  group_by(party, gender) %>%
  summarise(quant = n()) %>%
  mutate(freq = quant/sum(quant)) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(party = fct_inorder(as.factor(party))) %>%
  ggplot(aes(fill=gender, x=party, y=freq)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(fill="Legend") +
  xlab("Party") +
  ylab("Share") +
  scale_y_continuous(labels = scales::percent)

ggsave("Graphs/Master/Party_Gender_Share.png", dpi=300)

# # # # # Graph 2: Age distribution # # # # # 

# total
df_master %>%
  ggplot(aes(x=birth_year)) +
  geom_bar() + 
  xlab("Candidates' Year of Birth") + 
  ylab("Frequency")

# by party
df_master %>%
  ggplot(aes(x=birth_year, y = fct_rev(as.factor(party)), fill=party)) + 
  geom_density_ridges(scale=1.7, show.legend = FALSE) +
  labs(fill="Legend") +
  xlab("Year of Birth") + 
  ylab("Party")

ggsave("Graphs/Master/Party_Agedistribution.png", dpi=300)

# # # # # Graph 3: Vote outcome of candidates # # # # #

# all candidates & 1st Vote
df_master %>%
  ggplot(aes(x=percent_1)) +
  geom_histogram(color = "#000000", binwidth = 2) +
  xlab("Vote Share") +
  ylab("Frequency") +
  scale_x_continuous(labels = function(x) {paste(x, "%")}) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Percent_1_distribution.png", dpi=300, width=9.5, height=7)

# all candidates & 2nd Vote
df_master %>%
  ggplot(aes(x=percent_2)) +
  geom_histogram(color = "#000000", binwidth = 2) +
  xlab("Vote Share") +
  ylab("Frequency") +
  scale_x_continuous(breaks = c(0, 20, 40), labels = function(x) {paste(x, "%")}) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Percent_2_distribution.png", dpi=300, width=9.5, height=7)

# all candidates & diff 1st to 2nd Vote (1st Vote - 2nd Vote per candidate)
df_master %>%
  filter(pct_diff_1to2 < 100 & pct_diff_1to2 > -100) %>%
  ggplot(aes(x=pct_diff_1to2)) +
  geom_histogram(color = "#000000", binwidth = 3) +
  xlab("Percentage Difference") +
  ylab("Frequency") +
  scale_x_continuous(labels = function(x) {paste(x, "%")},
                     breaks = pretty_breaks(n=6)) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Pct_Diff.png", dpi=300, width=9.5, height=7)

summary(df_master$pct_diff_1to2)
sd(df_master$pct_diff_1to2, na.rm = TRUE)

# # # # # Graph 4: Geo-data of candidates # # # # #

# number of candidates per district with a Twitter Acc
df_shp %>%
  left_join((df_master %>%
               filter(Twitter_Acc == 1) %>%
               group_by(district_num) %>%
               summarise(count = n())), by = "district_num") %>%
  mutate(count = if_else(is.na(count), 0, as.numeric(count))) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = count, group = group), colour="#000000") +
  coord_fixed(1.4) +
  map_axes +
  scale_fill_gradientn(colors=rev(c("#658354", "#b3cf99", "#47697E", "#688B9A", "#FFCC33", "#FEEB75", "#ffffff")),
                      breaks=c(NA, 1, 2, 3, 4, 5, 6)) +
  labs(fill="Candidates")

ggsave("Graphs/Master/Geo_number_candidates.png", dpi=300)

# # # # # Graph 5: Geo-data of votes # # # # #
# set map theme
ggthemr_reset()
ggthemr(party_noFDP, layout = "clean", spacing = 1)

# check if direct mandate winners (1st vote) by number of party seats won is correct, 
# compare with official results from: https://www.tagesschau.de/inland/btw21/bundestagswahl-wahlkarte-101.html
df_master %>%
  filter(winner==1) %>%
  select(district_num, party) %>%
  group_by(party) %>%
  summarise(count = n()) %>%
  view()

# Graph of direct vote (1st vote)
df_shp %>%
  left_join((df_master %>%
               filter(winner==1) %>%
               select(district_num, party)),
            by = "district_num") %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = party, group = group), colour = "#000000") +
  coord_fixed(1.4) +
  map_axes +
  labs(fill="Party") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Geo_Percent_1.png", dpi=300)

# # # # # Graph: Geo-data of control variables per district # # # # #
# reset theme
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)

# map of household income per district
df_shp %>%
  left_join((df_master %>%
               group_by(district_num) %>%
               summarise(value = mean(district_avg_income))),
            by = "district_num") %>%
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(fill = value, group = group), colour = "#000000") +
  coord_fixed(1.4) +
  map_axes +
  scale_fill_gradient(low = "yellow", high = "#013220", na.value = "#ffffff") +
  labs(fill="Average Personal Income (€)")

# map of GDP per district
df_shp %>%
  left_join((df_master %>%
               group_by(district_num) %>%
               summarise(value = mean(district_gdp_2018)) %>%
               mutate(value = log(value))),
            by = "district_num") %>%
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(fill = value, group = group), colour = "#000000") +
  coord_fixed(1.4) +
  map_axes +
  scale_fill_gradient(low = "yellow", high = "#013220", na.value = "#ffffff") +
  labs(fill="2018 Log GDP (€)")

# map of employment rates
df_shp %>%
  left_join((df_master %>%
               group_by(district_num) %>%
               summarise(value = mean(district_unemprate_total)) %>%
               mutate(value = 100 - value)),
            by = "district_num") %>%
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(fill = value, group = group), colour = "#000000") +
  coord_fixed(1.4) +
  map_axes +
  scale_fill_gradient(low = "yellow", high = "#013220", na.value = "#ffffff") +
  labs(fill="Employment Rate (%)")

# map of education rates
df_shp %>%
  left_join((df_master %>%
               group_by(district_num) %>%
               summarise(value = mean(district_educ_general_total_grads_per1000))),
            by = "district_num") %>%
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(fill = value, group = group), colour = "#000000") +
  coord_fixed(1.4) +
  map_axes +
  scale_fill_gradient(low = "yellow", high = "#013220", na.value = "#ffffff") +
  labs(fill="Graduates per 1000 Inhabitants")

# # # # # Graph 6: Twitter Usage (posts) # # # # #
ggthemr_reset()
ggthemr(graph_theme, layout = "clean", spacing = 1)
# total
df_master %>%
  filter(Twitter_Acc==1) %>%
  ggplot(aes(x=Avg_Weekly_Posts)) +
  geom_histogram(color = "#000000", binwidth = 5) +
  xlab("Average Weekly Posts") +
  ylab("Frequency")
  
# by party
df_master %>%
  filter(Avg_Weekly_Posts <= 50 & Twitter_Acc==1) %>%
  ggplot(aes(x=Avg_Weekly_Posts, y = fct_rev(as.factor(party)), fill=party)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Average Weekly Posts") +
  ylab("Party")

# by time (4 time groups of July, August, September, and post-election)
df_master %>%
  select(firstname, lastname, Avg_posts_time_group1, Avg_posts_time_group2, 
         Avg_posts_time_group3, Avg_posts_time_group4) %>%
  gather("Time", "value", 3:6) %>%
  drop_na() %>%
  filter(value <= 100) %>%
  ggplot(aes(x=Time, y=value)) +
  geom_boxplot(fill = "#b3cf99") +
  scale_x_discrete(labels = c("July", "August", "September - Election", "Post-Election")) +
  xlab("Average Weekly Posts") +
  ylab("Frequency")

# # # # # Graph 7: Distribution of likes, replies, retweets and language # # # # #
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)
# total (candidates w Twitter) likes per post
df_master %>%
  filter(Mean_Likes < 200 & Twitter_Acc==1) %>%
  ggplot(aes(y=Mean_Likes, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Likes per Post")

ggsave("Graphs/Master/Likes_post.png", dpi=300)

# total (candidates w Twitter) replies per post
df_master %>%
  filter(Mean_Reply < 100 & Twitter_Acc==1) %>%
  ggplot(aes(y=Mean_Reply, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Replies per Post")

ggsave("Graphs/Master/Replies_post.png", dpi=300)

# total (candidates w Twitter) re-tweets per post
df_master %>%
  filter(Mean_RT < 1000 & Twitter_Acc==1) %>%
  ggplot(aes(y=Mean_RT, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Re-Tweets per Post")

ggsave("Graphs/Master/Retweets_post.png", dpi=300)

# Language by party (define new theme for better contrast)
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)
# Graph
df_master %>%
  filter(Twitter_Acc==1) %>%
  select(party, starts_with("lang_")) %>%
  group_by(party) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  rename_with( ~ substr(.x, 6, nchar(.x)), .cols = starts_with("lang_")) %>%
  pivot_longer(!party, names_to = "language", values_to = "Value") %>%
  mutate(language = if_else(language=="und", "undefined", language)) %>%
  ggplot(aes(x=party, y=Value, fill=language)) +
  geom_bar(position="fill", stat="identity") +
  xlab("Party") +
  ylab("Share of Language in Posts") +
  labs(fill="Legend") +
  scale_y_continuous(labels = scales::percent)

# # # # # Graph 8: Timeline of posting frequency # # # # #
ggthemr_reset()
ggthemr(graph_theme, layout = "clean", spacing = 1)

# total
df_twitter %>%
  group_by(Day) %>%
  summarise(count = n()) %>%
  mutate(Mov_Avg = zoo::rollmean(count, k = 7, fill = NA)) %>%
  view()


  ggplot(aes(x=Day)) +
  geom_line(aes(y=count, colour = "Total postings")) +
  geom_line(aes(y=Mov_Avg, color = "7-Day moving average")) +
  xlab("Date") +
  ylab("Frequency of Posts") +
  scale_colour_manual("",
                      breaks = c("Total postings", "7-Day moving average"),
                      values = c("#4b6043", "#FFCC33")) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dotted", color = "#000000", size = 1) +
  annotate(geom = "text",
           label = c("Election Date"),
           x = as.Date("2021-09-26"),
           y = c(3750),
           angle = 90, 
           vjust = 1.5,
           hjust = 0.5,
           family = "serif") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Posting_timeline_total.png", dpi=300, width=9.5, height=7)

# by party (define new colour scheme for this chart)
ggthemr_reset()
ggthemr(party_noCSU, layout = "clean", spacing = 1)
# obtain user ids and names of candidates
df_userids <- df_userids %>%
  rename(`User ID` = user_id)
# link names to party of each candidate
df_master_reduced <- df_master %>%
  filter(Twitter_Acc==1) %>%
  select(firstname, lastname, party)
# plot posting frequency by party
df_twitter %>%
  left_join(df_userids, by = "User ID") %>%
  left_join(df_master_reduced, by = c("firstname", "lastname")) %>%
  mutate(party = if_else((party=="CSU" | party=="CDU"), "CDU/CSU", party)) %>%
  group_by(party, Day) %>%
  summarise(count = n()) %>%
  mutate(Mov_Avg = zoo::rollmean(count, k = 7, fill = NA)) %>%
  drop_na() %>% 
  ggplot(aes(x=Day, y=Mov_Avg, colour = party)) +
  geom_line() +
  xlab("Date") +
  ylab("Frequency of Posts") +
  theme(legend.position = "bottom") +
  labs(colour = "") +
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dotted", color = "#000000", size = 1) +
  theme(text = element_text(family = "serif", size = 18)) +
  annotate(geom = "text",
           label = c("Election Date"),
           x = as.Date("2021-09-26"),
           y = c(600),
           angle = 90, 
           vjust = 1.5,
           hjust = 0.5,
           family = "serif")

ggsave("Graphs/Master/Posting_timeline_party.png", dpi=300, width=9.5, height=7)

# # # # # Graph: Scatterlot of vote outcome vs twitter usage # # # # #
# reset theme
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)

# by weekly posts & direct vote share (1st vote)
df_master %>%
  filter(Avg_Weekly_Posts <= 100) %>%
  ggplot(aes(x=Avg_Weekly_Posts, y=percent_1)) +
  geom_point() +
  xlab("Average Weekly Posts") +
  ylab("Direct Vote Share") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# by likes & direct vote share (1st vote)
df_master %>%
  filter(Mean_Likes <= 100) %>%
  ggplot(aes(x=Mean_Likes, y=percent_1)) +
  geom_point() +
  xlab("Average Likes") +
  ylab("Direct Vote Share") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# by weekly posts & difference of 1st to 2nd vote per candidate
df_master %>%
  filter(Avg_Weekly_Posts <= 100) %>%
  ggplot(aes(x=Avg_Weekly_Posts, y=pct_diff_1to2)) +
  geom_point() +
  xlab("Average Weekly Posts") +
  ylab("Difference of 1st to 2nd Vote") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# by likes & difference of 1st to 2nd vote per candidate
df_master %>%
  filter(Mean_Likes <= 100) %>%
  ggplot(aes(x=Mean_Likes, y=pct_diff_1to2)) +
  geom_point() +
  xlab("Average Likes") +
  ylab("Difference of 1st to 2nd Vote") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# # # # # Section on testing balance between samples # # # # #

# This will be once for binary variable (Active on Twitter) and 
# continuous treatment (level of activity on twitter)

# # # # # Testing balance between control (no Twitter) vs. treatment (Twitter) group # # # # #

# check whether candidates present in all major parties
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  group_by(party, Twitter_Acc) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=party, y=count, fill=Twitter_Acc)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Party") +
  ylab("Frequency") +
  labs(fill="Legend") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Twitter_treatment_party.png", dpi=300, width=9.5, height=7)

# Twitter profiles by party - relative frequencies
df_master %>%
  group_by(party, Twitter_Acc) %>%
  summarise(quant = n()) %>%
  mutate(freq = quant/sum(quant), 
         Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", 
                               if_else(Twitter_Acc==1, "Twitter", "NA"))) %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(party = fct_inorder(as.factor(party))) %>%
  ggplot(aes(fill=Twitter_Acc, x=party, y=freq)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(fill="Legend") +
  xlab("Party") +
  ylab("Share") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Twitter_treatment_party_relative.png", dpi=300, width=9.5, height=7)

# Compute same graph as above, but with three categories: no twitter, Twitter (non-active) and Twitter (active)
df_master %>%
  mutate(Twitter_Acc = if_else((Twitter_Acc==1 & Avg_Weekly_Posts==0), "Twitter (non-active)",
                               if_else((Twitter_Acc==1 & Avg_Weekly_Posts!=0), "Twitter (active)", "No Twitter"))) %>%
  #ordered(Twitter_Acc, levels = c("No Twitter", "Twitter (non-active)", "Twitter (active)")) %>%
  group_by(party, Twitter_Acc) %>%
  summarise(quant = n()) %>%
  mutate(freq = quant/sum(quant)) %>%
  ungroup() %>%
  mutate(Twitter_check = if_else(Twitter_Acc=="No Twitter", -as.numeric(freq), as.numeric(NA))) %>%
  mutate(Twitter_check =if_else(Twitter_Acc=="Twitter (non-active)", as.numeric(-60), as.numeric(Twitter_check))) %>%
  arrange(desc(Twitter_check)) %>%
  ggplot(aes(fill=fct_inorder(as.factor(Twitter_Acc)), x=fct_inorder(as.factor(party)), y=freq)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(fill="Legend") +
  xlab("Party") +
  ylab("Share") +
  scale_fill_manual(values=c("#658354", "#87ab69", "#b3cf99")) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Twitter_treatment_party_relative_three_categories.png", dpi=300, width=9.5, height=7)

# check distribution of 1st votes - histogram
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(x=percent_1, fill=Twitter_Acc)) +
  geom_histogram(color = "#000000", binwidth = 1) +
  xlab("Vote Share for Direct Candidates") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_x_continuous(labels = function(x) {paste(x, "%")}) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Vote1_treatment.png", dpi=300, width=9.5, height=7)

# check distribution of 1st votes - boxplot
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(y=percent_1, x=Twitter_Acc)) +
  geom_boxplot(color = "#000000") +
  scale_fill_manual(values=c("#4b6043", "#a3c585")) +
  xlab("Twitter Account") +
  ylab("Percentage Vote for Direct Candidates") +
  theme(text = element_text(family = "serif", size = 18))

# check distribution of 1st - 2nd votes - histogram
df_master %>%
  filter(pct_diff_1to2 < 100 & pct_diff_1to2 > -100) %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(x=pct_diff_1to2, fill=Twitter_Acc)) +
  geom_histogram(color="#000000", binwidth = 2) +
  #scale_fill_manual(values=c("#4b6043", "#a3c585")) +
  xlab("%-Point Difference of 1st to 2nd Vote") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_x_continuous(labels = function(x) {paste(x, "%")},
                     breaks = pretty_breaks(n=6)) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/1_to_2_pct_by_TW.png", dpi=300, width=9.5, height=7)

# check distribution of 1st - 2nd votes - boxplot
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(y=pct_diff_1to2, x=Twitter_Acc)) +
  geom_boxplot(color = "#000000") +
  scale_fill_manual(values=c("#4b6043", "#a3c585")) +
  xlab("Twitter Account") +
  ylab("%-Point Difference of 1st to 2nd Vote")

# check distribution of being incumbent
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  mutate(incumbent = if_else(incumbent==1, "Yes", "No")) %>%
  group_by(incumbent, Twitter_Acc) %>%
  summarise(value = n()) %>%
  ggplot(aes(x=incumbent, y=value, fill=Twitter_Acc)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Incumbent") + 
  ylab("Frequency") +
  labs(fill="Legend") +
  #scale_fill_manual(values=c("#4b6043", "#a3c585")) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Incumbent_treatment.png", dpi=300)

# check distribution of location (states)
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  group_by(state_short, Twitter_Acc) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=state_short, y=count, fill=Twitter_Acc)) +
    geom_bar(position="dodge", stat = "identity") +
  xlab("State") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_fill_manual(values=c("#4b6043", "#a3c585"))

# check distribution of gender
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  mutate(gender = if_else(gender==0, "Female", "Male")) %>%
  group_by(gender, Twitter_Acc) %>%
  summarise(value = n()) %>%
  ggplot(aes(x=Twitter_Acc, y=value, fill=gender)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Twitter Account") +
  ylab("Frequency") +
  labs(fill="Legend") +
  #scale_fill_manual(values=c("#4b6043", "#a3c585")) +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Gender_treatment.png", dpi=300)

# Graph of vote outcome with/wo a Twitter account by party
df_master %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  group_by(Twitter_Acc, party) %>%
  summarise(Mean_Outcome = mean(percent_1, na.rm = TRUE)) %>%
  arrange(desc(Mean_Outcome)) %>%
  mutate(party = fct_inorder(as.factor(party))) %>%
  ggplot(aes(x=party, y=Mean_Outcome, fill=Twitter_Acc)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Party") +
  ylab("Average Vote Outcome") +
  labs(fill="Legend") +
  theme(text = element_text(family = "serif", size = 18)) +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

ggsave("Graphs/Master/Vote_outcome_by_Twitter_party.png", dpi=300, width=9.5, height=7)

# check distribution of birth year - histogram
df_master %>% 
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(x=birth_year, fill=Twitter_Acc)) +
  geom_histogram(colour="#000000", binwidth=2) +
  #scale_fill_manual(values = c("#4b6043", "#a3c585")) +
  xlab("Year of Birth") +
  ylab("Frequency") +
  labs(fill="Legend") +
  theme(text = element_text(family = "serif", size = 18))
  
ggsave("Graphs/Master/Birth_year_treament.png", dpi=300, width=9.5, height=7)

# check distribution of birth year - boxplot
df_master %>% 
  mutate(Twitter_Acc = if_else(Twitter_Acc==0, "No Twitter", "Twitter")) %>%
  ggplot(aes(y=birth_year, x=Twitter_Acc)) +
  geom_boxplot(colour="#000000") +
  xlab("Twitter Account") +
  ylab("Year of Birth")

# # # # # Testing balance between arms of treatment (levels of activity on Twitter) # # # # #

# define the 33% and 66% quantiles of average weekly posting activity
summary(df_master$Avg_Weekly_Posts)
perc_33 <- as.numeric(quantile(df_master$Avg_Weekly_Posts, probs = 0.33, na.rm = TRUE))
perc_66 <- as.numeric(quantile(df_master$Avg_Weekly_Posts, probs = 0.66, na.rm = TRUE))

# define arms of treatment: low / medium / high posting activity
df_master <- df_master %>%
  mutate(Twitter_act = if_else((Avg_Weekly_Posts <= perc_33), as.character("Low"),
                               if_else((Avg_Weekly_Posts > perc_33 & Avg_Weekly_Posts <= perc_66), as.character("Medium"), 
                                       if_else((Avg_Weekly_Posts > perc_66), as.character("High"), as.character(NA)))))

# order variable
df_master$Twitter_act <- ordered(df_master$Twitter_act, levels = c("High", "Medium", "Low"))

# check whether candidates present in all major parties
df_master %>%
  group_by(party, Twitter_act) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  ggplot(aes(x=party, y=count, fill=Twitter_act)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Party") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_fill_manual(values = c("#4b6043", "#87ab69", "#b3cf99"))

# check distribution of 1st votes - histogram
df_master %>%
  select(percent_1, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(x=percent_1, fill=Twitter_act)) +
  geom_histogram(color = "#000000", binwidth = 1) +
  scale_fill_manual(values=c("#4b6043", "#87ab69", "#b3cf99")) +
  xlab("Percentage Vote for Direct Candidates") +
  ylab("Frequency") +
  labs(fill="Legend")

# check distribution of 1st votes - boxplot
df_master %>%
  select(percent_1, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(y=percent_1, x=Twitter_act)) +
  geom_boxplot(color = "#000000") +
  xlab("Twitter Activity") +
  ylab("Percentage Vote for Direct Candidates")

# check distribution of 1st - 2nd votes - histogram

df_master %>%
  select(pct_diff_1to2, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(x=pct_diff_1to2, fill=Twitter_act)) +
  geom_histogram(color = "#000000", binwidth = 1) +
  scale_fill_manual(values=c("#4b6043", "#87ab69", "#b3cf99")) +
  xlab("%-Point Difference of 1st to 2nd Vote") +
  ylab("Frequency") +
  labs(fill="Legend") +
  xlim(-50, 50)

# check distribution of 1st - 2nd votes - boxplot
df_master %>%
  select(pct_diff_1to2, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(y=pct_diff_1to2, x=Twitter_act)) +
  geom_boxplot(color = "#000000") +
  xlab("Twitter Activity") +
  ylab("%-Point Difference of 1st to 2nd Vote")

# check distribution of being incumbent
df_master %>%
  mutate(incumbent = if_else(incumbent==1, "Yes", "No")) %>%
  group_by(incumbent, Twitter_act) %>%
  summarise(value = n()) %>%
  drop_na() %>%
  ggplot(aes(x=incumbent, y=value, fill=Twitter_act)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Incumbent") + 
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_fill_manual(values=c("#4b6043", "#87ab69", "#b3cf99"))

# check distribution of location (states)
df_master %>%
  group_by(state_short, Twitter_act) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  ggplot(aes(x=state_short, y=count, fill=Twitter_act)) +
  geom_bar(position="dodge", stat = "identity") +
  xlab("State") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_fill_manual(values=c("#4b6043", "#87ab69", "#b3cf99"))

# check distribution of gender
df_master %>%
  mutate(gender = if_else(gender==0, "Female", "Male")) %>%
  group_by(gender, Twitter_act) %>%
  summarise(value = n()) %>%
  drop_na() %>%
  ggplot(aes(x=Twitter_act, y=value, fill=gender)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Twitter Account") +
  ylab("Frequency") +
  labs(fill="Legend") +
  scale_fill_manual(values=c("#4b6043", "#a3c585"))

# check distribution of birth year - histogram
df_master %>% 
  select(birth_year, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(x=birth_year, fill=Twitter_act)) +
  geom_histogram(colour="#000000", binwidth=2) +
  scale_fill_manual(values = c("#4b6043", "#87ab69", "#b3cf99")) +
  xlab("Year of Birth") +
  ylab("Frequency") +
  labs(fill="Legend")

# check distribution of birth year - boxplot
df_master %>% 
  select(birth_year, Twitter_act) %>%
  drop_na() %>%
  ggplot(aes(y=birth_year, x=Twitter_act)) +
  geom_boxplot(colour="#000000") +
  xlab("Twitter Account") +
  ylab("Year of Birth")

##### Checking for common support between control and treatment group!

# Compute propensity scores using logit model
logit_model <- glm(formula = Twitter_Acc ~ birth_year + gender + job_key + incumbent 
                   + isListed + Prediction + party + Top_candidate 
                   + I(log(district_avg_income)) + district_pop_foreign 
                   + district_age_60over + East_Germany, 
                   family = binomial(),
                   data = df_master)

# compute propensity scores
prop_df <- data.frame(Prop_Score = predict(logit_model, type = "response"),
                      Twitter_Acc = logit_model$model$Twitter_Acc)

# visualize propensity scores
prop_df %>%
  mutate(Twitter_Acc = if_else(Twitter_Acc == 1, "Twitter Account", "No Twitter Account")) %>%
  ggplot(aes(x=Prop_Score, fill=Twitter_Acc)) +
  geom_density(alpha=0.6) +
  xlab("Propensity Score") +
  ylab("Density") +
  labs(fill="Legend") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/Common_support.png", dpi=300, width=9.5, height=7)


