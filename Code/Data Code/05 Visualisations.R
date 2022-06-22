# Data visualizations
# Date: 22.06.2022
# Author: Alec Eisenkolb

# For this code to work, user must have previously ran the Twitter API functions
# "Twitter API Code.py", as well as the previous data cleaning function in R:
# "01 Data Cleaning.R", "02 District Predictions.R", "03 Twitter Cleaning.R" &
# "04 Twitter Replies Cleaning.R"

# install libraries, uncomment below if necessary
# install.packages("devtools")
# devtools::install_github("cttobin/ggthemr")

# load libraries
library(tidyverse)
library(ggplot2)
library(ggridges)
library(devtools)
library(ggthemr)
library(forcats)
library(zoo)

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
#colourful_palette <- c("#5B7444", "#A3C586", "#47697E", "#688B9A", "#FFCC33", "#FEEB75")
colourful_palette <- c("#658354", "#b3cf99", "#47697E", "#688B9A", "#FFCC33", "#FEEB75")
colourful_palette <- c("#555555", colourful_palette)
theme2 <- define_palette(
  swatch = colourful_palette,
  gradient = c(lower = colourful_palette[1L], upper = colourful_palette[2L]),
  background = '#ffffff'
)

# set new theme as default
ggthemr(graph_theme, layout = "clean", spacing = 1)

# import master and twitter dataframe
df_master <- read_csv("Clean Data/master.csv")
df_twitter <- read_csv("Clean Data/Twitter/twitter_clean.csv", col_types = "ccTccddddcccD")
df_userids <- read_csv("Clean Data/Twitter/twitter_ids.csv", col_types = "cccc")

# # # # # Graph 1: Gender distribution of candidates w Twitter Acc. # # # # #

# total data
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
  ylab("Frequency")

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
  ylab("Frequency") +
  scale_y_continuous(labels = scales::percent)

# # # # # Graph 2: Age distribution of candidates w Twitter Acc. # # # # # 

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

# # # # # Graph 3: Vote outcome of candidates w Twitter Acc. # # # # #

# total & 1st Vote
df_master %>%
  ggplot(aes(x=percent_1)) +
  geom_histogram(color = "#000000") +
  xlab("Percentage Vote for Direct Candidates") +
  ylab("Frequency")

# total & 2nd Vote
df_master %>%
  ggplot(aes(x=percent_2)) +
  geom_histogram(color = "#000000") +
  xlab("Percentage Vote for Party per District") +
  ylab("Frequency")

# total & diff 1st to 2nd Vote (1st Vote - 2nd Vote per candidate)
df_master %>%
  ggplot(aes(x=pctpoint_diff_1to2)) +
  geom_histogram(color = "#000000") +
  xlab("Percentage Point Difference between 1st and 2nd Vote") +
  ylab("Frequency")

# # # # # Graph 4: Geo-data of candidates # # # # #

# (22.06.2022)

# # # # # Graph 5: Geo-data of votes # # # # #

# (22.06.2022)

# # # # # Graph: Geo-data of control variables per district # # # # #

# (22.06.2022)

# # # # # Graph 6: Twitter Usage (posts) # # # # #

# total
df_master %>%
  ggplot(aes(x=Avg_Weekly_Posts)) +
  geom_histogram(color = "#000000", binwidth = 5) +
  xlab("Average Weekly Posts") +
  ylab("Frequency")
  
# by party
df_master %>%
  filter(Avg_Weekly_Posts <= 100) %>%
  ggplot(aes(x=Avg_Weekly_Posts, y = fct_rev(as.factor(party)), fill=party)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Average Weekly Posts") +
  ylab("Party")
  
# # # # # Graph 7: Distribution of likes, replies, retweets and language # # # # #

# total (candidates w Twitter) likes per post
df_master %>%
  filter(Mean_Likes < 500) %>%
  ggplot(aes(y=Mean_Likes, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Likes per Post")

# total (candidates w Twitter) replies per post
df_master %>%
  filter(Mean_Reply < 100) %>%
  ggplot(aes(y=Mean_Reply, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Replies per Post")

# total (candidates w Twitter) re-tweets per post
df_master %>%
  filter(Mean_RT < 1000) %>%
  ggplot(aes(y=Mean_RT, x=party)) +
  geom_boxplot(fill = "#b3cf99") +
  xlab("Party") +
  ylab("Re-Tweets per Post")

# Language by party (define new theme for better contrast)
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)
# Graph
df_master %>%
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
  ggplot(aes(x=Day)) +
  geom_line(aes(y=count, colour = "Total postings")) +
  geom_line(aes(y=Mov_Avg, color = "7-Day moving average")) +
  xlab("Date") +
  ylab("Frequency of Posts") +
  scale_colour_manual("",
                      breaks = c("Total postings", "7-Day moving average"),
                      values = c("#4b6043", "#FFCC33")) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dotted", color = "#000000", size = 1)

# by party (define new colour scheme for this chart)
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)
# obtain user ids and names of candidates
df_userids <- df_userids %>%
  rename(`User ID` = user_id)
# link names to party of each candidate
df_master_reduced <- df_master %>%
  select(firstname, lastname, party)
# plot posting frequency by party
df_twitter %>%
  left_join(df_userids, by = "User ID") %>%
  left_join(df_master_reduced, by = c("firstname", "lastname")) %>%
  mutate(party = if_else(party=="CSU", "CDU", party)) %>%
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
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dotted", color = "#000000", size = 1)

# # # # # Graph: Scatterlot of vote outcome vs twitter usage # # # # #

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
  ggplot(aes(x=Avg_Weekly_Posts, y=pctpoint_diff_1to2)) +
  geom_point() +
  xlab("Average Weekly Posts") +
  ylab("Difference of 1st to 2nd Vote") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# by likes & difference of 1st to 2nd vote per candidate
df_master %>%
  filter(Mean_Likes <= 100) %>%
  ggplot(aes(x=Mean_Likes, y=pctpoint_diff_1to2)) +
  geom_point() +
  xlab("Average Likes") +
  ylab("Difference of 1st to 2nd Vote") +
  scale_y_continuous(labels = function(x) {paste(x, "%")})

# # # # # Section on testing balance between samples # # # # #

##### Testing balance between control and treatment group!

### This will be once for binary variable (Active on Twitter) and continuous treatment (level of activity on twitter)
# change master dataframe (train)

##### Checking for common support between control and treatment group!


##### Testing differences in means of distributions of outcome between treatment and control group!

# see: Towards Data Science Article on iPhone images!




