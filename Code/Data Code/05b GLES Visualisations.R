# GLES 2021 Study Visualizations
# Date: 23.08.2022
# Author: Alec Eisenkolb

# For this code to work, user must have previously run the Twitter API functions
# "Twitter API Code.py", as well as the previous data cleaning function in R:
# "01 Data Cleaning.R". Notice that the GLES 2021 dataset has been merged together
# by the GESIS Institute for data privacy purposes, which cannot be replicated by 
# code available in this project but was done externally. 

# install.packages("sjlabelled")
# install.packages("gridExtra")
# install.packages("cowplot")

# load libraries
library(tidyverse)
library(ggplot2)
library(ggthemr)
library(sjlabelled)
library(mgsub)
library(gridExtra)
library(cowplot)

# set graph colour theme
# define colour palette
green_palette <- c("#4b6043", "#658354", "#75975e", "#87ab69", "#95bb72", "#a3c585",
                   "#b3cf99", "#c7ddb5", "#ddead1")
# define colour to outline boxes
green_palette <- c("#555555", green_palette)
# remove previous theme and set new theme
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

# set new theme as default
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)

# import data
df_gles <- read_csv("Clean Data/GLES_2021.csv")

# create tribble to transform party from numeric to string name (taken from labels from df_gles in Data Cleaning script)
party_table <- tribble(
  ~Number, ~Name,
  1, "CDU",
  2, "CDU",
  3, "CSU",
  4, "SPD",
  5, "FDP",
  6, "GRÜNE",
  7, "DIE LINKE",
  322, "AfD"
)

# Distribution of party alliance in this data
df_gles %>%
  mutate(partei = as.character(mgsub(partei, party_table$Number, party_table$Name))) %>%
  group_by(partei) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(partei, -n), y=n)) +
  geom_bar(stat="identity") +
  xlab("Party") +
  ylab("Observations")

# When did candidates begin their election campaign? Wahlkampfbeginn Frage b1
df_gles %>% 
  select(b1a, b1b) %>%
  pivot_longer(c("b1a", "b1b"), names_to = "Description", values_to = "Value") %>%
  mutate(Description = if_else(Description=="b1a", "General", "Full-time")) %>%
  ggplot(aes(x=Value, fill=factor(Description, levels=c("General", "Full-time")))) +
  geom_bar(position="dodge") +
  xlab("Begin of Election Campaign (Prior to Election)") +
  ylab("Frequency") +
  scale_x_discrete(limit=c(1, 2, 3, 4, 5),
                   labels=c("> 6 Months", "3-6 Months", "1-3 Months", 
                            "< 1 Months", "Never")) +
  labs(fill="Legend")
  
# How much time did candidates allocate to their campaign? Wahlkampf Zeitaufwand Frage b2
# Note here that variable was often encoded in groups of 1-10, 11-20 etc... so note there are peaks here on the averages
# as GESIS chose the average of the range whenever a range was given. 
df_gles %>%
  filter(b2<=150) %>%
  ggplot(aes(x=b2)) +
  #geom_histogram(color = "#000000", binwidth = 10) +
  geom_density() +
  xlab("Hours per Week") +
  ylab("Density")

# Time spent for political purposes during "normal" times
df_gles %>%
  ggplot(aes(x=a4)) +
  geom_bar() +
  xlab("Hours per Week") +
  ylab("Frequency") +
  scale_x_discrete(limit=c(0, 1, 2, 3, 4, 5, 6),
                   labels=c("None", "1-5 Hours", "6-10 Hours", "11-20 Hours", 
                            "21-30 Hours", "31-40 Hours", ">40 Hours"))

# Size of election campaign teams? Teamumfang Frage b3
# histogram
df_gles %>%
  filter(b3a <=100) %>%
  ggplot(aes(x=b3a)) +
  geom_histogram(color = "#000000", binwidth=3) +
  xlab("Team Size") +
  ylab("Count") +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

# by party
df_gles %>%
  mutate(partei = as.character(mgsub(partei, party_table$Number, party_table$Name))) %>%
  group_by(partei) %>%
  summarise(`Avg Team Size` = mean(b3a, na.rm=TRUE)) %>%
            #Paid_mean = mean(b3b, na.rm=TRUE),
            #Volunt_mean = mean(b3c, na.rm=TRUE)) %>%
  pivot_longer(!partei, names_to = "Team_Size", values_to = "Value") %>%
  ggplot(aes(x=reorder(partei, -Value), y=Value, fill=Team_Size)) +
  geom_bar(position="stack", stat="identity") +
  labs(fill="Legend") +
  xlab("Party") +
  ylab("Mean Team Size") +
  theme(legend.position = "None")

# Budget of election campaign? Budget Frage b5 (combine with b6 for a detailed split of budget origin)
df_gles %>%
  mutate(b5 = as.numeric(b5/1000)) %>%
  ggplot(aes(x=b5)) +
  #geom_histogram(color="#000000", binwidth=5) +
  geom_density() +
  xlab("Budget for Campaign ('000 €)") +
  ylab("Frequency") + 
  scale_x_continuous(breaks = seq(0, 200, by = 20))
  
# by type of money origin
df_gles %>%
  select(b5, b6a, b6b, b6c) %>%
  mutate(b6a = as.numeric((b6a/100)*b5/1000), # obtain nominal budget from percentage values
         b6b = as.numeric((b6b/100)*b5/1000),
         b6c = as.numeric((b6c/100)*b5/1000)) %>%
  rename(`00 Total Budget` = b5,
         `01 Party Funds` = b6a,
         `03 Donations` = b6b,
         `02 Private` = b6c) %>%
  pivot_longer(c(`00 Total Budget`, `01 Party Funds`, `03 Donations`, `02 Private`), 
               names_to = "Description", values_to = "Value") %>%
  filter(Description != "00 Total Budget") %>%
  ggplot(aes(x=Value, fill=Description)) +
  geom_density(color="#000000", alpha=0.6) +
  xlim(0, 20) +
  xlab("Budget ('000 Euros)") +
  ylab("Density") + 
  labs(fill = "Legend") +
  scale_fill_discrete(labels=c("Party Funds", "Private", "Donations"))

# Which type of campaigning did you engage in? Wahlkampfaktivität Frage b7
df_gles %>%
  select(starts_with("b7")) %>%
  rename(`Home Visit` = b7a,
         `Booth` = b7b,
         `Business Visit` = b7c,
         `Info Document` = b7d,
         `Consult. Hour` = b7e,
         `Online Event` = b7f,
         `Telephone` = b7g,
         `SMS` = b7h,
         `Party Event` = b7i,
         `Interview/Press` = b7j,
         `Speech` = b7k,
         `Letter` = b7l,
         `E-Mail` = b7m,
         `Poster` = b7n,
         `Magazine Ad` = b7o,
         `Advertisements` = b7p,
         `Brochures/Freebies` = b7q,
         `Webpage` = b7r,
         `Facebook` = b7s,
         `Twitter` = b7t,
         `Youtube` = b7u,
         `Abgeordnetenwatch` = b7v) %>%
  #mutate_all(~if_else(. == 0, 0, 1)) %>% # uncomment this if graph should show nominal count of activity, otherwise shows activity * weight of importance
  colSums(., na.rm = TRUE) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Description") %>%
  rename(Value = ".") %>%
  ggplot(aes(x=reorder(Description, -Value), y=Value)) +
  geom_bar(stat = "identity") +
  #xlab("Campaign Activity") +
  ylab("Measure of Importance") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        axis.title.x = element_blank())

# How did you use social media? Soziale Medien Frage b8
b8a <- df_gles %>%
  ggplot(aes(x=b8a)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("From Pressure of Competition?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 
b8b <- df_gles %>%
  ggplot(aes(x=b8b)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("For Greater Reach in Trad. Media?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 
b8c <- df_gles %>%
  ggplot(aes(x=b8c)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("Easier to Persuade Electorate?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 
b8d <- df_gles %>%
  ggplot(aes(x=b8d)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("Vulnerable to Data Privacy and Manipulation?") +
  theme(plot.title = element_text(size=10),
        axis.title.y = element_blank()) + 
  ylim(0, 300) 
b8e <- df_gles %>%
  ggplot(aes(x=b8e)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("For Direct Contact to Electorate?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 
b8f <- df_gles %>%
  ggplot(aes(x=b8f)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("To Better Focus on Selected Topics?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 
b8g <- df_gles %>%
  ggplot(aes(x=b8g)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("E-Campaigning is too much Effort?") +
  theme(plot.title = element_text(size=10),
        axis.title.y = element_blank()) + 
  ylim(0, 300) 
b8h <- df_gles %>%
  ggplot(aes(x=b8h)) +
  geom_bar() +
  theme(axis.title.x = element_blank()) +
  ylab("Count") +
  ggtitle("Easier Communication Online?") +
  theme(plot.title = element_text(size=8),
        axis.title.y = element_blank()) + 
  ylim(0, 350) 

# combine all boxplots
# plot grid of all positively-phrased questions
plot_grid(b8a, b8b, b8c, b8e, b8f, b8h,
          nrow = 2, ncol = 3)

# plot grid of the two negatively-phrased questions
plot_grid(b8d, b8g, nrow = 1, ncol = 2)











