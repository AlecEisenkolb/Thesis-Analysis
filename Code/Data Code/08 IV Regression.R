# Robustness Check: Instrumental Variable Regression using GLES 2021 Data
# Date: 23.08.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)

# import data
df_gles <- read_csv("Clean Data/GLES_2021.csv")

# create IV variable from response b8 (8 questions on politicians views on social media)
df_gles <- df_gles %>%
  filter(kandidaturtyp == 2 | kandidaturtyp == 3) %>% # filter only for direct candidates
  mutate(b8d = if_else(b8d==1, 5,
                       if_else(b8d==2, 4,
                               if_else(b8d==4, 2,
                                       if_else(b8d==5, 1, as.numeric(b8d))))),
         b8g = if_else(b8g==1, 5,
                       if_else(b8g==2, 4,
                               if_else(b8g==4, 2,
                                       if_else(b8g==5, 1, as.numeric(b8g)))))) %>%
  mutate(SM_preference = as.numeric(b8a + b8b + b8c + b8d + b8e + b8f + b8g + b8h)/8) # create preference variable

# graph the IV variable
df_gles %>%
  ggplot(aes(x=SM_preference)) +
  geom_bar() +
  xlab("Social Media Relevance") +
  ylab("Frequency") +
  xlim(1, 5)

### Run equivalent regressions on GLES dataset as on master data



### Run IV regression on GLES data as robustness check

# check correlation between instrument and treatment (twitter account)
cor(df_gles$SM_preference, df_gles$twitter_acc, use = "complete.obs") # correlation = 0.115

# check correlation between instrument and twitter use
cor(df_gles$SM_preference, df_gles$avg_weekly_posts, use = "complete.obs") # correlation = 0.0368




