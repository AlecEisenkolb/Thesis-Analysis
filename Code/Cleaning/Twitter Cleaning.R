# Twitter Data Preparation
# Date: 04.05.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)

# set path for raw data
PATH <- "Raw Data/Twitter API/"

# import tweets dataset
df_tweets <- read.csv(paste0(PATH, "tweets.csv"), colClasses = c(rep("character", 13)))

# # # # # STEP 1: Cleaning/fixing wrongly compiled tweets # # # # #

# identify rows which did not compile correctly (5 tweets in total need to be fixed)
test <- df_tweets[which(df_tweets$Likes=="None"), ]

# Row: 92888
### As illustration: the 5 wrongly coded tweets look as follows: (could be fixed 
### in a quick for-loop as well, but illustration is easier this way)
### Due to media link being falsely included, the info for columns 6 to 13 are 
### shifted to columns 2 to 9 of the next row.Hence we shift this information 
### back up, and delete the falsely codified row. This has been checked manually 
### on Twitter too, to make sure my changes are correct!
illustrate_row <- df_tweets[c(92887, 92888),]
df_tweets[92887, 6:13] <- df_tweets[92888, 2:9]

# Row: 109336
illustrate_row <- df_tweets[c(109335, 109336),]
df_tweets[109335, 6:13] <- df_tweets[109336, 2:9]

# Row: 122727
illustrate_row <- df_tweets[c(122726, 122727),]
df_tweets[122726, 6:13] <- df_tweets[122727, 2:9]

# Row: 131497
illustrate_row <- df_tweets[c(131496, 131497),]
df_tweets[131496, 6:13] <- df_tweets[131497, 2:9]

# Row: 178789
illustrate_row <- df_tweets[c(178788, 178789),]
df_tweets[178788, 6:13] <- df_tweets[178789, 2:9]

# Remove all rows which were falsely coded, from above (not done earlier as to not change the indices of the wrong rows)
df_tweets <- df_tweets[-c(92888, 109336, 122727, 131497, 178789),]

# Re-code classes of columns to numeric/character where necessary


# # # # # STEP 2: Check consistency of tweets from users during breaks # # # # #

### 4 user IDs were interrupted when exceeding API rate limits, the tweets from
### these users were compiled separately to check the consistency of the data  
### collection to ensure the interruptions did not cause any tweets to be missed.

# import tweets by 4 specific user IDs - indices were 277, 556, 823 and 1075
user_check <- read.csv(paste0(PATH, "tweet_check.csv"))
dim(user_check)
unique(user_check$User.ID)

### tweets by these users were 149 in total.
### Only one of these users had tweets, three others did not tweet at all.

# Cross-check with complete twitter dataset
check <- df_tweets %>%
  filter(User.ID == 409258073 | User.ID == 2288663038 | User.ID == 137036352 | 
           User.ID == 738636568760553472)

dim(check)
unique(check$User.ID)

### Filtering for identical user IDs in the total Twitter dataset leads to the
### same results - hence I can conclude that the interruption in the Twitter API
### did not lead to a false scraping of Tweets by these specific users affected
### by the API interruption. 

# clean memory
rm(check, user_check)

# # # # # STEP 3: # # # # # 
