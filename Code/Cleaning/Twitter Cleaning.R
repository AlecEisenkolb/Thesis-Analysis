# Twitter Data Preparation
# Date: 05.05.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)
library(readr)

# set path for raw data
PATH <- "Raw Data/Twitter API/"
EXPORT_PATH <- "Clean Data/Twitter/"

# import tweets dataset
df_tweets <- read.csv(paste0(PATH, "tweets.csv"), colClasses = c(rep("character", 13)))

# # # # # STEP 1: Cleaning/fixing wrongly compiled tweets # # # # #

# identify rows which did not compile correctly (5 tweets in total need to be fixed)
bad_tweets <- df_tweets[which(df_tweets$Likes=="None"), ] # "No" likes should be indicated by a numeric "0", not "None"!
indices <- rownames(bad_tweets)

# Row: 92888
### As illustration: the 5 wrongly coded tweets look as follows: 
### Due to media link being falsely included, the info for columns 6 to 13 are 
### shifted to columns 2 to 9 of the next row. Hence we shift this information 
### back up, and delete the falsely codified row. This has been checked manually 
### on Twitter too, to make sure my changes are correct!
illustrate_row <- df_tweets[c(92887, 92888),]

# Issues with these rows are fixed in the for-loop below
# looping over indices 92888, 109336, 122727, 131497 and 178789
for (index in indices){
  # Place value of columns 2-9 of index rows into respective row before
  df_tweets[as.numeric(index)-1, 6:13] <- df_tweets[as.numeric(index), 2:9]
  
}

# Check the row numbers 92885-92889 whether transformation was correct
check <- df_tweets[92885:92889,]

# Remove all rows which were falsely coded
df_tweets <- df_tweets[-c(92888, 109336, 122727, 131497, 178789),]

# Re-code classes of columns to numeric/character where necessary
df_tweets <- df_tweets %>%
  mutate_at(vars(Likes, Replies, Retweets, Quotes), ~as.numeric(as.character(.)))

sapply(df_tweets, class)

# clean memory
rm(bad_tweets, check)

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

# # # # # STEP 3: Final aesthetic cleaning before exporting # # # # # 

# change column names, by removing "." from all colnames
colnames(df_tweets) <- str_replace_all(colnames(df_tweets), "[:punct:]", " ")

# Delete the index row "X"
df_tweets <- df_tweets %>%
  select(-`X`)

# export df_tweets as csv for further API progress 
write.csv(df_tweets, paste0(EXPORT_PATH, "twitter_clean.csv"))


# # # # # Replies dataset # # # # #
# import replies dataframe
df_replies <- read_csv(paste0(PATH, "replies.csv"), col_types = "ccccc")

# check memory size of dataframe (1 million+ observations)
object.size(df_replies)

# check column classes
sapply(df_replies, class)


