# Twitter Data Preparation 
# Main Tweets by Candidates
# Date: 19.06.2022
# Author: Alec Eisenkolb

# For this code to work, user must have previously ran the Twitter API function
# to scrape main tweets of candidates "Twitter API.py".

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(tidyverse,
               readr,
               stringr)

# set path for raw data
PATH <- "Raw Data/Twitter API/"
EXPORT_PATH <- "Clean Data/Twitter/"
IMPORT_PATH <- "Clean Data/"

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
#user_check <- read.csv(paste0(PATH, "tweet_check.csv"))
#dim(user_check)
#unique(user_check$User.ID)

### tweets by these users were 149 in total.
### Only one of these users had tweets, three others did not tweet at all.

# Cross-check with complete twitter dataset
#check <- df_tweets %>%
#  filter(User.ID == 409258073 | User.ID == 2288663038 | User.ID == 137036352 | 
#           User.ID == 738636568760553472)

#dim(check)
#unique(check$User.ID)

### Filtering for identical user IDs in the total Twitter dataset leads to the
### same results - hence I can conclude that the interruption in the Twitter API
### did not lead to a false scraping of Tweets by these specific users affected
### by the API interruption. 

# clean memory
#rm(check, user_check)

# # # # # STEP 3: Change column names # # # # # 

# change column names, by removing "." from all colnames
colnames(df_tweets) <- str_replace_all(colnames(df_tweets), "[:punct:]", " ")

# Delete the index row "X"
df_tweets <- df_tweets %>%
  select(-`X`)

# # # # # STEP 4: Further data manipulation # # # # #

# create Day variable from datetime and convert IDs to strings
df_tweets <- df_tweets %>%
  mutate(Day = as.POSIXct(substr(Date, 0, 10), format = "%Y-%m-%d"), 
         `User ID` = as.character(`User ID`), 
         `Tweet ID` = as.character(`Tweet ID`),
         `Parent Tweet ID` = as.character(`Parent Tweet ID`))

# check column types
sapply(df_tweets, class)

# export df_tweets as CSV for further API progress
write.csv(df_tweets, paste0(EXPORT_PATH, "twitter_clean.csv"), row.names = FALSE)

# import dataframe with candidate names and Twitter User IDs 
# NOTE: merging by user ID is not possible as some candidates have multiple twitter accounts
df_user_ids <- read_csv(paste0(IMPORT_PATH, "Twitter/twitter_ids.csv"), col_types = "cccc")

# rename column such that it matches for merging
df_user_ids <- df_user_ids %>%
  rename(`User ID` = user_id) 

# merge twitter data with dataframe of candidate names & 
# create time variables indicating postings to be pre or post election
User_Tweet_Info <- df_tweets %>%
  left_join(df_user_ids, by = "User ID") %>%
  mutate(Pre_Elec = if_else(Day <= as.POSIXct("2021-09-26"), 1, 0), # create Dummy whether post was made before election
         Time_group = if_else(format(Day, "%m") == "07", 1, # category 1: posts made in July
                              if_else(format(Day, "%m") == "08", 2, # category 2: posts made in August
                                      if_else(format(Day, "%m") == "09" & Day <= as.POSIXct("2021-09-26"), 3, # category 3: posts in September AND before election
                                              if_else(Day > as.POSIXct("2021-09-26"), 4, as.numeric(NA)))))) # category 4: posts after election
  
# group by candidates names and summarise information on likes, replies and re-tweets
Tweet_summary <- User_Tweet_Info %>%
  group_by(firstname, lastname) %>%
  summarise(Sum_Posts = sum(n()),
            Avg_Weekly_Posts = (sum(n())/102)*7, # average weekly posts: divide by # of days (102) and multiply by 7 days per week (1st July - 10th Oct. 2021)
            Mean_Likes = mean(Likes),
            Min_Likes = min(Likes),
            Max_Likes = max(Likes),
            Med_Likes = median(Likes),
            Sd_Likes = sd(Likes),
            Mean_Reply = mean(Replies),
            Min_Reply = min(Replies),
            Max_Reply = max(Replies),
            Med_Reply = median(Replies),
            Sd_Reply = sd(Replies),
            Mean_RT = mean(Retweets),
            Min_RT = min(Retweets),
            Max_RT = max(Retweets),
            Med_RT = median(Retweets),
            Sd_RT = sd(Retweets))

# group by candidate names and timings of posts (pre and post election only)
Tweet_summary_posts <- User_Tweet_Info %>%
  group_by(firstname, lastname, Pre_Elec) %>%
  summarise(Avg_Posts = n()) %>%
  pivot_wider(names_from = "Pre_Elec", values_from = "Avg_Posts", names_prefix = "Pre_Elec_") %>%
  mutate(Pre_Elec_0 = if_else(is.na(Pre_Elec_0), as.numeric(0), as.numeric(Pre_Elec_0)),
         Pre_Elec_1 = if_else(is.na(Pre_Elec_1), as.numeric(1), as.numeric(Pre_Elec_1))) %>%
  mutate(Pre_Elec_0 = as.numeric((Pre_Elec_0/14)*7), # average weekly posts after election (14 days between 27.09 - 10.10.2021)
         Pre_Elec_1 = as.numeric((Pre_Elec_1/88)*7)) %>% # average weekly posts before election (88 days between 01.07 - 26.09.2021)
  rename(Avg_posts_pre_elec1 = Pre_Elec_1,
         Avg_posts_pre_elec0 = Pre_Elec_0)
  
# group by candidate names and timings of posts (using different Time groups)
Tweet_summary_posts2 <- User_Tweet_Info %>%
  group_by(firstname, lastname, Time_group) %>%
  summarise(Avg_Posts = n()) %>%
  pivot_wider(names_from = "Time_group", values_from = "Avg_Posts", names_prefix = "Time_group") %>%
  mutate_at(vars(Time_group1:Time_group4), ~replace(., is.na(.), 0)) %>% 
  mutate(Time_group1 = as.numeric((Time_group1/31)*7), # average weekly posts in July (31 days between 01.07 - 31.07.2021)
         Time_group2 = as.numeric((Time_group2/31)*7), # average weekly posts in August (31 days between 01.08 - 31.08.2021)
         Time_group3 = as.numeric((Time_group3/26)*7), # average weekly posts in September until election (26 days between 01.09 - 26.09.2021)
         Time_group4 = as.numeric((Time_group4/14)*7)) %>% # average weekly posts after election (14 days between 27.09 - 10.10.2021)
  rename(Avg_posts_time_group1 = Time_group1,
         Avg_posts_time_group2 = Time_group2,
         Avg_posts_time_group3 = Time_group3,
         Avg_posts_time_group4 = Time_group4)

# obtain information on language of tweets
df_tweet_info <- df_tweets %>%
  left_join(df_user_ids, by = "User ID") %>%
  group_by(firstname, lastname, Language) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "Language", values_from = "count") %>%
  mutate_all(~replace(., is.na(.), 0))

# obtain an order of which languages occur most often
lang <- df_tweet_info %>%
  select(-firstname, -lastname) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup()

# determine 5 most used languages
top5_langs <- (order(-colSums(lang[1:38]))[1:5]) + 2 # add 2 here, as the column index needs to take into account 2 further columns "firstname" and "lastname" in original data
other_langs <- (order(-colSums(lang[1:38]))[6:38]) + 2

# summarise language information per user for top 5 languages in general plus category "others"
df_tweet_info <- df_tweet_info %>%
  select(firstname, lastname, other_langs) %>%
  mutate(others = rowSums(.[3:35], na.rm = TRUE)) %>%
  select(firstname, lastname, others) %>%
  left_join(df_tweet_info %>% select(firstname, lastname, top5_langs), by = c("firstname", "lastname")) %>%
  rename_with( ~ paste0("lang_", .), .cols = c("de", "en", "nl", "others", "fr", "und"))

# Merge with other tweet info dataset
df_tweet_info <- df_tweet_info %>%
  left_join(Tweet_summary, by = c("firstname", "lastname")) %>%
  left_join(Tweet_summary_posts, by = c("firstname", "lastname")) %>%
  left_join(Tweet_summary_posts2, by = c("firstname", "lastname"))

### merge with main candidates dataframe (master_all_cand)
# import master candidates dataframe
df_master <- read_csv(paste0(IMPORT_PATH, "master_all_cand.csv"))

# merge data and encode the mean posting activity, likes, replies and retweets
# for Twitter account holders who were not active at all from NA to 0. 
variables <- c("Avg_Weekly_Posts", "Avg_posts_pre_elec0", "Avg_posts_pre_elec1",
               "Avg_posts_time_group1", "Avg_posts_time_group2", "Avg_posts_time_group3",
               "Avg_posts_time_group4", "Mean_Likes", "Mean_Reply", "Mean_RT")

df_master <- df_master %>%
  left_join(df_tweet_info, by = c("firstname", "lastname")) %>%
  mutate(across(all_of(variables), ~ if_else((is.na(.) & Twitter_Acc == 1), as.numeric(0), as.numeric(.))))

# save master dataframe
write.csv(df_master, "Clean Data/master_all_cand.csv", row.names = FALSE)

# clean memory
rm(df_tweet_info, df_user_ids, User_Tweet_Info, df_tweets, df_master, lang, 
   Tweet_summary, Tweet_summary_posts, Tweet_summary_posts2)
