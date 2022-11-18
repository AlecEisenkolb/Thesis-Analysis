# Twitter Data Preparation 
# REPLIES by Candidates
# Date: 16.06.2022
# Author: Alec Eisenkolb

# Not used in Thesis

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(tidyverse,
               readr)

# import replies dataframe
df_replies <- read_csv(paste0(PATH, "Twitter API/replies.csv"), col_types = "ccccc")
df_retweets <- read_csv(paste0(PATH, "Twitter API/retweets.csv"))

# check memory size of dataframe (1 million+ observations)
object.size(df_replies)

# check column classes
sapply(df_replies, class)