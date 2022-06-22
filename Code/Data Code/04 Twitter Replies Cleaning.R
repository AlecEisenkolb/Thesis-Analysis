# Twitter Data Preparation 
# REPLIES by Candidates
# Date: 16.06.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)
library(readr)

# import replies dataframe
df_replies <- read_csv(paste0(PATH, "replies.csv"), col_types = "ccccc")

# check memory size of dataframe (1 million+ observations)
object.size(df_replies)

# check column classes
sapply(df_replies, class)