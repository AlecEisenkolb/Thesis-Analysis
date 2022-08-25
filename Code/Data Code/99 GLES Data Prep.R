### Script to anonymise data from GLES and merge with Twitter data
# Author: Alec Eisenkolb
# Date: 28.07.2022

library(tidyverse)
library(readxl)

df_master <- read_csv("Clean Data/master_all_cand.csv")

df_master <- df_master %>%
  select(firstname, lastname, party, district_num, sex, Twitter_Acc, percent_1,
         percent_2, winner, East_Germany, starts_with("lang"),
         101:ncol(df_master)) %>%
  mutate(sex = if_else(firstname == "Heinrich Alexandra" & district_num == "286", as.numeric(1), as.numeric(sex)))

df_master %>%
  filter(district_num == "286") %>%
  view()

df_gles <- read_excel("/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis/Raw Data/GLES 2021/Kandidierende_BTW2021_Merge_Data.xlsx")

df_gles <- df_gles %>%
  mutate(gender = if_else(Geschlecht == "weiblich", as.numeric(0), as.numeric(1)),
         district_num = str_pad(Wahlkreisnummer, 3, side = "left", pad = "0")) %>%
  rename(firstname = Vorname,
         lastname = Nachname,
         party = Partei,
         sex = gender)

unique(df_gles$party)
unique(df_master$party)

df_merged <- df_master %>%
  left_join(df_gles, by = c("lastname", "party", "sex", "district_num"))

sum(is.na(df_merged$kid))

df_merged <- df_merged %>%
  select(kid, 6:39)

df_merged <- df_merged %>%
  rename(Percent_Election_Erststimme = percent_1,
         Percent_Election_Zweitstimme = percent_2)

df_gles2 <- df_gles %>%
  select(kid)

df_merged2 <- df_gles2 %>%
  left_join(df_merged, by = "kid")

# export
write.csv(df_merged2, "Raw Data/GLES 2021/GLES_Twitter_Data.csv", row.names = FALSE)



