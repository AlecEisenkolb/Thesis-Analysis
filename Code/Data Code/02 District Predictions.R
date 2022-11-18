# District Predictions 100 days prior to election
# Date: 16.06.2022
# Author: Alec Eisenkolb

### Data and some code taken from following source: 

### Gschwend, Thomas, Klara Müller, Simon Munzert, Marcel Neunhoeffer, 
### Lukas F. Stoetzer (2022). “The Zweitstimme Model: A Dynamic Forecast of the 
### 2021 German Federal Election.” PS: Political Science & Politics 55(1): 85–90

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(tidyverse,
               readr)

# set path for raw data
PATH <- "Raw Data/"

# path and import district predictions
dist_pred <- readRDS(paste0(PATH, "Wkr Forecast/2021_wkr.RDS"))

# create empty dataframe to store results in
forecast <- as.data.frame(matrix(NA, 
                                 nrow = 299,
                                 ncol = 7))
colnames(forecast) <- c("district_num", "SPD", "CDUCSU", "GRUENE", "FDP", "LINKE", "AFD")

### the following code (next 10 lines of code) has been written by the research  
### team to process the RDS file and compute predictions
nsim <- nrow(forecast)
mu_nsim <- 25
# compute matrix 
dist_pred <- matrix(unlist(lapply(dist_pred, "[[", "winner_nn")), 
                    nrow = nsim * mu_nsim, 
                    ncol = 299, 
                    byrow = T)
# compute winning probabilities for parties in each election district
dist_pred <- apply(dist_pred, 2, function(x)
  - sort(-table(x) / nrow(dist_pred)))

# define list of parties 
parties <- c("SPD", "CDU", "GRUENE", "FDP", "LINKE", "AFD")

# fill dataframe with values from list "dist_pred"
for (i in 1:299){
  # temporarily store election district's i results
  tmp <- dist_pred[[i]]
  
  # fill district number
  forecast[i, "district_num"] <- as.character(str_pad(i, 3, side = "left", pad = "0"))
  
  # loop over all parties
  for (party in parties){
    # if party is CDU we must also include CSU 
    if (party == "CDU"){
      if ("CDU" %in% names(tmp) | "CSU" %in% names(tmp)){
        # store results in dataframe
        forecast[i, "CDUCSU"] <- as.numeric(tmp[names(tmp)=="CDU" | names(tmp)=="CSU"])
        
      } else {
        # else fill with winning probability of 0
        forecast[i, "CDUCSU"] <- 0
        
      }
    } else {
      # check if name of party is in forecast
      if (party %in% names(tmp)){
        # store results in dataframe
        forecast[i, party] <- as.numeric(tmp[names(tmp)==party])
        
      } else {
        # else fill with winning probability of 0
        forecast[i, party] <- 0
      }
    }
  }
}

# pivot longer for easier merging with candidates dataset
forecast <- forecast %>%
  pivot_longer(!district_num, names_to = "party", values_to = "Prediction")

# rename entries for Parties in forecast table to match master dataframe
# change encoding of "CDUCSU" to CSU for bavarian districts, source: https://www.bundeswahlleiter.de/bundestagswahlen/2021/wahlkreiseinteilung/bund-99/land-9.html
forecast <- forecast %>%
  mutate(party = if_else(party == "AFD", "AfD", party),
         party = if_else(party == "GRUENE", "GRÜNE", party),
         party = if_else(party == "LINKE", "DIE LINKE", party),
         party = if_else(party == "CDUCSU" & (district_num >= 212 & district_num <= 257), "CSU", party), # select only bavarian districts and change encoding to CSU
         party = if_else(party == "CDUCSU" & !(district_num >= 212 & district_num <= 257), "CDU", party)) # the non-bavarian districts are encoded as CDU

# import master_nontwitter.csv and GLES data to merge the district forecast
master_df <- read_csv("Clean Data/master_all_cand.csv")
df_gles <- read_csv("Clean Data/GLES_2021.csv")

# check party encodings are the same for both datasets
unique(forecast$party)
unique(master_df$party)
unique(df_gles$party)

# merge data with district-level prediction
master_df <- master_df %>%
  left_join(forecast, by = c("district_num", "party"))

# count NA entries for "prediction" variable in master dataframe which we just added
colSums(is.na(master_df)) 

# merge gles data with district-level predictions
df_gles <- df_gles %>%
  left_join(forecast, by = c("district_num", "party"))

# write both master and gles dataset
write_csv(master_df, "Clean Data/master_all_cand.csv")
write_csv(df_gles, "Clean Data/GLES_2021.csv")

# clean memory
rm(master_df, forecast, parties, dist_pred, df_gles)
