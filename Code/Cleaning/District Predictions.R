# District Predictions
# Date: 11.05.2022
# Author: Alec Eisenkolb

### Data and some code taken from following source: 

### Gschwend, Thomas, Klara Müller, Simon Munzert, Marcel Neunhoeffer, 
### Lukas F. Stoetzer (2022). “The Zweitstimme Model: A Dynamic Forecast of the 
### 2021 German Federal Election.” PS: Political Science & Politics 55(1): 85–90

# import libraries
library(tidyverse)
library(readr)

# set path for raw data
PATH <- "Raw Data/"

# path and import
dist_pred <- readRDS(paste0(PATH, "Wkr Forecast/2021_wkr.RDS"))

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

# create empty dataframe to store results in
forecast <- as.data.frame(matrix(NA, 
                                 nrow = 299,
                                 ncol = 7))
colnames(forecast) <- c("district_num", "SPD", "CDUCSU", "GRUENE", "FDP", "LINKE", "AFD")

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





