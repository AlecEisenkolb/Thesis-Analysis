# Master Thesis Data prep (non-twitter data)
# Date: 21.04.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)

# set path
PATH <- "/Users/alec/Dropbox/HSG MEcon/Master Thesis/Raw Data/btw21 ergebniss/"
DTA <- "BWL_Endgültig.csv"

# import data
election_df <- read.csv(paste0(PATH, DTA), sep = ";", skip = 8, header = TRUE)

rm(election_df)