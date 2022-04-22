# Non-Twitter data preparation
# Date: 22.04.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)

# set path for raw data
PATH <- "Raw Data/"

##### ------------------ import election results dataset ----------------- #####
# path and import
DTA_Elec <- "btw21 ergebniss/BWL_Endgültig.csv"
election_df <- read.csv(paste0(PATH, DTA_Elec), sep = ";", skip = 9, header = TRUE, dec = ",")

# clean data
election_df <- election_df %>%
  filter(Gebietsart == "Wahlkreis", # remove results for BUND and LÄNDER
         Gruppenart != "System-Gruppe") %>% # remove non party results
  mutate(Gebietsnummer = str_pad(Gebietsnummer, 3, side = "left", pad = "0")) %>% # create standardized notation for Wahlkreise numbers
  select(Gebietsnummer, Gebietsname, Gruppenart, Gruppenname, Stimme, Anzahl, 
         Prozent) %>%
  pivot_wider(names_from = Stimme, values_from = c(Anzahl, Prozent)) %>% # Create wide version of election results
  mutate(PztPkt_diff_1to2 = as.numeric((Prozent_1 - Prozent_2))) %>% # compute outcome variable: difference of erststimme to zweitstimme per wahlkreis
  filter(PztPkt_diff_1to2 != is.na(PztPkt_diff_1to2))

# check whether there is one result per Wahlkreis and party (for uniqueness to merge)
election_df %>%
  group_by(Gebietsnummer, Gruppenname) %>%
  summarize(PartyCand = n()) %>%
  ungroup() %>%
  distinct(PartyCand)

# check classes of columns
sapply(election_df, class)

##### --------------------- import candidacy dataset --------------------- #####
# path and import
DTA_Cand <- "btw21_kandidaturen_utf8.csv"
candidate_df <- read.csv(paste0(PATH, DTA_Cand), sep = ";", skip = 8, header = TRUE, dec = ",")

# clean data
candidate_df <- candidate_df %>%
  filter(Gebietsart == "Wahlkreis") %>% # filter for candidates directly electable only
  mutate(Gebietsnummer = str_pad(Gebietsnummer, 3, side = "left", pad = "0")) %>% # standardize notation for Wahlkreis numbers
  mutate(Geschlecht = if_else(Geschlecht == "m", 1, 0), # change encoding of gender variable
         VorpGewaehlt = if_else(VorpGewaehlt == "X", 1, 0)) %>% # change encoding of incumbent variable
  select(Nachname, Vornamen, Geschlecht, Geburtsjahr, Geburtsort, PLZ, Wohnort, 
         WohnortLandAbk, Staatsangehörigkeit, Beruf, Berufsschluessel, Gebietsnummer, 
         Gruppenname, GruppennameLang, VerknKennzeichen, VerknGebietsname, VerknGruppenname, 
         VerknListenplatz, VorpGewaehlt) # select important variables

# check whether there is one candidate per Wahlkreis and party (for uniqueness to merge)
candidate_df %>%
  group_by(Gebietsnummer, Gruppenname) %>%
  summarize(PartyCand = n()) %>%
  ungroup() %>%
  distinct(PartyCand)

# check classes of columns
sapply(candidate_df, class) 

##### --------------------- import structural dataset -------------------- #####
# path and import
DTA_Struc <- "btw21_strukturdaten.csv"
structural_df <- read.csv(paste0(PATH, DTA_Struc), sep = ";", skip = 8, header = TRUE) %>%
  select(-`Fußnoten`)

# check classes - numerics are in character mode as german thousand and decimal seperator is different
sapply(structural_df, class)

# clean data
structural_df <- structural_df %>%
  mutate(across(c(4:ncol(structural_df)), gsub, pattern = ".", replacement = "", fixed = TRUE)) %>% # delete german thousand seperator
  mutate(across(c(4:ncol(structural_df)), gsub, pattern = ",", replacement = ".", fixed = TRUE)) %>% # replace german decimal seperator with "."
  mutate(across(c(4:ncol(structural_df)), as.numeric)) %>% # transform columns to numeric
  rename()
  
# check classes again
sapply(structural_df, class)

##### --------------------------- merge datasets ------------------------- #####
# merge
master_df <- election_df %>%
  left_join()

# Election dataset has few less observations that candidacy dataset, as election data
# combines smaller parties into a category of "others", thus removing the results of
# direct candidates of much smaller parties, which are still listed in the candidacy
# dataset. 






