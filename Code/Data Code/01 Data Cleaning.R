# Non-Twitter data preparation
# Date: 22.08.2022
# Author: Alec Eisenkolb

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(Hmisc,
               tidyverse,
               haven,
               fauxnaif)

# set path for raw data
PATH <- "Raw Data/"

##### ------------------ import election results dataset ----------------- #####
### This dataset includes information on the district election results for 2021
### and 2017 federal elections, including percent and absolute votes for 
### Erststimme (direct candidates) and Zweitstimme (party vote) per district. 

# path and import
DTA_Elec <- "btw21 ergebniss/BWL_Endgültig.csv"
election_df <- read.csv(paste0(PATH, DTA_Elec), sep = ";", skip = 9, header = TRUE, dec = ",")

# clean data
election_df <- election_df %>%
  filter(Gebietsart == "Wahlkreis", # remove results for BUND and LÄNDER
         Gruppenart != "System-Gruppe") %>% # remove non party results
  mutate(Gebietsnummer = str_pad(Gebietsnummer, 3, side = "left", pad = "0")) %>% # create standardized notation for Wahlkreise numbers
  select(Gebietsnummer, Gebietsname, Gruppenname, Stimme, Anzahl, 
         Prozent) %>% # selecting specific variables (removing election results from past election)
  pivot_wider(names_from = Stimme, values_from = c(Anzahl, Prozent)) %>% # Create wide version of election results
  mutate(Pzt_diff_1to2 = as.numeric(((Prozent_1 - Prozent_2)/Prozent_2)*100)) %>% # compute outcome variable: difference of erststimme to zweitstimme per district (%-diff)
  #filter(Pzt_diff_1to2 != is.na(Pzt_diff_1to2)) %>%
  rename(district_num = Gebietsnummer,
         district = Gebietsname,
         party = Gruppenname,
         votes_1 = Anzahl_1,
         votes_2 = Anzahl_2,
         percent_1 = Prozent_1,
         percent_2 = Prozent_2,
         pct_diff_1to2 = Pzt_diff_1to2) # rename variables into English

# check whether there is one result per Wahlkreis and party (for uniqueness to merge)
election_df %>%
  group_by(district_num, party) %>%
  summarize(PartyCand = n()) %>%
  ungroup() %>%
  distinct(PartyCand)

# check classes of columns
sapply(election_df, class)

##### --------------------- import candidacy dataset --------------------- #####
### This dataset includes information on all political candidates for the 
### German parliament in Germany's 2021 federal election.

# path and import
DTA_Cand <- "btw21_kandidaturen_utf8.csv"
candidate_df <- read.csv(paste0(PATH, DTA_Cand), sep = ";", skip = 8, header = TRUE, dec = ",")

# clean data
candidate_df <- candidate_df %>%
  filter(Gebietsart == "Wahlkreis") %>% # filter for candidates directly electable only
  mutate(Gebietsnummer = str_pad(Gebietsnummer, 3, side = "left", pad = "0")) %>% # standardize notation for Wahlkreis numbers
  mutate(Geschlecht = if_else(Geschlecht == "m", 1, 0), # change encoding of gender variable (Male == 1)
         VorpGewaehlt = if_else(VorpGewaehlt == "X", 1, 0)) %>% # change encoding of incumbent variable
  select(Nachname, Vornamen, Titel, Geschlecht, Geburtsjahr, Geburtsort, PLZ, Wohnort, 
         WohnortLandAbk, Staatsangehörigkeit, Beruf, Berufsschluessel, Gebietsnummer, 
         Gruppenname, GruppennameLang, VerknKennzeichen, VerknGebietsname, VerknGruppenname, 
         VerknListenplatz, VorpGewaehlt) %>% # select important variables
  rename(lastname = Nachname,
         firstname = Vornamen,
         sex = Geschlecht,
         birth_year = Geburtsjahr,
         birth_place = Geburtsort,
         home_place = Wohnort,
         home_state = WohnortLandAbk,
         nationality = Staatsangehörigkeit,
         occupation = Beruf,
         job_category = Berufsschluessel,
         district_num = Gebietsnummer,
         party = Gruppenname,
         party_full = GruppennameLang,
         party_list = VerknKennzeichen,
         list_district = VerknGebietsname,
         list_party = VerknGruppenname,
         list_spot = VerknListenplatz,
         incumbent = VorpGewaehlt,
         title = Titel) %>% # rename variables into English
  filter(party=="AfD" | party=="CDU" | party=="CSU" | party=="DIE LINKE" | 
           party=="FDP" | party=="GRÜNE" | party=="SPD") %>% # filter for the major parties, as we only have twitter accounts from those parties
  mutate(title = if_else(title=="", as.numeric(0), as.numeric(1))) # change title variable into binary dummy

# check whether there is one candidate per district and party (for uniqueness to merge)
candidate_df %>%
  group_by(district_num, party) %>%
  summarize(PartyCand = n()) %>%
  ungroup() %>%
  distinct(PartyCand)

# check classes of columns
sapply(candidate_df, class) 

##### --------------------- import structural dataset -------------------- #####
### This dataset includes all structural data per political district.

# path and import
DTA_Struc <- "btw21_strukturdaten.csv"
structural_df <- read.csv(paste0(PATH, DTA_Struc), sep = ";", skip = 8, header = TRUE) %>%
  select(-`Fußnoten`)

# check classes - numerics are in character mode as German thousand and decimal separator is different
sapply(structural_df, class)

# clean data
structural_df <- structural_df %>%
  mutate(Wahlkreis.Nr. = str_pad(Wahlkreis.Nr., 3, side = "left", pad = "0")) %>% # Change encoding of district number variable, to match that of other datasets
  mutate(across(c(4:ncol(structural_df)), gsub, pattern = ".", replacement = "", fixed = TRUE)) %>% # delete german thousand seperator
  mutate(across(c(4:ncol(structural_df)), gsub, pattern = ",", replacement = ".", fixed = TRUE)) %>% # replace german decimal seperator with "."
  mutate(across(c(4:ncol(structural_df)), as.numeric)) %>% # transform columns to numeric
  rename(state = Land,
         district_num = Wahlkreis.Nr.,
         district = Wahlkreis.Name,
         municipalities = Gemeinden.am.31.12.2019..Anzahl.,
         district_size_km2 = Fläche.am.31.12.2019..km..,
         district_pop = Bevölkerung.am.31.12.2019...Insgesamt..in.1000.,
         district_pop_germans = Bevölkerung.am.31.12.2019...Deutsche..in.1000.,
         district_pop_foreign = Bevölkerung.am.31.12.2019...Ausländer..innen....,
         district_pop_density = Bevölkerungsdichte.am.31.12.2019..EW.je.km..,
         district_pop_chg_birthrate_per1000 = Zu......bzw..Abnahme.....der.Bevölkerung.2019...Geburtensaldo..je.1000.EW.,
         district_pop_chg_migration_per1000 = Zu......bzw..Abnahme.....der.Bevölkerung.2019...Wanderungssaldo..je.1000.EW.,
         district_age_u18 = Alter.von.....bis.....Jahren.am.31.12.2019...unter.18....,
         district_age_18_24 = Alter.von.....bis.....Jahren.am.31.12.2019...18.24....,
         district_age_25_34 = Alter.von.....bis.....Jahren.am.31.12.2019...25.34....,
         district_age_35_59 = Alter.von.....bis.....Jahren.am.31.12.2019...35.59....,
         district_age_60_74 = Alter.von.....bis.....Jahren.am.31.12.2019...60.74....,
         district_age_75over = Alter.von.....bis.....Jahren.am.31.12.2019...75.und.mehr....,
         district_landusage_infrastructure = Bodenfläche.nach.Art.der.tatsächlichen.Nutzung.am.31.12.2019...Siedlung.und.Verkehr....,
         district_landusage_nature = Bodenfläche.nach.Art.der.tatsächlichen.Nutzung.am.31.12.2019...Vegetation.und.Gewässer....,
         district_amnt_homes_per1000 = Fertiggestellte.Wohnungen.2019..je.1000.EW.,
         district_stock_homes_per1000 = Bestand.an.Wohnungen.am.31.12.2019...insgesamt..je.1000.EW.,
         district_avg_homesize = Wohnfläche.am.31.12.2019..je.Wohnung.,
         district_avg_homesize_perPerson = Wohnfläche.am.31.12.2019..je.EW.,
         district_amnt_cars_per1000 = PKW.Bestand.am.01.01.2020...PKW.insgesamt..je.1000.EW.,
         district_amnt_e_cars_per1000 = PKW.Bestand.am.01.01.2020...PKW.mit.Elektro..oder.Hybrid.Antrieb....,
         district_amnt_firms_per1000 = Unternehmensregister.2018...Unternehmen.insgesamt..je.1000.EW.,
         district_amnt_artisan_firm_per1000 = Unternehmensregister.2018...Handwerksunternehmen..je.1000.EW.,
         district_educ_vocational_school = Schulabgänger..innen.beruflicher.Schulen.2019,
         district_educ_general_total_grads_per1000 = Schulabgänger..innen.allgemeinbildender.Schulen.2019...insgesamt.ohne.Externe..je.1000.EW.,
         district_educ_grads_less_hauptschule = Schulabgänger..innen.allgemeinbildender.Schulen.2019...ohne.Hauptschulabschluss....,
         district_educ_grads_hauptschule = Schulabgänger..innen.allgemeinbildender.Schulen.2019...mit.Hauptschulabschluss....,
         district_educ_grads_mittlerereife = Schulabgänger..innen.allgemeinbildender.Schulen.2019...mit.mittlerem.Schulabschluss....,
         district_educ_grads_high_school = Schulabgänger..innen.allgemeinblldender.Schulen.2019...mit.allgemeiner.und.Fachhochschulreife....,
         district_care_rate_child_u3 = Kindertagesbetreuung.am.01.03.2020...Betreute.Kinder.unter.3.Jahre..Betreuungsquote.,
         district_care_rate_child_3_u6 = Kindertagesbetreuung.am.01.03.2020...Betreute.Kinder.3.bis.unter.6.Jahre..Betreuungsquote.,
         district_avg_income = Verfügbares.Einkommen.der.privaten.Haushalte.2018..EUR.je.EW.,
         district_gdp_2018 = Bruttoinlandsprodukt.2018..EUR.je.EW.,
         district_total_workers_per1000 = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...insgesamt..je.1000.EW.,
         district_workers_agriculture = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...Land..und.Forstwirtschaft..Fischerei....,
         district_workers_manufacture = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...Produzierendes.Gewerbe....,
         district_workers_commerce = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...Handel..Gastgewerbe..Verkehr....,
         district_workers_publicprivate_service = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...Öffentliche.und.private.Dienstleister....,
         district_workers_otherservice = Sozialversicherungspflichtig.Beschäftigte.am.30.06.2020...Übrige.Dienstleister.und..ohne.Angabe.....,
         district_support_recipients_total_per1000 = Empfänger..innen.von.Leistungen.nach.SGB.II..Oktober.2020....insgesamt..je.1000.EW.,
         district_support_recipients_nonemployable = Empfänger..innen.von.Leistungen.nach.SGB.II..Oktober.2020....nicht.erwerbsfähige.Hilfebedürftige....,
         district_support_foreign = Empfänger..innen.von.Leistungen.nach.SGB.II..Oktober.2020....Ausländer..innen....,
         district_unemprate_total = Arbeitslosenquote.Februar.2021...insgesamt,
         district_unemprate_male = Arbeitslosenquote.Februar.2021...Männer,
         district_unemprate_female = Arbeitslosenquote.Februar.2021...Frauen,
         district_unemprate_age_15_24 = Arbeitslosenquote.Februar.2021...15.bis.24.Jahre,
         district_unemprate_age_55_64 = Arbeitslosenquote.Februar.2021...55.bis.64.Jahre) %>% # change column name to English
  mutate(district_age_60over = district_age_60_74 + district_age_75over) 

# check classes again
sapply(structural_df, class)

cor(structural_df$district_avg_income, structural_df$district_unemprate_total)

##### --------------------- import twitter ID dataset -------------------- #####
### This data contains a list of all twitter accounts with their respective IDs
### alongside other variables and demographics

# path and import
DTA_twitter <- "Twitter IDs/twitter_ids.csv"
twitterid_df <- read.csv(paste0(PATH, DTA_twitter), sep = ";", header = TRUE, 
                         colClasses = c(rep("character", 12), rep("integer", 5), rep("character", 5)))

# clean data
twitterid_df <- twitterid_df %>%
  select(id, lastname, firstname, gender, state, party, district_number, incumbent, 
         list_place, isListed, isDC, screen_name1, screen_name2, user_id1, user_id2,
         change_v2) %>% # select variables of interest
  mutate(gender = if_else(gender == "m", 1, 0),
         district_number = str_pad(district_number, 3, side = "left", pad = "0"),
         party = if_else(party == "GR\xdcNE", "GRÜNE", party)) %>% # change encoding of gender, district_num and party variable
  rename(district_num = district_number) %>% # change name of variable to match other datasets
  mutate(Twitter_Acc = if_else(user_id1!="", 1, 0)) %>% # add variable indicating candidates that have a Twitter account
  filter(isDC == 1) # filter for direct candidates only

##### --------------------------- merge datasets ------------------------- #####
# merge & filter for candidates that are a direct candidate in an election district 
master_df <- candidate_df %>%
  left_join(twitterid_df, by = c("district_num", "party")) %>%
  left_join(election_df, by = c("district_num", "party")) %>%
  left_join(structural_df, by = "district_num") %>%
  mutate(Twitter_Acc = if_else(is.na(Twitter_Acc), 0, as.numeric(Twitter_Acc)))

# Check master dataset whether duplicate variables are identical (additional check whether data is correctly merged)
check <- master_df %>% 
  select(ends_with(c(".y", ".x")))

# Check variables firstname and lastname
check %>%
  select(lastname.x, firstname.x, lastname.y, firstname.y) %>%
  filter(!(lastname.x == lastname.y) | !(firstname.x == firstname.y)) %>%
  view()

### The inspection above showed that both the variables lastname.y and firstname.y 
### cannot show the German "umlaute" correctly, hence we delete these variables 
### and solely keep lastname.x and firstname.x. 

# Check variable incumbent
check %>%
  select(incumbent.x, incumbent.y) %>%
  filter(!(incumbent.x == incumbent.y)) %>%
  view()

### No observations seem to have a mismatch regarding these variables, 
### hence it is not important which one we keep

# Check variable state
check %>%
  select(state.x, state.y) %>%
  view()

### Both variables are identical, but one is the abbreviation of the state and 
### the other is its full name, we keep both in case and rename these accordingly

# Check variable district
check %>%
  select(district.x, district.y) %>%
  filter(!(district.x == district.y)) %>%
  view()

election_df %>%
  filter(district == "Paderborn – Gütersloh III" | district == "Höxter – Lippe II") %>%
  select(district_num, district)

structural_df %>%
  filter(district == "Paderborn" | district == "Höxter – Gütersloh III – Lippe II") %>%
  select(district_num, district)

### The two districts which seem to have an inconsistency are districts 136 and 137. 
### Further google searches led to the results, that the official website from the 
### "Bundeswahlleiter" (https://www.bundeswahlleiter.de/bundestagswahlen/2021/wahlkreiseinteilung/bund-99/land-5/wahlkreis-136.html) 
### defines the election districts according to the variable "district.y". 
### Hence we will keep this variable as it follows the official district organisation.

# Remove and rename the above variables from the master dataset
master_df <- master_df %>%
  select(-`firstname.y`, -`lastname.y`, -`incumbent.x`, -`district.x`) %>%
  rename(firstname = firstname.x,
         lastname = lastname.x, 
         incumbent = incumbent.y,
         state_short = state.x,
         state = state.y,
         district = district.y)

# Check for candidates with missing information
check <- master_df %>%
  filter(is.na(gender) | is.na(birth_year) | is.na(firstname) | is.na(state_short))

### There is only one candidate which has not been perfectly matched between the
### candidate_df dataframe and twitterid_df dataframe. Hence I will match the missing
### information manually

# Apply correction as discussed above
master_df <- master_df %>%
  mutate(gender = if_else((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029"), as.numeric(sex), as.numeric(gender)),
         state_short = if_else((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029"), as.character("HB"), as.character(state_short)),
         incumbent = if_else((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029"), as.numeric(0), as.numeric(incumbent)),
         isListed = if_else((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029"), as.numeric(1), as.numeric(isListed)),
         list_place = if_else(((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029")), as.numeric(1), as.numeric(list_place)),
         isDC = if_else((firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029"), as.numeric(1), as.numeric(isDC)))

### All information which has been manually entered above was taken from the following sources
### as well as the data from the candidacy data set:

### Der Bundeswahlleiter. (2021, Aug 5). Bundestagswahl 2021: Bundeswahlausschuss hat über
### Beschwerden entschieden. Der Bundeswahlleiter. https://www.bundeswahlleiter.de/info/presse/mitteilungen/bundestagswahl-2021/22_21_2bwa-entscheidung.html.
### Accessed on: 23.06.2022.

### buten un binnen (2021, Jun 26). Politiker aus Cuxhaven führt Bremer AfD in die Bundestagswahl.
### buten un binnen. https://www.butenunbinnen.de/nachrichten/cuxhavener-afd-bundestagswahl-liste-bremen-100.html
### Accessed on: 23.06.2022.

# Check whether updates have been incorporated correctly
check <- master_df %>%
  filter(firstname=="Hans Olaf" & lastname=="Kappelt" & district_num=="029")

### Further data manipulation
# import job category key to summarise occupation of candidates
job_key <- read.csv(paste0(PATH, "btw21_gewaehlte_utf8/kldb2010_berufsschluessel.csv"), 
                    sep = ";", skip = 8, header = TRUE)

# select and rename relevant variables
job_key <- job_key %>%
  rename(job_key = Berufsbereich_Schluessel,
         job_key_cat = Berufsbereich_Bezeichnung,
         job_category = Berufshauptgruppe_Berufsschluessel) %>%
  select(job_key, job_key_cat, job_category)

# merge with main dataframe
master_df <- master_df %>%
  left_join(job_key, by = "job_category")

# import dataframe for politicians who won a seat in the current election
winner_df <- read.csv(paste0(PATH, "btw21_gewaehlte_utf8/btw21_gewaehlte-fortschreibung_utf8.csv"),
                      sep = ";", skip = 8, header = TRUE)

# clean data of winners
winner_df <- winner_df %>%
  filter(Gebietsart=="Wahlkreis") %>% # select only candidates that won a seat by direct vote
  select(Nachname, Vornamen, Gebietsnummer, Prozent) %>%
  rename(lastname = Nachname,
         firstname = Vornamen,
         district_num = Gebietsnummer,
         percent_win = Prozent) %>%
  mutate(winner = 1, # create new variable to indicate winner
         district_num = str_pad(district_num, 3, side = "left", pad = "0")) # change encoding of district_num to match main dataframe

# merge with main dataframe
master_df <- master_df %>%
  left_join(winner_df, by = c("firstname", "lastname", "district_num")) %>%
  mutate(winner = if_else(is.na(winner), as.numeric(0), as.numeric(winner)))

# manually encode a variable to represent the top candidates for each party
master_df <- master_df %>%
  mutate(Top_candidate = if_else(((lastname == "Habeck" & firstname == "Robert") | 
                                    (lastname == "Baerbock" & firstname == "Annalena Charlotte Alma") | 
                                    (lastname == "Scholz" & firstname == "Olaf") | 
                                    (lastname == "Laschet" & firstname == "Armin") | 
                                    (lastname == "Lindner" & firstname == "Christian Wolfgang") | 
                                    (lastname == "Bartsch" & firstname == "Dietmar Gerhard") | 
                                    (lastname == "Wißler" & firstname == "Janine Natalie") |
                                    (lastname == "Chrupalla" & firstname == "Tino") |
                                    (lastname == "Weidel" & firstname == "Alice Elisabeth")), 1, 0))

# check for consistency of new variable 
# only have 8 top candidates in dataframe, as "Armin Laschet" was not a direct candidate for parliament
describe(master_df$Top_candidate)

### Source for the above list of top candidates:
### Mendelson, B. (2021, Sep. 27). Die Kanzlerkandidaten zur Bundestagswahl 2021 -
### wer folgt auf Angela Merkel? Handelsblatt. Available at: https://www.handelsblatt.com/politik/deutschland/baerbock-laschet-scholz-die-kanzlerkandidaten-zur-bundestagswahl-2021-wer-folgt-auf-angela-merkel-/27069870.html
### [Accessed on: 04.07.2022]. 

# change NAs of "list place" variable to 99, such that we avoid deleting over 400 observations when we include this in regressions
# inspect list_place variable first
summary(master_df$list_place) # max is 79, hence we choose 99 to indicate that candidates have no list place

master_df <- master_df %>%
  mutate(list_place = if_else(is.na(list_place), 99, as.numeric(list_place)))

# include an East-Germany dummy variable based on district location
# East-Berlin district numbers are 076, 083, 084, 085, & 086.
master_df <- master_df %>%
  mutate(East_Germany = if_else((state_short == "MV" | state_short == "BB" |
                                   state_short == "SN" | state_short == "TH" |
                                   state_short == "ST" | district_num == "076" |
                                   district_num == "083" | district_num == "084" |
                                   district_num == "085" | district_num == "086"),
                                as.numeric(1), as.numeric(0)))

### Source for East-Berlin district locations taken from official site of State of Berlin:
### NA. (2021). Wahlgebietseinteilung. Landeswahlleiterin für Berlin. Accessed at: 
### https://www.berlin.de/wahlen/wahlen/wahlen-2021/wahlgebietseinteilung/artikel.966836.php#wahlkreiseinteilung
### Accessed on: 05.07.2022.

summary(master_df$East_Germany)

# check correct encoding of "isListed" variable
sum(is.na(master_df$list_party))
sum(is.na(master_df$list_district))
sum(master_df$isListed == 0) # all variables have same num. of candidates that are not listed, hence correct encoding

test <- candidate_df %>%
  left_join(twitterid_df, by = c("district_num", "party"))

test %>%
  filter(is.na(Twitter_Acc)) %>%
  view()

##### --------------------------- export datasets ------------------------ #####

# Save data as CSV file - master_all_cand is the master dataframe including all direct candidates, irrespective of having a twitter account
write_csv(master_df, "Clean Data/master_all_cand.csv")

# Further filter for candidates that only have a twitter account
master_twitter_only <- master_df %>%
  filter(Twitter_Acc == 1)

# Save data as CSV file - master_twitter_cand is master dataframe which only includes candidates with Twitter accounts
write_csv(master_twitter_only, "Clean Data/master_twitter_cand.csv")

### Election dataset has few less observations that candidacy dataset, as election data
### combines smaller parties into a category of "others", thus removing the results of
### direct candidates of much smaller parties, which are still listed in the candidacy
### dataset. Twitter ID dataset only includes candidates of largest parties: SPD, CDU, 
### FDP, GRÜNE, DIE LINKE and AfD. 

##### --------------------------- twitter ID Data ------------------------ #####
# # # # # Prepare a seperate dataset for Twitter scraping # # # # #
twitter_ID1 <- master_twitter_only %>%
  select(lastname, firstname, screen_name1, user_id1) %>%
  rename(screenname = screen_name1,
         user_id = user_id1)

twitter_ID2 <- master_twitter_only %>%
  select(lastname, firstname, screen_name2, user_id2) %>%
  filter(user_id2 != "") %>% # remove all "NA" values, encoded as empty strings 
  rename(screenname = screen_name2, 
         user_id = user_id2)

# Create final dataframe with all twitter IDs and screen names (duplicated names 
# as candidates sometimes have multiple accounts - but we scrape all)
twitter_api <- twitter_ID1 %>%
  rbind(twitter_ID2)

# Save data as CSV file for use in Twitter API in Python
write_csv(twitter_api, "Clean Data/Twitter/twitter_ids.csv")

### We have 1201 candidates from the core parties that are simultaneously direct 
### candidates in a district and also have a twitter ID. In total we have 1215 
### twitter IDs which we will proceed to scrape using the Twitter API. 

##### -------------------------- shapefile dataset ----------------------- #####
# # # # # Cleaning shapefile of election districts # # # # #
# import specific libraries for GIS data
library(plyr)
library(sp)
library(rgdal)

# importing shapefile
district_shp <- readOGR(paste0(PATH, "Geography/btw21_district_shp/"))

# save rownames as an identifier for each district
district_shp@data$id = rownames(district_shp@data)

# save polygon shapes as a normal dataframe
shp_df <- ggplot2::fortify(district_shp)

# combine the polygons in dataframe with district names by unique identifier
shapefiles <- join(shp_df, district_shp@data, by="id")

# save data
write_csv(shapefiles, "Clean Data/District_shapefiles.csv")

#####------------------- gles 2021 election study data ------------------- #####

# import merged GLES data
df_gles <- read_dta("Raw Data/GLES 2021/ZA7704_v1-0-0_merge.dta")

sum(df_gles$twitter_acc == "NA") # 187 candidates from 735 do not have a Twitter Account
sum(df_gles$sum_posts == "NA") # 512 candidates from 735 were not active on Twitter (no scraping of their tweets)

# checking the labels on various variables
unique(df_gles$bundesland)
unique(df_gles$partei)
unique(df_gles$kandidaturtyp)
unique(df_gles$e8)

# clean GLES dataset
df_gles <- df_gles %>%
  mutate(Incumbent = if_else(a2a==3, 1, 0), # create an incumbent variable based on their success during last federal election
         IsListed = if_else((kandidaturtyp==1 | kandidaturtyp==3), 1, 0)) %>% # create dummy variable whether candidate is listed in state party list for election
  select(-(1:27)) %>% # remove all structural variables of the study
  select(1:11, "a1", starts_with("a2"), "a4", "b1a", "b1b", "b2", starts_with("b3"),
         "b4", "b5", starts_with("b6"), starts_with("b7"), starts_with("b8"), "b9", 
         starts_with("b17"), starts_with("e8"), starts_with("e9"), starts_with("e10"), 
         "e12", "twitter_acc":"avg_posts_time_group4", Incumbent, IsListed, east_germany) %>% #only keep variables of interest
  mutate_all(list(~na_if_in(., c(-71, -73, -91, -92, -93, -94, -95, -97, -98, -99)))) %>% # fix numeric NA values from survey
  mutate_all(list(~na_if_in(., c("-71", "-73","-91", "-92", "-93", "-94", "-95", 
                                 "-97", "-98", "-99", "-97 trifft nicht zu")))) %>% # fix string NA values from survey
  mutate(wknr = str_pad(as.numeric(wknr), 3, side = "left", pad = "0")) %>% # change encoding of election district numbers
  rename(district_num = wknr) %>% # rename variables to English
  left_join(structural_df, by = "district_num") %>% # merge data with structural data from election districts
  mutate(percent_election_erststimme = as.numeric(percent_election_erststimme),
         percent_election_zweitstimme = as.numeric(percent_election_zweitstimme)) # transform strings into numeric variables

# Clean highest educational achievement - add bachelor/master/PhD to list
df_gles <- df_gles %>%
  mutate(e8 = if_else(e9i == 1, 8, # 8 = bachelor degree
                      if_else(e9j == 1, 9, # 9 = master degree
                              if_else(e9k == 1, 10, as.numeric(e8))))) # 10 = doctor

# check consistency of variables created above (count observations for unique values in variables)
table(df_gles$e8)
table(df_gles$IsListed)
table(df_gles$kandidaturtyp)

summary(df_gles$percent_election_erststimme)
summary(df_gles$percent_election_zweitstimme)

# Change numerical encoding of party to string names of each party
# create tribble to transform party from numeric to string name (taken from labels of variable)
party_table <- tribble(
  ~Number, ~Name,
  1, "CDU",
  2, "CDU",
  3, "CSU",
  4, "SPD",
  5, "FDP",
  6, "GRÜNE",
  7, "DIE LINKE",
  322, "AfD"
)

# Add new variable "party" with names rather than numbers
df_gles <- df_gles %>%
  mutate(party = as.character(mgsub(as.numeric(partei), party_table$Number, party_table$Name)))

# check consistency of new party variable
table(df_gles$partei)
table(df_gles$party)
table(df_gles$e8)

# create IV variable from response b8 (8 questions on politicians views on social media)
df_gles <- df_gles %>%
  filter(kandidaturtyp == 2 | kandidaturtyp == 3) %>% # filter only for direct candidates
  mutate(b8d = if_else(b8d==1, 5,
                       if_else(b8d==2, 4,
                               if_else(b8d==4, 2,
                                       if_else(b8d==5, 1, as.numeric(b8d))))),
         b8g = if_else(b8g==1, 5,
                       if_else(b8g==2, 4,
                               if_else(b8g==4, 2,
                                       if_else(b8g==5, 1, as.numeric(b8g)))))) %>%
  mutate(SM_preference = as.numeric(b8a + b8b + b8c + b8d + b8e + b8f + b8g + b8h)/8) # create preference variable

# create the constituency-level age distribution variable of 60+ population AND have to create
# dummy variables for party otherwise regressions won't run properly
df_gles <- df_gles %>%
  mutate(district_age_60over = district_age_60_74 + district_age_75over) %>%
  mutate(partyCDU = if_else(party=="CDU", 1, 0),
         partyCSU = if_else(party=="CSU", 1, 0),
         partyDIELINKE = if_else(party=="DIE LINKE", 1, 0),
         partyFDP = if_else(party=="FDP", 1, 0),
         partyGRUENE = if_else(party=="GRÜNE", 1, 0),
         partySPD = if_else(party=="SPD", 1, 0)) %>%
  mutate(Educ = if_else((e8==5 | e8==4), "High School",
                        if_else((e8==8 | e8==9 | e8==10), "University Degree", "Other"))) %>%
  mutate(Twitter_GLES = if_else(b7t==0, 0, 
                                if_else(is.na(b7t), as.numeric(NA), 1))) # Create a GLES Twitter variable

# Check how similar the GESIS twitter and GLES twitter variables are 
# Note: encoded differently, hence should not necessarily be identical!
table(df_gles$b7t)
table(df_gles$Twitter_GLES)
table(df_gles$Twitter_GLES==df_gles$twitter_acc)

# Check consistency of other variables (education and party encodings!)
table(df_gles$Educ)
table(df_gles$party)
table(df_gles$partyCDU)
table(df_gles$partyCSU)
table(df_gles$partyDIELINKE)
table(df_gles$partyFDP)
table(df_gles$partyGRUENE)
table(df_gles$partySPD)

# create social media variables (Facebook, Youtube and All of Social Media)
df_gles <- df_gles %>%
  mutate(Facebook_acc = if_else(b7s == 0, 0, 
                                if_else(is.na(b7s), as.numeric(NA), 1)),
         Youtube_acc = if_else(b7u == 0, 0,
                               if_else(is.na(b7u), as.numeric(NA), 1)),
         Social_media_OR = if_else((b7s == 0 & b7t == 0 & b7u == 0), 0,
                                if_else((is.na(b7s) & is.na(b7t) & is.na(b7u)), as.numeric(NA), 1))) %>%
  mutate(FB_TW_acc = if_else(Facebook_acc == 1 & Twitter_GLES == 1, 1, 0)) %>% # variable for candidates who own both Facebook and Twitter
  mutate(Social_media_AND = if_else(Facebook_acc==1 & Twitter_GLES==1 & Youtube_acc==1, 1, 0))

# check consistency of above variables
table(df_gles$Facebook_acc)
table(df_gles$Youtube_acc)
table(df_gles$FB_TW_acc)
table(df_gles$Social_media_OR)
table(df_gles$b7s)
table(df_gles$b7t)
table(df_gles$b7u)

# Check how many candidates have only or both FB and Twitter
df_gles <- df_gles %>%
  mutate(SM_Check = if_else(Twitter_GLES == 1 & Facebook_acc == 1 & Youtube_acc == 1, "All",
                            if_else(Twitter_GLES == 1 & Facebook_acc == 0 & Youtube_acc == 0, "Twitter only",
                                    if_else(Twitter_GLES == 0 & Facebook_acc == 1 & Youtube_acc == 0, "Facebook only",
                                            if_else(Twitter_GLES == 0 & Facebook_acc == 0 & Youtube_acc == 1, "Youtube only",
                                                    if_else(Twitter_GLES == 1 & Facebook_acc == 1 & Youtube_acc == 0, "FB and Twitter",
                                                            if_else(Twitter_GLES == 0 & Facebook_acc == 1 & Youtube_acc == 1, "FB and Youtube",
                                                                    if_else(Twitter_GLES == 1 & Facebook_acc == 0 & Youtube_acc == 1, "Twitter and Youtube", 
                                                                            if_else(Twitter_GLES == 0 & Facebook_acc == 0 & Youtube_acc == 0, "None", "Other")))))))))


# export
write.csv(df_gles, "Clean Data/GLES_2021.csv", row.names = FALSE)

