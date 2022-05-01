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
  select(Gebietsnummer, Gebietsname, Gruppenname, Stimme, Anzahl, 
         Prozent) %>% # selecting specific variables (removing election results from past election)
  pivot_wider(names_from = Stimme, values_from = c(Anzahl, Prozent)) %>% # Create wide version of election results
  mutate(PztPkt_diff_1to2 = as.numeric((Prozent_1 - Prozent_2))) %>% # compute outcome variable: difference of erststimme to zweitstimme per wahlkreis
  filter(PztPkt_diff_1to2 != is.na(PztPkt_diff_1to2)) %>%
  rename(district_num = Gebietsnummer,
         district = Gebietsname,
         party = Gruppenname,
         votes_1 = Anzahl_1,
         votes_2 = Anzahl_2,
         percent_1 = Prozent_1,
         percent_2 = Prozent_2,
         pctpoint_diff_1to2 = PztPkt_diff_1to2) # rename variables into English

# check whether there is one result per Wahlkreis and party (for uniqueness to merge)
election_df %>%
  group_by(district_num, party) %>%
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
         incumbent = VorpGewaehlt) # rename variables into English

# check whether there is one candidate per district and party (for uniqueness to merge)
candidate_df %>%
  group_by(district_num, party) %>%
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
         district_unemprate_age_55_64 = Arbeitslosenquote.Februar.2021...55.bis.64.Jahre) # change column name to English

# check classes again
sapply(structural_df, class)

##### --------------------- import twitter ID dataset -------------------- #####
# path and import
DTA_twitter <- "Twitter IDs/twitter_ids.csv"
twitterid_df <- read.csv(paste0(PATH, DTA_twitter), sep = ";", header = TRUE)

# clean data
twitterid_df <- twitterid_df %>%
  select(id, lastname, firstname, gender, state, party, district_number, incumbent, 
         list_place, isListed, isDC, screen_name1, screen_name2, user_id1, user_id2,
         change_v2) %>% # select variables of interest
  mutate(gender = if_else(gender == "m", 1, 0),
         district_number = str_pad(district_number, 3, side = "left", pad = "0"),
         party = if_else(party == "GR\xdcNE", "GRÜNE", party)) %>% # change encoding of gender, district_num and party variable
  rename(district_num = district_number) # change name of variable to match other datasets

##### --------------------------- merge datasets ------------------------- #####
# merge
master_df <- twitterid_df %>%
  filter(isDC == 1 & !is.na(user_id1)) %>% # filter for candidates that are a direct candidate in an election district AND have a twitter ID that we can scrape (relevant for analysis)
  left_join(election_df, by = c("district_num", "party")) %>%
  left_join(candidate_df, by = c("district_num", "party")) %>%
  left_join(structural_df, by = "district_num")

# Check master dataset whether duplicate variables are identical (additional check whether data is correctly merged)
check <- master_df %>% 
  select(ends_with(c(".y", ".x")))

# Check variables firstname and lastname
check %>%
  select(lastname.x, firstname.x, lastname.y, firstname.y) %>%
  filter(!(lastname.x == lastname.y) | !(firstname.x == firstname.y)) %>%
  view()

### The inspection above showed that both the variables lastname.x and firstname.x cannot show the German "umlaute" correctly, 
### hence we delete these variables and solely keep lastname.y and firstname.y. 

# Check variable incumbent
check %>%
  select(incumbent.x, incumbent.y) %>%
  filter(!(incumbent.x == incumbent.y)) %>%
  view()

### No observations seem to have a mismatch regarding these variables, hence it is not important which one we keep

# Check variable state
check %>%
  select(state.x, state.y) %>%
  view()

### Both variables are identical, but one is the abbreviation of the state and the other is its full name, we keep both in case and rename these accordingly

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

### The two districts which seem to have an inconsistency are districts 136 and 137. Further google searches led to the results,
### that the official website from the "Bundeswahlleiter" (https://www.bundeswahlleiter.de/bundestagswahlen/2021/wahlkreiseinteilung/bund-99/land-5/wahlkreis-136.html) 
### defines the election districts according to the variable district.y. Hence we will keep this variable as it follows the official district organisation.

# Remove and rename the above variables from the master dataset
master_df <- master_df %>%
  select(-`firstname.x`, -`lastname.x`, -`incumbent.x`, -`district.x`) %>%
  rename(firstname = firstname.y,
         lastname = lastname.y, 
         incumbent = incumbent.y,
         state_short = state.x,
         state = state.y,
         district = district.y)

### Election dataset has few less observations that candidacy dataset, as election data
### combines smaller parties into a category of "others", thus removing the results of
### direct candidates of much smaller parties, which are still listed in the candidacy
### dataset. Twitter ID dataset only includes candidates of largest parties: SPD, CDU, 
### FDP, GRÜNE, DIE LINKE and AfD. 

# Prepare a seperate dataset for Twitter scraping
twitter_ID1 <- master_df %>%
  select(lastname, firstname, screen_name1, user_id1) %>%
  rename(screenname = screen_name1,
         user_id = user_id1)

twitter_ID2 <- master_df %>%
  select(lastname, firstname, screen_name2, user_id2) %>%
  drop_na() %>%
  rename(screenname = screen_name2, 
         user_id = user_id2)

# Create final dataframe with all twitter IDs and screen names (duplicated names as candidates sometimes have multiple accounts - but we scrape all)
twitter_api <- twitter_ID1 %>%
  rbind(twitter_ID2)

### We have 1201 candidates from the core parties that are simultaneously direct candidates in a district and also have
### a twitter ID. In total we have 1215 twitter IDs which we will proceed to scrape using the Twitter API. 





