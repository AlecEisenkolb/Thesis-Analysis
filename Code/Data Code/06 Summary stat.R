# Summary Statistics and Tables
# Date: 06.07.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)
library(stargazer)

# import data
df_master <- read_csv("Clean Data/master_all_cand.csv")

# compute variable that shows difference in posting frequency between "hotphase" and july/august
# As time group 1 = july and time group 2 = august, and they both have equal number of days in a month,
# we can simply divide the sum of their post frequency by 2 and then subtract from post frequency in "hotphase".
df_master <- df_master %>%
  mutate(Delta_AvgPost = (Avg_posts_time_group3 - (Avg_posts_time_group1 + Avg_posts_time_group2)/2)) %>%
  mutate(AfD = if_else(party=="AfD", 1, 0),
         CDU = if_else(party=="CDU", 1, 0),
         CSU = if_else(party=="CSU", 1, 0),
         Linke = if_else(party=="DIE LINKE", 1, 0),
         FDP = if_else(party=="FDP", 1, 0),
         Gruene = if_else(party=="GRÜNE", 1, 0),
         SPD = if_else(party=="SPD", 1, 0),
         Job_others = if_else(job_key==0, 1, 0),
         Job_agriculture = if_else(job_key==1, 1, 0),
         Job_manufacture = if_else(job_key==2, 1, 0),
         Job_construction = if_else(job_key==3, 1, 0),
         Job_naturalscience = if_else(job_key==4, 1, 0),
         Job_logistics = if_else(job_key==5, 1, 0),
         Job_retail = if_else(job_key==6, 1, 0),
         Job_proffesionalservice = if_else(job_key==7, 1, 0),
         Job_socialservice = if_else(job_key==8, 1, 0),
         Job_academia_media = if_else(job_key==9, 1, 0))

colnames(df_master)
table(df_master$job_key)

# compute summary statistics for control variables (full table) #####
stargazer::stargazer(as.data.frame(df_master[, c("gender", "birth_year", "incumbent", 
                                                 "isListed", "Top_candidate",
                                                 "Job_others", "Job_agriculture", "Job_manufacture",
                                                 "Job_construction", "Job_naturalscience", "Job_logistics",
                                                 "Job_retail", "Job_proffesionalservice", "Job_socialservice",
                                                 "Job_academia_media",
                                                 "Prediction", "AfD", "CDU", "CSU", "Linke", "FDP", 
                                                 "Gruene", "SPD",
                                                 "district_avg_income", "district_unemprate_total",
                                                 "district_pop_foreign", "district_age_60over", "East_Germany")]),
                     summary=TRUE, type="latex",
                     title="Summary Statistics of Key Variables")

# compute summary statistics for control variables (selected variables in main text) #####
stargazer::stargazer(as.data.frame(df_master[, c("gender", "birth_year", "incumbent", 
                                                 "isListed", "AfD", "CDU", "CSU", "Linke", "FDP", 
                                                 "Gruene", "SPD",
                                                 "district_avg_income", "district_unemprate_total",
                                                 "district_pop_foreign", "East_Germany")]),
                     summary=TRUE, type="latex",
                     title="Summary Statistics of Key Variables")

# compute summary statistics for independent variables (Twitter account & Twitter activity variables
stargazer::stargazer(as.data.frame(df_master[, c("Twitter_Acc", "Mean_Likes", 
                                                 "Mean_Reply", "Mean_RT",
                                                 "Avg_Weekly_Posts", "Avg_posts_time_group3",
                                                 "Delta_AvgPost")]),
                     summary=TRUE, type="latex",
                     title="Summary Statistics of Independent Variables")

sapply(df_master, class)

# compute table with summary statistics for first and second vote election results
stargazer::stargazer(as.data.frame(df_master[, c("percent_1", "percent_2")]),
                     summary=TRUE, type="latex",
                     title="Summary Statistics of Election Results")

# compute table with summary statistics seperately for constituency-level variables
df_dist <- df_master %>%
  group_by(district_num) %>%
  summarise(mean_income = mean(district_avg_income, na.rm = TRUE),
            mean_foreign = mean(district_pop_foreign, na.rm = TRUE),
            mean_unemp = mean(district_unemprate_total, na.rm = TRUE),
            mean_60over = mean(district_age_60over, na.rm = TRUE)) %>%
  ungroup()

stargazer::stargazer(as.data.frame(df_dist),
                     summary=TRUE, type="latex",
                     title="Summary Statistics of District Variables")

# check difference in means between Twitter and non-Twitter for variables:
# birth year, gender, incumbent, isListed, East-Germany dummy

### check whether variable is normally distributed (Shapiro-Wilk Normality Test) - assumption for t-test!
# birth year / age is not normally distributed, as p-value is lower than critical value of 0.05. 
with(df_master, shapiro.test(birth_year[Twitter_Acc == 1])) # p = 1.299 x 10^(-10)
with(df_master, shapiro.test(birth_year[Twitter_Acc == 0])) # p = 1.261 x 10^(-07)

# compute a new variable in master dataframe to show share of population with ages
# 18 - 35 per election district
df_master <- df_master %>%
  mutate(district_age_young = district_age_18_24 + district_age_25_34)

# define variables for which to compute test
vars <- c("birth_year", "gender", "incumbent", "isListed", "East_Germany",
          "district_avg_income", "district_unemprate_total", "district_pop_foreign")

# potentially other vars to include: "district_age_young", "district_educ_general_total_grads_per1000"
# create empty dataframe table to store p-values in
test_table <- data.frame(matrix(NA, nrow=8, ncol=7))
colnames(test_table) <- c("Variable", "Mean Twitter", "Median Twitter", "Mean No Twitter", "Median No Twitter", "T-Test Unpaired", "Wilcoxon-Test Unpaired")

# set counter
i <- 1
for (var in vars){
  
  # get vector of Twitter account == 1 and variable of interest
  twitter_var <- df_master %>%
    filter(Twitter_Acc==1) %>%
    select(var) %>%
    as.matrix()
  
  # get vector of Twitter account == 0 and variable of interest
  nontwitter_var <- df_master %>%
    filter(Twitter_Acc==0) %>%
    select(var) %>%
    as.matrix()
  
  # compute unpaired t-test object - unpaired implies independent samples and equal variances
  ttest <- t.test(twitter_var,
                  nontwitter_var, 
                  var.equal = TRUE)
  
  # compute unpaired wilcoxon-test object - unpaired implies independent samples and equal variances
  wtest <- wilcox.test(twitter_var,
                       nontwitter_var,
                       paired = FALSE,
                       alternative = "two.sided")
  
  # extract variable name
  test_table[i, 1] <- as.character(var)
  
  # extract mean of twitter
  test_table[i, 2] <- as.numeric(mean(twitter_var))
  
  # extract median of twitter
  test_table[i, 3] <- as.numeric(median(twitter_var))
  
  # extract mean of non-twitter
  test_table[i, 4] <- as.numeric(mean(nontwitter_var))
  
  # extract median of non-twitter
  test_table[i, 5] <- as.numeric(median(nontwitter_var))
  
  # extract t-test p-value
  test_table[i, 6] <- as.numeric(ttest$p.value)
  
  # extract unpaired wilcoxon-test p-value
  test_table[i, 7] <- as.numeric(wtest$p.value)
  
  i <- i + 1
}

# export table
stargazer(test_table, summary=FALSE, type="latex")

### Analyse mean of average weekly postings pre and post election
# paired t-test
t.test(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec1,
       df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec0,
       paired = TRUE) # p-value = 2.2x10^(-16), hence reject H0 and difference in means is not equal to 0

# paired wilcoxon test
wilcox.test(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec1,
            df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec0,
            paired = TRUE,
            alternative = "two.sided") # p-value < 2.2 x 10^(-16), hence reject H0 and difference in means is not equal to 0

# means of before and after election posting average
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec1) # mean pre-election = 9.36 posts per week
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_pre_elec0) # mean post-election = 5.99 posts per week

# Thus overall can conclude that data shows candidates on Twitter were significantly 
# more active before the election than in the two weeks measured after the election.

### Analyse mean of average weekly postings between different phases of election cycle: July, August, hot-phase (September-Election), post-election (Election-October)
# mean of average posting activity per phase
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group1) # July mean = 7.19 posts per week
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group2) # August mean = 9.74 posts per week
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group3) # September-Election mean = 11.48 posts per week
mean(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group4) # Election-October mean = 5.99 posts per week

# paired t-test of hot-phase vs. July
t.test(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group3,
       df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group1,
       paired = TRUE) # p-value < 2.2 x 10^(-16), hence reject H0 and difference is statistically different from 0

# paired t-test of hot-phase vs. August
t.test(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group3,
       df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group2,
       paired = TRUE) # p-value = 1.147 x 10^(-09), hence reject H0 and difference is statistically different from 0

# paired t-test of hot-phase vs. post-election
t.test(df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group3,
       df_master[df_master$Twitter_Acc==1, ]$Avg_posts_time_group4,
       paired = TRUE) # p-value < 2.2 x 10^(-16), hence reject H0 and difference is statistically different from 0

# compute mean of vote outcome by subgroups of Twitter account
df_master %>%
  group_by(Twitter_Acc) %>%
  summarise(Mean_Erststimme = mean(percent_1, na.rm = TRUE),
            Mean_Zweitstimme = mean(percent_2, na.rm = TRUE),
            Mean_Diff = mean(pct_diff_1to2, na.rm = TRUE)) %>%
  view()

# compute correlation between twitter account and candidate's vote outcome
cor(df_master$Twitter_Acc, df_master$percent_1, use = "complete.obs")

# check share of active/non-active Twitter account holders
df_master %>%
  mutate(Twitter_Acc = if_else((Twitter_Acc==1 & Avg_Weekly_Posts==0), "Twitter (non-active)",
                               if_else((Twitter_Acc==1 & Avg_Weekly_Posts!=0), "Twitter (active)", "No Twitter"))) %>%
  group_by(Twitter_Acc) %>%
  summarise(quant = n()) %>%
  mutate(share = quant/sum(quant)) %>%
  view()

# check for candidates that do not have an election result associated to them
df_master %>%
  filter(is.na(percent_1)) %>%
  view()




