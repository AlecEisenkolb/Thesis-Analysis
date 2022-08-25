# Summary Statistics and Tables
# Date: 06.07.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)

# import data
df_master <- read_csv("Clean Data/master_all_cand.csv")

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
          "district_avg_income", "district_unemprate_total", "district_pop_foreign",
          "district_age_young", "district_educ_general_total_grads_per1000")

# create empty dataframe table to store p-values in
test_table <- data.frame(matrix(NA, nrow=10, ncol=3))
colnames(test_table) <- c("Variable", "T-Test Unpaired", "Wilcoxon-Test Unpaired")

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
  
  # extract t-test p-value
  test_table[i, 2] <- as.numeric(ttest$p.value)
  
  # extract unpaired wilcoxon-test p-value
  test_table[i, 3] <- as.numeric(wtest$p.value)
  
  i <- i + 1
}

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


