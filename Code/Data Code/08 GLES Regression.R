# Robustness Check: Regression Analysis using GLES 2021 Data
# Date: 08.10.2022
# Author: Alec Eisenkolb

# install package pacman to access function p_load to load and install packages
if (!require("pacman")) install.packages("pacman")

# import libraries
pacman::p_load(tidyverse,
               betareg,
               margins,
               ivtools,
               stargazer,
               ivreg)

# import data
df_gles <- read_csv("Clean Data/GLES_2021.csv")

# investigate balance of this variable for regressions
table(df_gles$Facebook_acc)
table(df_gles$Youtube_acc)
table(df_gles$FB_TW_acc)
table(df_gles$Social_media_AND)

# re-level highest educational attainment variable
table(df_gles$Educ)
df_gles$Educ <- factor(df_gles$Educ)
df_gles$Educ <- relevel(df_gles$Educ, 2) # set "Other" as base level

# define model with twitter variable defined by the GESIS data set
beta_gles <- betareg(formula = I(percent_election_erststimme/100) ~ twitter_acc 
                + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD
                + I(log(district_avg_income)) + district_age_60over 
                + district_pop_foreign + east_germany, data = df_gles)
# extract coefficients
summary(beta_gles)
# estimate marginal effects
margins(beta_gles)

# define model with the twitter variable from the GLES data set
beta_gles_tw <- betareg(formula = I(percent_election_erststimme/100) ~ Twitter_GLES 
                     + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                     + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD
                     + I(log(district_avg_income)) + district_age_60over 
                     + district_pop_foreign + east_germany, data = df_gles)
# extract coefficients
summary(beta_gles_tw)
# estimate marginal effects
margins(beta_gles_tw)

### Run similar regressions not only on the dummy of having a twitter account, but also on facebook and social media in general

## Run beta regression model on Facebook dummy
beta_fb <- betareg(formula = I(percent_election_erststimme/100) ~ Facebook_acc 
                   + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                   + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD 
                   + I(log(district_avg_income)) + district_age_60over 
                   + district_pop_foreign + east_germany,
                   data = df_gles)
# extract coefficients
summary(beta_fb)
# estimate marginal effects
margins(beta_fb)

## Run beta regression model on YouTube dummy
beta_yt <- betareg(formula = I(percent_election_erststimme/100) ~ Youtube_acc 
                   + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                   + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD 
                   + I(log(district_avg_income)) + district_age_60over 
                   + district_pop_foreign + east_germany,
                   data = df_gles)
# extract coefficients
summary(beta_yt)
# estimate marginal effects
margins(beta_yt)

## Run beta regression model on Facebook/Twitter dummy
beta_fb_tw <- betareg(formula = I(percent_election_erststimme/100) ~ FB_TW_acc 
                     + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                     + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD 
                     + I(log(district_avg_income)) + district_age_60over
                     + district_pop_foreign + east_germany,
                     data = df_gles)
# extract coefficients 
summary(beta_fb_tw)
# estimate marginal effects
margins(beta_fb_tw)

## Run beta regression model on Social Media dummy
beta_sm <- betareg(formula = I(percent_election_erststimme/100) ~ Social_media_AND 
                   + geburtsjahr + geschlecht + Educ + Incumbent + IsListed + Prediction 
                   + partyCDU + partyCSU + partyDIELINKE + partyFDP + partyGRUENE + partySPD 
                   + I(log(district_avg_income)) + district_age_60over 
                   + district_pop_foreign + east_germany,
                   data = df_gles)

# extract coefficients
summary(beta_sm)
# estimate marginal effects
margins(beta_sm)

# summarise regression results above in a table
stargazer(beta_gles, beta_gles_tw, beta_fb, beta_yt, beta_fb_tw, beta_sm,
          type="latex",
          single.row=FALSE)

### Part below not used in Thesis

# # # # # IV Regression Analysis # # # # #

### Run IV regression on GLES data as robustness check
# Dependent variable: Winner of election indicator
# Endogenous variable: Twitter account indicator

# check correlation between instrument and treatment (twitter account)
cor(df_gles$SM_preference, df_gles$twitter_acc, use = "complete.obs") # correlation = 0.115
# check correlation between instrument and twitter use
cor(df_gles$SM_preference, df_gles$avg_weekly_posts, use = "complete.obs") # correlation = 0.0368

# Run first-stage regression: linear model of instrument on regressors
glm_firststage <- glm(formula = twitter_acc ~ SM_preference
                    + geburtsjahr + geschlecht + e10 + Incumbent + Prediction 
                    + partei + I(log(district_avg_income)) + district_unemprate_total 
                    + district_pop_foreign + east_germany,
                    data = df_gles)
# extract coefficients
summary(glm_firststage)

# Run second stage regression
glm_secondstage <- glm(formula = gewaehlt ~ twitter_acc 
                            + geburtsjahr + geschlecht + e10 + Incumbent + Prediction 
                            + partei + I(log(district_avg_income)) + district_unemprate_total 
                            + district_pop_foreign + east_germany,
                            data = df_gles)
# get summary
summary(glm_secondstage)
# set IV regression between first stage and second stage above
iv_regression <- ivglm(estmethod="ts", 
                       fitX.LZ = glm_firststage, 
                       fitY.LX = glm_secondstage,
                       data=df_gles)
summary(iv_regression)

### Run a linear 2SLS IV-regression between outcome of first-past-the-post (percentage) and instrument
# Dependent variable: proportion of votes won in first-past-the-post vote
# Endogenous variable: Twitter account indicator
lm_iv <- ivreg(formula = I(percent_election_erststimme/100) ~ geburtsjahr 
               + geschlecht + e10 + Incumbent + Prediction 
               + partei + I(log(district_avg_income)) + district_unemprate_total 
               + district_pop_foreign + east_germany | twitter_acc | SM_preference,
               data = df_gles)
# extract coefficients
summary(lm_iv)
# compute robust standard errors
robust.se.lm_iv <- sqrt(diag(vcovHC(lm_iv, type = "HC0")))
# put results into a regression table
stargazer(lm_iv, type="html", se=list(robust.se.lm_iv),
          out="Tables/IV_linear.html", single.row=TRUE)

### IV-Regression analysis on the social media indicator variable
# Dependent variable: Winner of election indicator
# Endogenous variable: Social media indicator
# check correlation between instrument and treatment (social media)
cor(df_gles$SM_preference, df_gles$Social_media, use = "complete.obs") # correlation = 0.118

# Run first-stage regression: linear model of instrument on regressors and social media
glm_firststage_sm <- glm(formula = Social_media ~ SM_preference
                      + geburtsjahr + geschlecht + e10 + Incumbent + Prediction 
                      + partei + I(log(district_avg_income)) + district_unemprate_total 
                      + district_pop_foreign + east_germany,
                      data = df_gles)
# extract coefficients
summary(glm_firststage_sm)
# Run second stage regression
glm_secondstage_sm <- glm(formula = gewaehlt ~ Social_media 
                       + geburtsjahr + geschlecht + e10 + Incumbent + Prediction 
                       + partei + I(log(district_avg_income)) + district_unemprate_total 
                       + district_pop_foreign + east_germany,
                       data = df_gles)
# get summary
summary(glm_secondstage_sm)
# run IV regression between first stage and second stage above
iv_regression_sm <- ivglm(estmethod="ts", 
                       fitX.LZ = glm_firststage_sm, 
                       fitY.LX = glm_secondstage_sm,
                       data=df_gles)
summary(iv_regression_sm)

### Run linear 2SLS IV-regression for social media indicator
# Dependent variable: proportion of votes in first-past-the-post vote
# Endogenous variable: social media indicator
lm_iv_sm <- ivreg(formula = I(percent_election_erststimme/100) ~ geburtsjahr 
               + geschlecht + e10 + Incumbent + Prediction 
               + partei + I(log(district_avg_income)) + district_unemprate_total 
               + district_pop_foreign + east_germany | Social_media | SM_preference,
               data = df_gles)
# extract coefficients
summary(lm_iv_sm)
# compute robust standard errors
robust.se.lm_iv_sm <- sqrt(diag(vcovHC(lm_iv_sm, type = "HC0")))
# put results into a regression table
stargazer(lm_iv_sm, type="html", se=list(robust.se.lm_iv_sm),
          out="Tables/IV_linear_sm.html", single.row=TRUE)
  
