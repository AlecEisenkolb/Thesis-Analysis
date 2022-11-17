# Regression Analysis
# Date: 14.10.2022
# Author: Alec Eisenkolb

# import libraries
library(tidyverse)
library(lmtest)
library(sandwich)
#install.packages("betareg")
library(betareg)
#install.packages("margins")
library(margins)
#install.packages("stargazer")
library(stargazer)
library(ggthemr)

library(broom)
library(coefplot)
library(ggeffects)
#install.packages("effects")
library(effects)
#install.packages("GGally")
library(GGally)
require(mgsub)

# define second colour scheme (used for line graphs for better contrast between groups)
colourful_palette <- c("#658354", "#b3cf99", "#47697E", "#688B9A", "#FFCC33", "#FEEB75")
colourful_palette <- c("#555555", colourful_palette)
theme2 <- define_palette(
  swatch = colourful_palette,
  gradient = c(lower = colourful_palette[1L], upper = colourful_palette[2L]),
  background = '#ffffff'
)
# set new theme as default
ggthemr_reset()
ggthemr(theme2, layout = "clean", spacing = 1)

# set path
PATH <- "Clean Data/"

# import data
df_master <- read_csv(paste0(PATH, "master_all_cand.csv"))

### run OLS regression: OLS 1
# dependent variable: percent vote from 1st vote (first-past-the-post vote)
# independent variable: bindary dummy whether candidates have a Twitter account
OLS_1 <- lm(I(percent_1/100) ~ Twitter_Acc, data = df_master)
# extract coefficients
summary(OLS_1)
# extract corefficients with robust standard errors
coeftest(OLS_1, vcov = vcovHC(OLS_1, type = "HC0"))
robust.se.OLS1 <- sqrt(diag(vcovHC(OLS_1, type = "HC0")))

# plot regression
plot(df_master$Twitter_Acc, (df_master$percent_1)/100)
abline(OLS_1)

### OLS 2: add further control variables (demographics) - age, gender, job 
# set factor variables and base categories for regression
df_master$job_key <- factor(df_master$job_key)
df_master$job_key <- relevel(df_master$job_key, 1) # set base level to be "Others" = students, pensioners, self-employed and job-seeking
# estimate model
OLS_2 <- lm(I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key, 
              data = df_master)
# extract coefficients
summary(OLS_2)
# extract corefficients with robust standard errors
coeftest(OLS_2, vcov = vcovHC(OLS_2, type = "HC0"))
robust.se.OLS2 <- sqrt(diag(vcovHC(OLS_2, type = "HC0")))

### OLS 3: add further control variables (politics) - incumbent, list place, district forecast, party dummy
# set factor variables and base categories for regression
df_master$party <- factor(df_master$party)
df_master$party <- relevel(df_master$party, 1) # set the party AfD as base category
# estimate model
OLS_3 <- lm(I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key + incumbent 
            + isListed + Prediction + party + Top_candidate, data = df_master)
# can change above variable "isListed" to "list_place" to see how actual place in list has an effect
# this is negative and significant at the 10% level
# extract coefficients
summary(OLS_3)
# extract corefficients with robust standard errors
coeftest(OLS_3, vcov = vcovHC(OLS_3, type = "HC0"))
robust.se.OLS3 <- sqrt(diag(vcovHC(OLS_3, type = "HC0")))

### OLS 4: add further control variables (structural) - district indicators and East-Germany dummy
OLS_4 <- lm(I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key + incumbent 
            + isListed + Prediction + party + Top_candidate 
            + I(log(district_avg_income)) + 
            + district_pop_foreign + district_age_60over + East_Germany, data = df_master) 
# extract coefficients
summary(OLS_4)
# extract corefficients with robust standard errors
coeftest(OLS_4, vcov = vcovHC(OLS_4, type = "HC0"))
robust.se.OLS4 <- sqrt(diag(vcovHC(OLS_4, type = "HC0")))

### Run a Breusch-Pagan test for heteroscedasticity
bptest(OLS_4) 
# as the test-statistic = 307.81 and the corresponding p-value = 2.2x10^-16 (i.e. is much less than 0.05),
# we can reject the null hypothesis and obtain enough evidence to indicate that heteroscedasticity is present in our data.

# residuals of OLS4
residuals <- as.vector(rstandard(OLS_4))
resid <- as.vector(resid(OLS_4))

# Shapiro-Wilk test of normality for residuals
shapiro.test(resid) # p-value of less than 2.2x10^-16 indicates non-normal residuals
# Kolmogorov-Smirnov test of normality for residuals
ks.test(resid, "pnorm") # p-value of less than 2.2x10^-16 indicates non-normal residuals

# plot residuals against fitted values
png("Graphs/Diagnostics/Fitted_V_Resid.png", units="in", width=9.5, height=7, res=300)
plot(fitted(OLS_4), resid, ylab="Residuals", xlab="Fitted Values", family="serif"); abline(0,0, col=2)
dev.off()

ggplot() +
  geom_point(aes(x=fitted(OLS_4), y=resid), shape=1) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Diagnostics/Fitted_V_Resid_OLS_2.png", dpi=300, width=9.5, height=7)

# qq-plot of residuals
png("Graphs/Diagnostics/QQ_Plot_OLS.png", units="in", width=9.5, height=7, res=300)
qqnorm(residuals, ylab="Standardized Residuals",
       xlab="Normal Scores",
       main="", family="serif"); qqline(residuals, col=2, family="serif")
dev.off()

# density plot of residuals
png("Graphs/Diagnostics/Density_Residuals_OLS.png", units="in", width=9.5, height=7, res=300)
plot(density(residuals), main="", family="serif")
dev.off()

plot(OLS_4)

# compute regression results table
stargazer(OLS_1, OLS_2, OLS_3, OLS_4, se=list(robust.se.OLS1, robust.se.OLS2,
                                              robust.se.OLS3, robust.se.OLS4),
          type="latex", single.row=FALSE)

### BETA REGRESSION MODELS ###

## Run a beta-regression model to alleviate the issues of heteroscedasticity and asymmetries we experience in the data
## Additionally useful to model a proportional dependent variable (percentage of first-past-the-post vote)
## Beta 1: Simple model specification with demographic control variables
beta_1 <- betareg(formula = I(percent_1/100) ~ Twitter_Acc + birth_year + gender 
                  + job_key, data=df_master)
# extract coefficients
summary(beta_1)

## Beta 2: model including political control variables
beta_2 <- betareg(formula = I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key 
                  + incumbent + isListed + Prediction + party + Top_candidate,
                  data = df_master)
# extract coefficients
summary(beta_2)

## Beta 3: model including structural control variables
beta_3 <- betareg(formula = I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key 
                  + incumbent + isListed + Prediction + party + Top_candidate 
                  + I(log(district_avg_income)) + district_pop_foreign 
                  + district_age_60over + East_Germany, data = df_master)
# extract coefficients
summary(beta_3)

# Run a likelihood ratio test for goodness of fit comparison of beta regression models
# where beta_3 is the full model, and beta_2 & beta_1 are two different nested models within the full model. 
lrtest(beta_3, beta_2, beta_1)

# Result: test statistic for both models are well below 0.05, hence we reject H0 and conclude that full model provides best overall fit. 

# Run a BIC (Bayes-Information-Criterion) for model selection, closely related to the AIC
AIC(beta_3, beta_2, beta_1, k=log(nrow(df_master)-4)) # subtract 4 from total observations as 4 obvs deleted in regression due to NAs

# Result: beta_3 (full model) and beta_2 have similar low BIC, with beta_2 performing slightly better. As these are very similar, 
# decide to continue with full model given lrtest showed beta_3 was the better overall fit. 

# Report the marginal effects of the three beta regressions above, to more easily interpret the coefficient estimates
margins(beta_1)
margins(beta_2)
margins(beta_3)

# compute regression results table
stargazer(beta_1, beta_2, beta_3, type="latex", single.row=FALSE)

## Test full beta regression (beta_3) using other link functions, instead of the default logit link function
# Probit, Log-Log, C-Log-Log and Cauchy link functions
sapply(c("logit", "probit", "loglog", "cloglog", "cauchit"),
       function(x) {logLik(update(beta_3, link = x))})

# Result: as the log-likelihood of both the probit and log-log link functions outperform the logit link function,
# we run the full model on these respective link functions and compute the coefficients and marginal effects.

# Beta Regression model using a Probit link function:
beta_probit <- update(beta_3, link = "probit")
# extract coefficients
summary(beta_probit)
# estimate marginal effects
margins(beta_probit)

# Beta Regression model using a Log-Log link function:
beta_loglog <- update(beta_3, link = "loglog")
# extract coefficients
summary(beta_loglog)
# estimate marginal effects
margins(beta_loglog)

# Result: running a beta regression using the probit and log-log link functions
# show no major differences in the magnitude and significance level of the independent variable.

### OLS AND BETA WITH PARTY INTERACTION EFFECTS ###

OLS_interact <- lm(I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key + incumbent 
                   + isListed + Prediction + Top_candidate 
                   + I(log(district_avg_income)) 
                   + district_pop_foreign + district_age_60over + East_Germany
                   + Twitter_Acc*I(party) , data = df_master)

beta_interact <- betareg(I(percent_1/100) ~ Twitter_Acc + birth_year + gender + job_key + incumbent 
                         + isListed + Prediction + Top_candidate 
                         + I(log(district_avg_income)) 
                         + district_pop_foreign + district_age_60over + East_Germany
                         + Twitter_Acc*I(party) , data = df_master)
# extract coefficients
summary(OLS_interact)
summary(beta_interact)
margins(beta_interact)

# compute regression results table
stargazer(OLS_interact, beta_interact, type="latex", single.row=FALSE)

# create table to convert coefficient names
coef_table <- tribble(
  ~Old_name, ~New_name,
  "CDU", "Twitter Acc. * CDU",
  "CSU", "Twitter Acc. * CSU",
  "DIE LINKE", "Twitter Acc. * LEF",
  "FDP", "Twitter Acc. * FDP",
  "GRÜNE", "Twitter Acc. * GRE",
  "SPD", "Twitter Acc. * SPD",
  "Twitter_Acc", "Twitter Acc."
)

# clean OLS results
ols_tidy <- tidy(OLS_interact, conf.int = TRUE) %>%
  filter(grepl("^Twitter_Acc", term)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(Reg = "OLS",
         term = as.character(mgsub(as.character(term), coef_table$Old_name, coef_table$New_name))) %>%
  mutate(term = str_sub(term, start = -18)) %>%
  mutate(term = as.character(mgsub(term, c("LEF", "GRE"), c("The Left", "The Greens"))))

beta_tidy <- tidy(beta_interact, conf.int = TRUE) %>%
  filter(grepl("^Twitter_Acc", term)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(Reg = "Beta",
         term = as.character(mgsub(as.character(term), coef_table$Old_name, coef_table$New_name))) %>%
  mutate(term = str_sub(term, start = -18)) %>%
  mutate(term = as.character(mgsub(term, c("LEF", "GRE"), c("The Left", "The Greens"))))

# Plot the coefficient estimates of both OLS and Beta interaction variables

ggcoef(ols_tidy) +
  ylab("Interaction Variable") +
  xlab("Coefficient Estimate") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/OLS_Interaction.png", dpi=300, width=9.5, height=7)

ggcoef(beta_tidy) +
  ylab("Interaction Variable") +
  xlab("Coefficient Estimate") +
  theme(text = element_text(family = "serif", size = 18))

ggsave("Graphs/Master/BETA_Interaction.png", dpi=300, width=9.5, height=7)

### MODELS WITH TWITTER ACTIVITY INDEPENDENT VARS ###

# only select candidates who have a twitter account
df_twitter <- df_master %>%
  filter(Twitter_Acc == 1)

### Test final beta regression model on other independent variables (logit link function - remain with the default)
## Independent variable: average weekly posting
beta_avgpost <- betareg(formula = I(percent_1/100) ~ Avg_Weekly_Posts 
                        + birth_year + gender + job_key + incumbent 
                        + isListed + Prediction + party + Top_candidate 
                        + log(district_avg_income) + district_age_60over 
                        + district_pop_foreign + East_Germany, data = df_twitter)
# extract coefficients
summary(beta_avgpost)
# estimate marginal effects
margins(beta_avgpost)

## Independent variable: average weekly posting in hot phase of election (Sep. - election day)
beta_hotphase <- betareg(formula = I(percent_1/100) ~ Avg_posts_time_group3 
                         + birth_year + gender + job_key + incumbent 
                         + isListed + Prediction + party + Top_candidate 
                         + log(district_avg_income) + district_age_60over 
                         + district_pop_foreign + East_Germany, data = df_twitter)
# extract coefficients
summary(beta_hotphase)
# estimate marginal effects
margins(beta_hotphase)

## Independent variable: difference of average weekly posting in hot phase of election compared to July & August average
# create variable that shows difference in weekly postings from July&August to September (hot phase of election)
reg <- df_twitter %>%
  mutate(Delta_AvgPost = (Avg_posts_time_group3 - (Avg_posts_time_group1 + Avg_posts_time_group2)/2))

beta_postdiff <- betareg(formula = I(percent_1/100) ~ Delta_AvgPost 
                         + birth_year + gender + job_key + incumbent + isListed 
                         + Prediction + party + Top_candidate 
                         + log(district_avg_income) + district_age_60over 
                         + district_pop_foreign + East_Germany, data = reg)
# extract coefficients
summary(beta_postdiff)
# estimate marginal effects
margins(beta_postdiff)

## Independent variable: average likes per post 
beta_likes <- betareg(formula = I(percent_1/100) ~ Mean_Likes + birth_year + gender 
                      + job_key + incumbent + isListed + Prediction + party + Top_candidate
                      + log(district_avg_income) + district_age_60over 
                      + district_pop_foreign + East_Germany, data = df_twitter)
# extract coefficients
summary(beta_likes)
# estimate marginal effects
margins(beta_likes)

## Independent variable: average replies per post 
beta_reply <- betareg(formula = I(percent_1/100) ~ Mean_Reply + birth_year + gender 
                      + job_key + incumbent + isListed + Prediction + party + Top_candidate
                      + log(district_avg_income) + district_age_60over 
                      + district_pop_foreign + East_Germany, data = df_twitter)
# extract coefficients
summary(beta_reply)
# estimate marginal effects
margins(beta_reply)

## Independent variable: average re-tweets per post 
beta_rt <- betareg(formula = I(percent_1/100) ~ Mean_RT + birth_year + gender 
                   + job_key + incumbent + isListed + Prediction + party + Top_candidate
                   + log(district_avg_income) + district_age_60over 
                   + district_pop_foreign + East_Germany, data = df_twitter)
# extract coefficients
summary(beta_rt)
# estimate marginal effects
margins(beta_rt)

# compute regression table with above beta-regression results
stargazer(beta_avgpost, beta_hotphase, beta_postdiff, beta_likes, beta_reply, beta_rt,
          type="latex", single.row=FALSE)

### OTHER MODELS ###

### Test full model on other dependent variables
## Dependent variable: dummy variable on whether candidate won seat
logit_win <- glm(winner ~ Twitter_Acc + birth_year + gender + job_key 
                    + incumbent + isListed + Prediction + party + Top_candidate 
                    + log(district_avg_income) + district_age_60over 
                    + district_pop_foreign + East_Germany, 
                    data = df_master, family = "binomial")
# extract coefficients
summary(logit_win)
# estimate marginal effects
margins(logit_win)

## Dependent variable: %-difference between 1st and 2nd vote per district
OLS_pctdiff <- lm(pct_diff_1to2 ~ Twitter_Acc + birth_year + gender + job_key 
                  + incumbent + isListed + Prediction + party + Top_candidate
                  + log(district_avg_income) + district_age_60over 
                  + district_pop_foreign + East_Germany, data = df_master)
# extract coefficients
summary(OLS_pctdiff)
# extract corefficients with robust standard errors
coeftest(OLS_pctdiff, vcov = vcovHC(OLS_pctdiff, type = "HC0"))
robust.se.OLSpctdiff <- sqrt(diag(vcovHC(OLS_pctdiff, type = "HC0")))

# run same regression as above, but without party affiliation
OLS_pctdiff_wo_party <- lm(pct_diff_1to2 ~ Twitter_Acc + birth_year + gender + job_key 
                  + incumbent + isListed + Prediction + Top_candidate
                  + log(district_avg_income) + district_age_60over 
                  + district_pop_foreign + East_Germany, data = df_master)
# extract coefficients
summary(OLS_pctdiff_wo_party)
# extract corefficients with robust standard errors
coeftest(OLS_pctdiff_wo_party, vcov = vcovHC(OLS_pctdiff_wo_party, type = "HC0"))
robust.se.OLSpctdiff_wo_party <- sqrt(diag(vcovHC(OLS_pctdiff_wo_party, type = "HC0")))


# create table of regression results above
stargazer(OLS_pctdiff, OLS_pctdiff_wo_party, se=list(robust.se.OLSpctdiff, robust.se.OLSpctdiff_wo_party), 
          type="latex", single.row = FALSE)


