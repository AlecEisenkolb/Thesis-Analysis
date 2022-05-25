# this code reproduces the district level predictions and further quantities of interest in
# Gschwend, Müller, Munzert, Neunhoeffer, Stoetzer 2021
# 'The Zweitstimme Model: A Dynamic Forecast of the 2021 German Federal Election'

# load packages

p_needed <-
  c(
    "stringi",
    "keras",
    "tidyverse",
    "MASS",
    "future.apply",
    "rgdal",
    "geojsonio",
    "rmapshaper",
    "abind",
    "maptools",
    "cartogram",
    "magrittr",
    "abind"
  )

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
print(sapply(p_needed, require, character.only = TRUE))

#plan(multiprocess)


if(replicate){
# Election Results for 09, 13 and 17

c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
res17 <- c(32.9, 20.5, 9.2, 8.9, 10.7, 12.6, 5) / 100
res13 <- c(41.5, 25.7, 8.6, 8.4, 4.8, 4.7, 6.3) / 100
res09 <- c(33.8, 23, 11.9, 10.7, 14.6, 0.1, 6) / 100
res05 <- c(35.2, 34.2, 8.7, 8.1, 9.8, 0.1, 3.9) / 100

btw_bund_res <- rbind(res17, res13, res09, res05)

colnames(btw_bund_res) <-
  c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")

rownames(btw_bund_res) <- c("2017", "2013", "2009", "2005")

# Read in the candidate data.

full_df <-
  read.csv2("district_prediction/raw-data/btw_candidates_1983-2017.csv",
            stringsAsFactors = F)

election <- 2021
election_l1 <- election - 4

# load Zweitstimme forecast

# Option 1: to reproduce results in the paper, load forecast data from 2021-06-17

forecast <-
  readRDS("district_prediction/processed-data/forecast.RDS")

# Option 2: to instead run the model on the most recent forecast
# uncomment the code chunk below until line 85

# get_zweitstimme_api <-
#   httr::GET("http://api.zweitstimme.org/results")
#
# # Parse GET object to raw json
# zweitstimme_api_to_json <-
#   httr::content(get_zweitstimme_api, as = "text")
#
# # From json to R list
# zweitstimme_output <- jsonlite::fromJSON(zweitstimme_api_to_json)
#
# rm(get_zweitstimme_api,zweitstimme_api_to_json )
#
# forecast <- zweitstimme_output$forecast
#
# adjustOrder <-
#   match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"),
#         colnames(forecast))
#
# forecast <- forecast[, adjustOrder]
#
# colnames(forecast) <-
#   c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")

# uncomment to draw random sample of forecasts, decreases run time of the model
# forecast <- forecast[sample(nrow(forecast), size=2000, replace=F),]

nsim <- nrow(forecast)

# set seed

set.seed(20210617)

# Split into training and test data.

train <-
  full_df[(full_df$election < election &
             full_df$election != 1990) |
            (full_df$election == 1990 & full_df$east != 1) ,]

test <- full_df[full_df$election == 2017, ]

test$weight <- 1

ff <-
  "resp_E ~ ncand + propPlatz + resp_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"

reg <-
  lm(ff,
     data = train)

mu_nsim <- 25

S <- mvrnorm(n = mu_nsim, coef(reg), vcov(reg))

rf_df <-
  model.frame(as.formula(ff),
              data = train)

rf_df <- rf_df[sample(1:nrow(rf_df), nrow(rf_df)),]

# The test set without vote share predictions is using the last result.

ff_test <-
  "resp_E ~ ncand + propPlatz + res_l1_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"

rf_test <-
  model.frame(as.formula(ff_test),
              data = test)

# That it works with lm the variables in training and test have to have the
# same names though.

colnames(rf_df) <-
  colnames(rf_test) <-
  c(
    "resp_E",
    "ncand",
    "propPlatz",
    "resp_Z",
    "res_l1_E",
    "formercand",
    "east",
    "female",
    "incumbent",
    "akad",
    "incumbent_in_wkr"
  )

y <- rf_df$resp_E

# The neural net needs the predictor variables in a matrix.

x <- as.matrix(rf_df[, 2:(ncol(rf_df))])

# Rescale the data for better convergence of the neural net.

means <- apply(x , 2, mean, na.rm = T)
sds <- apply(x , 2, sd, na.rm = T)

# Don't rescale categorical/binary features

means[c(2, 5:10)] <- 0
sds[c(2, 5:10)] <- 1

x_rescaled <-
  as.matrix(sweep(sweep(x, 2, means), 2, sds, "/"))

test_y <- rf_test$resp_E

test_x <- as.matrix(rf_test[, 2:(ncol(rf_test))])

# Rescale the test set with the same means and sds as the training set.

test_x_rescaled <-
  as.matrix(sweep(sweep(test_x, 2, means), 2, sds, "/"))

k_clear_session()

# Dropout to prevent overfitting and for model uncertainty of the neural net.

drop_out <- layer_dropout(rate = 0.1)

input <- layer_input(shape = c(ncol(test_x)))

output <- input %>%
  layer_dense(
    units = 128,
    activation = "tanh",
    kernel_initializer = "glorot_normal",
    input_shape = c(ncol(x))
  ) %>%
  
  drop_out(training = T) %>%
  layer_dense(units = 64,
              activation = "tanh",
              kernel_initializer = "glorot_normal") %>%
  drop_out(training = T) %>%
  
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)

model %>% compile(optimizer = "adam",
                  loss = "mse")

callbacks_list <- list(
  callback_early_stopping(monitor = "loss",
                          patience = 25),
  callback_reduce_lr_on_plateau(
    factor = 0.2,
    patience = 5,
    min_lr = 0.0001
  )
)

shuf_ind <- sample(nrow(x_rescaled), nrow(x_rescaled))

# Train neural net.

nn_history <- model %>% fit(
  x = x_rescaled[shuf_ind, ],
  y = y[shuf_ind],
  validation_split = 0.1,
  epochs = 500,
  batch_size = 500,
  callbacks = callbacks_list
)

res_el <- btw_bund_res[paste0(election_l1), ]

sim.swing <- -sweep(-forecast, 2,-res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x)
  x / res_el))

zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.swing))

for (i in 1:nrow(test)) {
  if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
    zs_pred[i,] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  if (test[i, "partei"] == "CSU") {
    zs_pred[i,] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  
  if (!(test[i, "partei"] %in% colnames(sim.prop.swing)) &
      test[i, "partei"] != "CSU") {
    zs_pred[i,] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, "And"] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
}

test_sim <-
  data.frame(
    test$resp_E,
    test$ncand,
    test$propPlatz,
    zs_pred[, 2],
    test$resp_E,
    test$formercand,
    test$east,
    test$female,
    test$incumbent,
    test$akad,
    test$incumbent_in_wkr
  )

colnames(test_sim) <-
  c(
    "resp_E",
    "ncand",
    "propPlatz",
    "resp_Z",
    "res_l1_E",
    "formercand",
    "east",
    "female",
    "incumbent",
    "akad",
    "incumbent_in_wkr"
  )

test_y <- test_sim$resp_E

vals <-
  t(sapply(aggregate(test, list(test$wkr), function(x)
    x)$partei, '[', seq(max(
      sapply(aggregate(test, list(test$wkr), function(x)
        x)$partei, length)
    ))))

res_list <- vector("list", length = nrow(forecast))

for (zsim in 1:nrow(forecast)) {
  test_sim$resp_Z <- zs_pred[, zsim]
  
  test_x <- as.matrix(test_sim[, 2:(ncol(test_sim))])
  
  test_x_rescaled <-
    as.matrix(sweep(sweep(test_x, 2, means), 2, sds, "/"))
  
  preds <- vector("list", length = mu_nsim)
  
  for (i in 1:mu_nsim) {
    preds[[i]] <- predict(model, x = test_x_rescaled)
  }
  
  preds <- do.call("cbind", preds)
  
  for (j in 1:299) {
    preds[test$wkr == j, ] <-
      sweep(preds[test$wkr == j,], 2, colSums(preds[test$wkr == j, ]), "/")
  }
  
  tmp_winner <- future_lapply(1:mu_nsim, function(x) {
    select <-
      cbind(1:299, unlist(lapply(
        aggregate(-preds[, x], list(test$wkr), order)[, 2], `[[`, 1
      )))
    
    vals[select]
  })
  
  res_list[[zsim]] <-
    list(winner_nn = tmp_winner)
  cat(zsim, "of", nrow(forecast), "\n")
  
}

# save the results

saveRDS(res_list, "district_prediction/processed-data/2021_wkr.RDS")
} else {
  
  election <- 2021
  election_l1 <- election - 4
  
  c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
  res17 <- c(32.9, 20.5, 9.2, 8.9, 10.7, 12.6, 5) / 100
  res13 <- c(41.5, 25.7, 8.6, 8.4, 4.8, 4.7, 6.3) / 100
  res09 <- c(33.8, 23, 11.9, 10.7, 14.6, 0.1, 6) / 100
  res05 <- c(35.2, 34.2, 8.7, 8.1, 9.8, 0.1, 3.9) / 100
  
  btw_bund_res <- rbind(res17, res13, res09, res05)
  
  colnames(btw_bund_res) <-
    c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
  
  rownames(btw_bund_res) <- c("2017", "2013", "2009", "2005")
  
  forecast <-
    readRDS("district_prediction/processed-data/forecast.RDS")
  nsim <- nrow(forecast)
  mu_nsim <- 25
  res_list <- readRDS("district_prediction/processed-data/2021_wkr.RDS")
}

# get district winner

winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_nn")),
    nrow = nsim * mu_nsim,
    ncol = 299,
    byrow = T
  )

# get winning probabilities

win_probs <- apply(winner, 2, function(x)
  - sort(-table(x) / nrow(winner)))

wber <- read.csv2("district_prediction/raw-data/btw17_wber.csv")

wahl1721 <-
  read.csv2(
    "district_prediction/raw-data/btwkr21_umrechnung_btw17.csv",
    skip = 4,
    # fileEncoding = "UTF-8",
    na.strings = "",
    stringsAsFactors = F
  )

res_el <- btw_bund_res[paste0(election_l1), ]

sim.swing <- -sweep(-forecast, 2,-res_el)

sim.prop.swing <- t(apply(sim.swing, 1, function(x)
  x / res_el))

wahl1721 <- wahl1721[-1,]
wahl1721 <- wahl1721[-nrow(wahl1721),]
wahl1721 <-
  wahl1721[!(wahl1721[, 3] == "Land" |
               wahl1721[, 3] == "Insgesamt"),]

wahl1721$Land1 <- as.numeric(as.factor(wahl1721$Land))

wahl17_Z <-
  as.matrix((wahl1721[, c(9, 11, 13, 21, 15, 17, 19, 23)]))

class(wahl17_Z) <- "numeric"
wahl17_Zp <- wahl17_Z[, 2:8] / wahl17_Z[, 1] * 100

wahl17_E <-
  as.matrix((wahl1721[, c(9, 11, 13, 21, 15, 17, 19, 23) - 1]))

class(wahl17_E) <- "numeric"
wahl17_Ep <- wahl17_E[, 2:8] / wahl17_E[, 1] * 100

wahl17_Zp[, 1] <-
  ifelse(wahl17_Zp[, 1] == 0, wahl17_Zp[, 6], wahl17_Zp[, 1])
wahl17_Zp <- wahl17_Zp[, c(1, 2, 4, 5, 3, 7)]

wahl17_Ep[, 1] <-
  ifelse(wahl17_Ep[, 1] == 0, wahl17_Ep[, 6], wahl17_Ep[, 1])
wahl17_Ep <- wahl17_Ep[, c(1, 2, 4, 5, 3, 7)]

propEZ <- wahl17_Ep / wahl17_Zp

# Mean imputation for districts with no AfD candidate 2017

propEZ[propEZ[, 6] == 0 |
         is.na(propEZ[, 6]), 6] <-
  mean(propEZ[propEZ[, 6] > 0, 6], na.rm = T)

nsims <- nrow(forecast)

mu_nsims <- 25

simZ <- array(NA, dim = c(nsims * mu_nsims, 6, 299))

counter <- 1

cat("\n Simulate District Results: \n")
pb <- txtProgressBar(min = 0, max = 299, style = 3)
for (wkr in 1:299) {
  for (party in 1:6) {
    for (nsim in 1:nsims) {
      for (mu_nsim in 1:mu_nsims) {
        simZ[counter, party, wkr] <-
          wahl17_Zp[wkr, party] + sim.prop.swing[nsim, party] * wahl17_Zp[wkr, party]
        counter <- counter + 1
        if (counter > (nsims * mu_nsims)) {
          counter <- 1
        }
      }
    }
  }
  setTxtProgressBar(pb, wkr)
}

close(pb)

# start to calculate seat distribution

# Set population size for each Bundesland (based on 2017 data)
Bev <- c(
  2673803,
  1525090,
  7278789,
  568510,
  15707569,
  5281198,
  3661245,
  9365001,
  11362245,
  899748,
  2975745,
  2391746,
  1548400,
  3914671,
  2145671,
  2077901
)

Land <-
  c(
    "SH",
    "HH",
    "NI",
    "HB",
    "NW",
    "HE",
    "RP",
    "BW",
    "BY",
    "SL",
    "BE",
    "BB",
    "MV",
    "SN",
    "ST",
    "TH"
  )

# calculate divisor

div1 <- round(sum(Bev) / 598, 0)

while (sum(round(Bev / div1, 0)) < 598) {
  div1 <- div1 - 1
}

while (sum(round(Bev / div1, 0)) > 598) {
  div1 <- div1 + 1
}

lsitze <- round(Bev / div1, 0)

lsitze <- data.frame(as.numeric(as.factor(Land)), lsitze)

lsitze <- lsitze[, 2][order(lsitze[, 1])]

df.forecast <-
  lapply(1:nsims, function (x)
    t(replicate(mu_nsims, as.matrix(forecast)[x,])))

df.forecast <- do.call("rbind", df.forecast)

# include 5% hurdle and Grundmandatsklausel

mandates <- df.forecast
mandates[, 1:7] <- NA

for (party in colnames(mandates)) {
  for (i in 1:(nsims * mu_nsims)) {
    mandates[i, which(colnames(mandates) == party)] <-
      length(winner[i, ][winner[i, ] == party])
    
  }
}

df.forecast <- df.forecast * 100
klausel <- df.forecast >= 5 | mandates >= 3
klausel[, 7] <- TRUE

tmpZstimme <- array(NA, c(nsims * mu_nsims, 7, 299))

wbet <- wahl1721[, 5] / wahl1721[, 4]

simZCsu <- simZ

simZCsu <-
  abind(simZ, array(0, replace(dim(simZ), 2, 1)), along = 2)

simZCsu[, 7, wahl1721$Wkr.Nr.[wahl1721$Land1 == 4]] <-
  simZCsu[, 1, wahl1721$Wkr.Nr.[wahl1721$Land1 == 4]]

simZCsu[, 1, wahl1721$Wkr.Nr.[wahl1721$Land1 == 4]] <- 0

cat("\n Incorporate 5% or 3 Mandates Hurdle: \n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

for (sim in 1:(nsims * mu_nsims)) {
  for (party in 1:7) {
    tmpZstimme[sim, party,] <-
      if (klausel[sim, party])
        round(simZCsu[sim, party,] / 100 * wbet * wber$Wber, 0)
    else
      rep(0, 299)
  }
  setTxtProgressBar(pb, sim)
}
close(pb)

finZstimme <- array(NA, c(299, 8, nsims * mu_nsims))

for (i in 1:(nsims * mu_nsims)) {
  tmp <- t(tmpZstimme[i, ,])
  finZstimme[, , i] <- cbind(tmp, wahl1721$Land1)
}

# aggregate votes per Land

Lergebnis <- array(NA, c(16, 7, nsims * mu_nsims))

cat("\n Calculate Results by Land: \n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Lergebnis[j, i, k] <-
        sum(finZstimme[, i, k][finZstimme[, 8, k] == j])
    }
  }
  setTxtProgressBar(pb, k)
}
close(pb)

cat("\n Calculate Seat Distribution by Party and Land: \n This might take a while...\n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

div2 <- matrix(NA, nsims * mu_nsims, 16)

for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    div2[j, i] <- round(sum(Lergebnis[i, , j]) / lsitze[i], 0)
    
    
    while (sum(round(Lergebnis[i, , j] / div2[j, i], 0)) < lsitze[i]) {
      div2[j, i] <- div2[j, i] - 1
    }
    
    while (sum(round(Lergebnis[i, , j] / div2[j, i], 0)) > lsitze[i]) {
      div2[j, i] <- div2[j, i] + 1
    }
  }
  setTxtProgressBar(pb, j)
}
close(pb)

Psitze <- array(NA, c(16, 7, nsims * mu_nsims))

for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    Psitze[i, , j] <- round(Lergebnis[i, , j] / div2[j, i], 0)
  }
}

adj.simWin <- array(NA, dim = c(nsims * mu_nsims, 7, 299))

winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_nn")),
    nrow = nsims * mu_nsims,
    ncol = 299,
    byrow = T
  )

cat("\n Get District Winners:\n")
pb <- txtProgressBar(min = 0, max = 299, style = 3)
for (wkr in 1:299) {
  for (nsim in 1:(nsims * mu_nsims)) {
    adj.simWin[nsim, , wkr] <-
      c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "CSU") %in% winner[nsim, wkr]
  }
  setTxtProgressBar(pb, wkr)
}
close(pb)

o.dir <- adj.simWin

# district mandates

new.dir <- array(NA, c(299, 7, nsims * mu_nsims))

for (i in 1:(nsims * mu_nsims)) {
  new.dir[, , i] <- t(o.dir[i, ,])
}

new.dirid <- array(NA, c(299, 8, nsims * mu_nsims))

for (i in 1:(nsims * mu_nsims)) {
  new.dirid[, , i] <- cbind(t(o.dir[i, ,]), wahl1721$Land1)
}

Pdirect <- array(NA, c(16, 7, nsims * mu_nsims))

for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    for (k in 1:7) {
      Pdirect[i, k, j] <- sum(new.dirid[, k, j][new.dirid[, 8, j] == i])
    }
  }
}

# first, seats are distributed according to 'old' electoral law from 2017
# minimum number of seats per party from first and second vote

Pmin <- array(NA, c(16, 7, nsims * mu_nsims))

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Pmin[j, i, k] <-
        ifelse(Psitze[j, i, k] >= Pdirect[j, i, k], Psitze[j, i, k], Pdirect[j, i, k])
    }
  }
}

totfin <- matrix(NA, nsims * mu_nsims, 8)
for (i in 1:(nsims * mu_nsims)) {
  totfin[i,] <- apply(finZstimme[, , i], 2, sum)
}

totfin <- totfin[, 1:7]

totPmin <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  totPmin[i,] <- apply(Pmin[, , i], 2, sum)
}

# smallest divisor is used to calculate number of seats

bundsitze <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  bundsitze[i,] <-
    round(totfin[i,] / min(round((
      totfin[i,] / (totPmin[i,] - 0.48)
    )[(totfin[i,] / (totPmin[i,] - 0.48)) > 0], 0)), 0)
}

# size of the Bundestag

bt <- apply(bundsitze, 1, sum)

# Set to TRUE if you want the final distribution. Takes a while.

if (FALSE) {
  div3 <- matrix(NA, nsims * mu_nsims, 7)
  
  for (j in 1:(nsims * mu_nsims)) {
    cat("\n", j, "\t")
    for (i in 1:7) {
      cat(i)
      div3[j, i] <-
        ifelse(round(sum(Lergebnis[, i, j]) / bundsitze[j, i], 0) == "NaN",
               1,
               round(sum(Lergebnis[, i, j]) / bundsitze[j, i], 0))
      if (div3[j, i] != 1) {
        while (sum(apply(rbind(
          round(Lergebnis[, i, j] / div3[j, i], 0)
          , Pdirect[, i, j]
        ), 2, max)) > bundsitze[j, i]) {
          div3[j, i] <- div3[j, i] + 1
        }
        while (sum(apply(rbind(
          round(Lergebnis[, i, j] / div3[j, i], 0)
          , Pdirect[, i, j]
        ), 2, max)) < bundsitze[j, i]) {
          div3[j, i] <- div3[j, i] - 1
        }
      }
    }
  }
  
  # final distribution of seats
  
  finsitze <- array(NA, c(7, 16, nsims * mu_nsims))
  
  for (j in 1:(nsims * mu_nsims)) {
    for (i in 1:7) {
      finsitze[i, , j] <-
        apply(rbind(round(Lergebnis[, i, j] / div3[j, i], 0)
                    , Pdirect[, i, j]), 2, max)
    }
  }
}

# plot simulated size of the Bundestag

plot(
  table(bt),
  main = "Simulated Size of the German Bundestag \nfor 2021. Electoral Law for 2017.",
  xlab = "Number of seats",
  ylab = "Frequency",
  las = 1,
  xaxt = "n",
  bty = "n",
  col = "grey"
)
axis(side = 1)

# repeat the procedure for seat distribution according to 'new' electoral law 2021
# all previous steps until remain equal
# minimum number of seats per party from first and second vote

Pmin_new <- array(NA, c(16, 7, nsims * mu_nsims))

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Pmin_new[j, i, k] <-
        ifelse(Pdirect[j, i, k] >= ceiling((Psitze[j, i, k] +  Pdirect[j, i, k]) /
                                             2),
               Pdirect[j, i, k], ceiling((Psitze[j, i, k] +  Pdirect[j, i, k]) /
                                           2))
    }
  }
}

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Pmin_new[j, i, k] <-
        ifelse(Pmin_new[j, i, k] < Psitze[j, i, k], Psitze[j, i, k], Pmin_new[j, i, k])
    }
  }
}

totfin_new <- matrix(NA, nsims * mu_nsims, 8)

for (i in 1:(nsims * mu_nsims)) {
  totfin_new[i,] <- apply(finZstimme[, , i], 2, sum)
}

totfin_new <- totfin_new[, 1:7]

totPmin_new <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  totPmin_new[i,] <- apply(Pmin_new[, , i], 2, sum)
}

# overhang mandates

totPsitze <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  totPsitze[i,] <- apply(Psitze[, , i], 2, sum)
}

totover <- totPmin_new - totPsitze

# overhang per party and Land

over <- Pmin_new - Psitze

# get smallest divisor

bundsitze_new <- matrix(NA, nsims * mu_nsims, 7)

for (i in 1:(nsims * mu_nsims)) {
  div <- min(round((totfin_new[i,] / (
    totPmin_new[i,] - 0.48
  ))
  [(totfin_new[i,] / (totPmin_new[i,] - 0.48)) > 0], 0))
  
  while (sum((totPmin_new[i, ] - round(totfin_new[i,] / div, 0))
             [(totPmin_new[i, ] - round(totfin_new[i,] / div, 0)) > 0]) != 3) {
    div <- div + 1
  }
  
  bundsitze_new[i,] <- round(totfin_new[i,] / div, 0)
}

fin_over <- totPmin_new - bundsitze_new

# size of the Bundestag

bt_new <- apply(bundsitze_new, 1, sum)

# distribute remaining overhang seats

over_div <- array(NA, c(16, 7, nsims * mu_nsims))
remaining <- array(NA, c(16, 7, nsims * mu_nsims))

# get divisors

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      over_div[j, i, k] <-
        ifelse(over[j, i, k] > 0, Lergebnis[j, i, k] / (Pmin_new[j, i, k] - (fin_over[k, i] - 0.5)), NA)
    }
  }
}

# determine Land per party where remaining overhang mandates are kept

for (k in 1:(nsims * mu_nsims)) {
  for (i in 1:7) {
    if (fin_over[k, i] > 0) {
      aux <- over_div[, i, k]
      aux %<>% sort(aux, decreasing = F)
      
      # find smallest divisors
      divs <- aux[1:fin_over[k, i]]
      
      for (l in 1:length(divs)) {
        aux <- which(over_div[, i, k] == divs[l])
        
        # mark Land and party
        remaining[aux, i, k] <- 1
        
      }
    }
  }
}

# CSU overhang mandates
remaining[4, 7, ] <- ifelse(fin_over[, 7] > 0, fin_over[, 7], NA)

# plot simulated size of Bundestag

plot(
  table(bt_new),
  main = "Simulated Size of the German Bundestag \nfor 2021. Electoral Law for 2021.",
  xlab = "Number of seats",
  ylab = "Frequency",
  las = 1,
  xaxt = "n",
  bty = "n",
  col = "grey"
)
axis(side = 1)

# quantities of interest

# add column of bt size

bundsitze <- cbind(bundsitze, apply(bundsitze, 1, sum))

bundsitze_new <- cbind(bundsitze_new, apply(bundsitze_new, 1, sum))

# coalition probabilites

prob1 <- (sum((
  bundsitze_new[, 1]
  + bundsitze_new[, 7]
  + bundsitze_new[, 2]
) > (round(bundsitze_new[, 8] / 2))) / (mu_nsims * nsims)) * 100
cat("<\n CDU-CSU-SPD: \n ~", prob1)

prob2 <- (sum((
  bundsitze_new[, 1]
  + bundsitze_new[, 7]
  + bundsitze_new[, 4]
) > (round(bundsitze_new[, 8] / 2))) / (mu_nsims * nsims)) * 100
cat("\n CDU-CSU-Greens: \n ~", prob2)

prob3 <- (sum((
  bundsitze_new[, 1]
  + bundsitze_new[, 7]
  + bundsitze_new[, 4]
  + bundsitze_new[, 5]
) > (round(bundsitze_new[, 8] / 2))
) / (mu_nsims * nsims)) * 100
cat("\n CDU-CSU-Greens-FDP: \n ~", prob3)

prob4 <- (sum((
  bundsitze_new[, 4]
  + bundsitze_new[, 2]
  + bundsitze_new[, 5]
) > (round(bundsitze_new[, 8] / 2))) / (mu_nsims * nsims)) * 100
cat("\n Greens-SPD-FDP: \n ~", prob4)

prob5 <- (sum((
  bundsitze_new[, 4]
  + bundsitze_new[, 2]
  + bundsitze_new[, 3]
) > (round(bundsitze_new[, 8] / 2))) / (mu_nsims * nsims)) * 100
cat("\n Greens-SPD-Linke: \n ~", prob5)

prob6 <- (sum((
  bundsitze_new[, 1]
  + bundsitze_new[, 7]
  + bundsitze_new[, 2]
  + bundsitze_new[, 5]
) > (round(bundsitze_new[, 8] / 2))
) / (mu_nsims * nsims)) * 100
cat("\n CDU-CSU-SPD-FDP: \n ~", prob6)

prob7 <- (sum((
  bundsitze_new[, 1]
  + bundsitze_new[, 7]
  + bundsitze_new[, 5]
) > (round(bundsitze_new[, 8] / 2))) / (mu_nsims * nsims)) * 100
cat("\n CDU-CSU-FDP: \n ~", prob7)

# quantities of interest NEW electoral law 2021
cat("\n The probability that the next Bundestag will have more than 709 seats is: \n")
print((prob_new <- sum(bt_new > 709) / (mu_nsims * nsims)))

cat("\n The median size of the next Bundestag is: \n")
print((med_bt_new <- median(bt_new)))

cat("\n With a 5/6 credible interval between:\n")
print((quant_bt_new <- quantile(bt_new, c(1 / 12, 11 / 12))))

# quantities of interest OLD electoral law before 2021
cat("\n The probability that the next Bundestag will have more than 709 seats is: \n")
print((prob <- sum(bt > 709) / (mu_nsims * nsims)))

cat("\n The median size of the next Bundestag is: \n")
print((med_bt <- median(bt)))

cat("\n With a 5/6 credible interval between:\n")
print((quant_bt <- quantile(bt, c(1 / 12, 11 / 12))))

# comparison new and old electoral law
diff <- bt_new - bt

cat(
  "\n Difference in seats of the next Bundestag according to new compared to old electoral law: \n"
)
print(median(diff))
print((quant_diff <- quantile(diff, c(1 / 12, 11 / 12))))

