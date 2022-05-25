# this code reproduces the figures in the SM part D in 
# Gschwend, M?ller, Munzert, Neunhoeffer, Stoetzer 2021
# 'The Zweitstimme Model: A Dynamic Forecast of the 2021 German Federal Election'

library(tidyr)
library(tidyverse)
library(magrittr)
library(Cairo)

# get information from this file:

btwkr21 <- read.csv2("../raw-data/btwkr21_umrechnung_btw17.csv")
colnames(btwkr21) <- btwkr21[4,]
btwkr21 <- btwkr21[-c(1:5),]
btwkr21$`Wkr-Nr.` <- as.numeric(btwkr21$`Wkr-Nr.`)
btwkr21 <- btwkr21[btwkr21$`Wkr-Nr.` %in% c(1:299),]

# Define colors for each party

parties_col <-
  c(
    rgb(0, 0, 0, 0.8 * 255, maxColorValue = 255),
    rgb(227, 0, 15, 0.8 * 255, maxColorValue = 255),
    rgb(120, 0, 57, 0.8 * 255, maxColorValue = 255),
    rgb(70, 150, 43, 0.8 * 255, maxColorValue = 255),
    rgb(255, 237, 0, 0.8 * 255, maxColorValue = 255),
    rgb(1, 166, 235, 0.8 * 255, maxColorValue = 255)
  )

# load district prediction

res_list <- readRDS("/Users/alec/Desktop/2021_wkr.RDS")
nsim <- 2000
mu_nsim <- 25

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



for(i in 1:299){
  
col <- as.data.frame(matrix(NA, 
                            nrow=length(win_probs[[i]][win_probs[[i]]>0.005]), 
                            ncol=2))

colnames(col) <- c("party", "col")

col$party <- names(win_probs[[i]][win_probs[[i]]>0.005])

col %<>% 
  mutate(col = ifelse(party == "CDU", rgb(0, 0, 0, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "CSU", rgb(0, 0, 0, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "SPD", rgb(227, 0, 15, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "FDP", rgb(255, 237, 0, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "AFD", rgb(1, 166, 235, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "LINKE", rgb(120, 0, 57, 0.8 * 255, maxColorValue = 255), col))%>%
  mutate(col = ifelse(party == "GRUENE", rgb(70, 150, 43, 0.8 * 255, maxColorValue = 255), col))
  
Cairo(
  file = paste0("../figures/wkr_", i, ".png"),
  type = "png",
  units = "in",
  width = 5,
  height = 3,
  pointsize = 20,
  dpi = 96,
)

par(mar=c(0,4,0,5))

pie(win_probs[[i]][win_probs[[i]]>0.005],
    labels = paste0(names(win_probs[[i]][win_probs[[i]]>0.005]),
                    " ", 
                    round(win_probs[[i]][win_probs[[i]]>0.005]*100), "%"),
    col=col$col,
    clockwise=F,
    cex=0.86,
    border=F)

dev.off()
}

