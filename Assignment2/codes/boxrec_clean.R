rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(data.table)
library(tidyverse)

df <- read_csv('data/raw/first_csv.csv')

df<- df %>% mutate(
    lagged = NULL,
    age = as.numeric(age),
    points = as.numeric(points),
    rank = as.numeric(rank),
    stars = as.numeric(stars)
    ) %>% 
    separate(w_l_d , sep = "\u00A0", into = c("won","lost",'draw')) %>% 
    separate(residence, sep = ',(?=[^,]+$)', into = c('residence_city','residence_country'), fill = 'left')

df <- df %>% mutate(
    won = as.numeric(won),
    lost = as.numeric(lost),
    draw = as.numeric(draw)
)

write_csv(df,'data/clean/boxers_clean.csv')