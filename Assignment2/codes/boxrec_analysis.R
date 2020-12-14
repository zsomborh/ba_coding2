
# Cean workspace, call libraries, import df -------------------------------



rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(data.table)
library(tidyverse)
library(moments)
library(xtable)
library(ggpubr)
library(estimatr)
library(texreg)

df <- read_csv('data/clean/boxers_clean.csv')


# create summary table ----------------------------------------------------


points_sum <- df %>% summarise(
    variable = 'Points',
    mean     = mean(points),
    median   = median(points),
    std      = sd(points),
    iq_range = IQR(points), 
    min      = min(points),
    max      = max(points),
    skew     = skewness(points),
    numObs   = sum( !is.na( points ) ) )

age_sum <- df %>% summarise(
    variable = 'Age',
    mean     = mean(age),
    median   = median(age),
    std      = sd(age),
    iq_range = IQR(age, na.rm= TRUE), 
    min      = min(age),
    max      = max(age),
    skew     = skewness(age),
    numObs   = sum( !is.na( age ) ) )

stars_sum <- df %>% summarise(
    variable = 'Stars',
    mean     = mean(stars),
    median   = median(stars),
    std      = sd(stars),
    iq_range = IQR(stars, na.rm=TRUE), 
    min      = min(stars),
    max      = max(stars),
    skew     = skewness(stars),
    numObs   = sum( !is.na( stars ) ) )


df_summary <- points_sum %>% add_row( stars_sum ) %>% add_row(age_sum)
xtb <- xtable(df_summary,type = "latex", caption = "Summary statistics of examined variables")


# plots for descriptive stats ---------------------------------------------



p1 <- ggplot( df[df$points < 350 ,] , aes( x = points ) ) +
    geom_histogram( alpha = 1, binwidth = 5, color = 'black', fill = 'lightblue') +
    xlim(c(0,200))+
    ylim(c(0,350)) +
    xlab('Distribution of points under 350 points')

p2<- ggplot( df[df$points > 350,] , aes( x = points ) ) +
    geom_histogram( alpha = 1, binwidth = 100, color = 'black', fill = 'blue') +
    xlab('Distribution of points above 350 points')

p3<- ggplot( df , aes( x = age ) ) +
    geom_histogram( alpha = 1, binwidth = 5, color = 'black', fill = 'navyblue') +
    xlab('Age distribution')

p4<- ggplot( df , aes( x = division ) ) +
    geom_bar( alpha = 1, color = 'black', fill = 'lightgreen') + coord_flip()+
    xlab('Division breakdown')

p5 <- ggplot( df , aes( x = stars ) ) +
    geom_histogram( alpha = 1, binwidth = 0.5, color = 'black', fill = 'green') +
    xlab('Star breakdown')

p6<- ggplot( df , aes( x = stance ) ) +
    geom_bar( alpha = 1, color = 'black', fill = 'darkgreen') + coord_flip() +
    xlab('Stance breakdown')


ggarrange(p1, p2, p3,p4,p5,p6, nrow = 2, ncol = 3 )

df <- df %>%  mutate(
    ln_points =  log(points),
    age_sq = age**2,
    win_rate = won/(won+draw+lost),
    ln_win_rate = log(win_rate)
)


# plot to see patterns ----------------------------------------------------



ggplot(df, aes(x=age,y=ln_points, color = stance )) + geom_point() + facet_wrap(~ division)

ggplot(df, aes(x=win_rate,y=ln_points, color = stance)) + geom_point() + facet_wrap(~ stance)

ggplot( data = df, aes( x = win_rate, y = ln_points ) ) + 
    geom_point( color='blue') +
    geom_smooth( method = lm , color = 'red' ) +
    labs(x='Win rate',y='log points')


# linear regression results -----------------------------------------------



reg1 <- lm_robust(ln_points ~ win_rate , data = df, se_type = 'HC2')
reg2 <- lm_robust(ln_points ~ win_rate + age , data = df, se_type = 'HC2')
reg3 <- lm_robust(ln_points ~ win_rate +age + age_sq, data = df, se_type = 'HC2')


summary<- htmlreg( list(reg1 , reg2 , reg3 ),
                   type = 'html',
                   custom.model.names = c("ln_points - ln_win_rate","ln_points - ln_win_rate+age",
                                          "ln_points - ln_win_rate+age(quadratic)"),
                   caption = "Average association between points and win rate")

