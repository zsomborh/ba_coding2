rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Get packages ------------------------------------------------------------


library(data.table)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(RSelenium)
library(pingr)
library(binman)
library(dplyr)


# Define functions to scrape data and create URLs -------------------------


get_boxer_urls <- function(rank = 3) {
    domain <- 'https://boxrec.com/'
    language <- 'en/'
    subdirectory <- 'ratings'
    query <- '?offset='
    step <- 50 #only 50 rows are shown for table
    rank <- (1:rank-1)*step
    return(paste0(domain,language, subdirectory,query,rank))
}



# navigate driver to custom page and read html
navigate_driver <- function(url, client){
    remDr1 <- client
    remDr1$navigate(url)
    content <-read_html(remDr1$getPageSource()[[1]])
    Sys.sleep(40) # Don't overload the server be idle for 40 secs
    return(content)
}

# Scrape ranking table 

get_boxer_data <-function(content) {

    #Get table with most data
    table <- content %>% html_table(fill = TRUE)
    table <- table[[2]]
    table<- clean_names(table)
    table <- 
        table  %>%  mutate(
        rank = x,
        x = NULL,
        x_2 = NULL, 
        last_6 = NULL
    )
    
    table <- table%>% filter(!is.na(as.numeric(points)))
    
    
    # Get the number of stars for a given boxer
    boxes <- content %>% html_nodes('#ratingsResults div span')
    star_list <- lapply(boxes, function(x){
        x %>% html_children() %>%  html_attr('class')
        })
    
    stars <- lapply(star_list,str_count,pattern = 'fas fa-star star-icon -star')
    half_stars <- lapply(star_list,str_count,pattern = 'fas fa-star-half star-icon -star')
    stars <- 
        unlist(lapply(stars,sum),use.names = FALSE) + 
        unlist(lapply(half_stars,sum),use.names = FALSE) *0.5
    
    table$stars <- stars
    
    # Get last six match
    boxes <- content %>% html_nodes('#ratingsResults tr :nth-child(8)') 
    
    lastsix <- lapply(boxes[2:length(boxes)], function(x){
        my_list <- unlist(x %>% html_children() %>% html_children() %>% html_children() %>% html_attr('class'))
        my_list <- grep('last6',my_list, value = TRUE)
        my_list <- str_remove(my_list, 'last6 bg')
        return(my_list)
    })
    
    lastsix_df<- do.call(rbind,lastsix)
    colnames(lastsix_df) <- c('last6','last5','last4', 'last3', 'last2', 'last1')
    
    final_df <- cbind(table,lastsix_df)
    
    return(final_df)
}



# Configuration for Rselenium ----------------------------------------------


# Set routes for webbrowser for Selenium

chromeversion <- toString(dir(path = 'C:/Program Files (x86)/Google/Chrome/Application/')) %>% 
    str_extract(., '^\\d+\\.\\d+\\.\\d+\\.') %>% 
    str_replace_all(., '\\.', '\\\\.')  

chromedriver <-  str_extract_all(toString(list_versions("chromedriver")), paste0(chromeversion, '\\d+'), simplify = TRUE) %>% 
    as.numeric_version(.) %>% 
    min(.)

# OPTIONAL: kill port if it runs a selenium browser instance

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
pingr::ping_port("localhost", 4567) #check if port is ready to use // only if I receive three NAs

# Initiate webbrowser in port 4567
rD1 <- rsDriver(browser = "chrome", port = 4567L, geckover = NULL, 
                chromever =  toString(chromedriver), iedrver = NULL, 
                phantomver = NULL, verbose = TRUE)
remDr1 <- rD1[["client"]]

# I need to log in with user, otherwise the site blocks the content after a few pages are downloaded
login = 'https://boxrec.com/en/login'
remDr1$navigate(login)


# Scraping the ranking tables ---------------------------------------------

urls<- get_boxer_urls(200) # I will take the first 200 pages and get data on 10k boxers this way

# Iterate through URLS, open them up in Selenium, and get the html content and scrape the table. 
# After that wait 40 s and finally add the 50 observation/page into df.
# I used for loops so that every time the loop breaks, the data that is stored until the break is 
# still available

df<- data.frame()

for (i in urls){
    content <- navigate_driver(i, remDr1)
    tdf <- get_boxer_data(content)
    df <- rbind(df,tdf)
}

df2<- df %>% unique() %>% filter(!duplicated(rank))

# write data to disk for further analysis
write_csv(df2,'data/raw/first_csv.csv')