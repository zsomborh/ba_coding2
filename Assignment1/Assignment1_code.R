library(data.table)
library(rvest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

get_portfolio_urls <- function(keyword,num = 5) {
    domain <- 'https://www.portfolio.hu/'
    subdirectory <- 'kereses'
    query <- '?q='
    search_term <- URLencode(keyword)
    page <- 'page='
    return(paste0(domain,subdirectory,query,search_term,'&',page,seq(1,num)))
}

get_boxes <- function(url) {
    t<- read_html(url)
    boxes <-
        t %>% 
        html_nodes('.article-lists article')
    return(boxes)
}

get_portfolio_df <- function(keyword,num) {

    links <- get_portfolio_urls(keyword,num)
    
    boxes<- lapply(links,get_boxes)    
    
    boxes_df<- lapply(boxes, function(x) {
        t_list <- list()
        t_list[['time']] <- x %>%  html_nodes('time') %>% html_attr('datetime')
        t_list[['author']] <- x %>% html_nodes('time') %>%  html_nodes('.ml-2') %>% html_text()
        t_list[['title']] <- x %>% html_nodes('.row')%>% html_nodes('a') %>% html_text()
        t_list[['title']] <- t_list[['title']][t_list[['title']]!=""]
        t_list[['link']] <- unique(x %>% html_nodes('.row')%>% html_nodes('a') %>% html_attr('href'))
        return(data.frame(t_list))  
    })

    df<- rbindlist(boxes_df)
    return(df)
    
}


df<- get_portfolio_df('big data',20)
