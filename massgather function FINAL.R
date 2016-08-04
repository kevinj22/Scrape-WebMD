# Now need a function to go through the doctor links and gather each page
# A mass gather function to reopen the session and take another page
# Added an if statement as sometimes the page doesn't load / scrape correctly the first time

library(rvest)
library(plyr)
library(dplyr)

massgather<-function (links){
  temp.ses<-html_session(links) %>%
    html_nodes('div.rating-comment > p.rating-comment-body') %>%
    html_text()
  if (length(temp.ses)==0){
    i=0
    while(length(temp.ses)==0){
      i=i+1
      temp.ses<-html_session(links) %>%
        html_nodes('div.rating-comment > p.rating-comment-body') %>%
        html_text()
      if (i==5){
        temp.ses=NULL
        break
      }
    }
  }

  return(temp.ses) }

