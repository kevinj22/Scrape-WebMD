# Gather Links from main page of RateMD category i.e https://www.ratemds.com/best-doctors/on/toronto/cosmetic-plastic-surgeon/

library(rvest)
library(plyr)
library(dplyr)

# Opens the main webpage then follows the 

gatherlinks<- function(session,links) {
  temp.page<-html_session(session) %>%
    jump_to(links) %>%
    html_nodes('.search-item-doctor-link') %>%
    html_attr('href')
  return(temp.page)
}