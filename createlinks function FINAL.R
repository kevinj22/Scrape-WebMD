# Function to generate all page links for invidiual doctors

library(rvest)
library(plyr)
library(dplyr)

createlinks <- function(webpage){
  
  # Open webpage and gather the page numbers 
  
  page<-read_html(webpage) 
  pgs<- page %>% html_nodes(xpath='//nav') %>% html_text()
  
  # Check that the pg numbers were gathered, if the length of pgs is less than 3 the webpage is only a single page
  # Thus generate the link as the initial page
  
  if ((length(pgs)!=0) & (length(pgs)<3)){
    indexlinks<-as.character(webpage)
    maxpg=0
  
  # If the pgs were gathered and the length is 3 or greater the final page number is always in the 3rd element of pgs
    
  } else if ((length(pgs)!=0) & (length(pgs)>=3)) {
    
    # Failsafe check to see if the pg number is blank, doctors with only a few pages have '' in pgs[3]
    # This converts the '' to NA as if statements in R prefer to check for NA vs ''
    
    pgs[3]<-sapply(pgs[3], function(f){is.na(f)<-which(f == '');f}) 
    
    # If the pg max slot is NA 
    
    if (is.na(pgs[3])){
      indexlinks<-as.character(webpage)
      maxpg=0
      
    # in the event the doctor has many pages split the string and collect the max pg
      
    } else { 
      maxpg<- as.numeric(strsplit(pgs[3],'[.]')[[1]][4])
    } 
    
    # Check if the maxpg is NA, if it is then need to split a seperate section of pgs to get the max page
    
    if (is.na(maxpg)==TRUE){
      maxpg<- as.numeric(strsplit(pgs[length(pgs)],'')[[1]][length(strsplit(pgs[length(pgs)],'')[[1]])])
    } 
    
  # Failsafe to check if pgs was scraped properly, if not reopen the page and try again
    
  } else if (length(pgs)==0){
    i=0
    while (length(pgs)==0){
      i=i+1
      page<-read_html(webpage) 
      pgs<- page %>% html_nodes(xpath='//nav') %>% html_text()
      if (i==5){
        break
      }
    }
  }
  
  # If maxpg is not 0 then create ?page to fill in all links
  
  if (maxpg !=0){
    
    indexlinks<- rep(webpage,maxpg)
    pagenums<-rep('?page=',maxpg-1)
    pagenums<-paste(pagenums,seq(2,maxpg),sep='')
    
    for (i in 1:length(pagenums)){
      indexlinks[i+1]<-paste(indexlinks[i+1],pagenums[i],sep='')
    }
  }
  return(indexlinks)
  
}

