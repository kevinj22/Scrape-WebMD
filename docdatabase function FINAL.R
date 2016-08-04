# Gathers all review stats from RateMD


library(rvest)
library(plyr)
library(dplyr)

docdatabase.nest<- function (links){
  
  # Open session, get review stats
  # Create intial data base as NULL
  # If the data is not gathered, set as NULL and moves on to next page
  
  createDB<-function(links){
    
    docdb<-NULL
    
    review.stats<-html_session(links) %>%
      html_nodes('div.row.rating-numbers-compact') %>%
      html_text()
    
    # Fail safe to check if the data was gathered properly to prevent loss of connection / website loading problems
    # While loop with break after 5 runs
    
    if (length(review.stats)==0){
      w=0
      while(length(review.stats==0)){
        w=w+1
        stats<-read_html(links) 
        review.stats<- stats %>%
          html_nodes('div.row.rating-numbers-compact') %>%
          html_text()
        if (w==5) {
          break
        } }
    } else {
      # Building the database once all other conditions have been met
      # Str split the stats up to identify the ratings
      # Whenever there is a blank rating [this creates a problem [^0-9]+ creates a problem
      # Use [^0-9 ]+ instead with gsub to remove the spaces
      
      temp.review<-strsplit(review.stats, '[^0-9 ]+')
      
      # Get the Doctor Name
      
      Doctor.Name<-paste(strsplit(links,'[/,+,-]')[[1]][c(7,8,9)],collapse = '.')
      
      # Create empty vectors for storage
      
      staff<-as.vector(as.numeric())
      punct<-as.vector(as.numeric())
      helpfull<-as.vector(as.numeric())
      knowledge<-as.vector(as.numeric())
      
      for (k in 1:length(temp.review)) {
        staff[k]<-as.numeric(temp.review[[k]][1])
        punct[k]<-as.numeric(temp.review[[k]][2])
        helpfull[k]<-as.numeric(temp.review[[k]][3])
        knowledge[k]<-as.numeric(temp.review[[k]][4])
      }
      
      # Put review stats in docdb
      
      docdb<-data.frame(staff,punct,helpfull,knowledge)
      
      # Calculate row mean for the overall star rating
      
      docdb$Stars<-rowMeans(docdb[-1],na.rm=T)
      
      # Add Doctor Name last so rowMeans operates on only numbers 
      
      docdb$Doctor.Name<-Doctor.Name
      
    }
    return(docdb)
  }
  
  # Check if the DB created is null if so run the function again
  
  DB<-createDB(links)
    
    if (is.null(DB)==TRUE){
      l=0
      while(is.null(DB)==TRUE){
        l=l+1
        DB<-createDB(links)
        if (l==3){
          break
        }
      }
    }
  return(DB)
}