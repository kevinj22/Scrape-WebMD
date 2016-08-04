# Good copy of Script to Scrape RateMD

library(rvest)
library(plyr)
library(dplyr)

# Gathering max pages and index of all dr links

# Define main page 

mainpage<-'https://www.ratemds.com/best-doctors/on/toronto/cosmetic-plastic-surgeon/'
main<-read_html(mainpage)

# Determine max number of pages to cycle through
# Get number of pages

pgs <- main %>% html_nodes(xpath='//nav') %>% html_text()

# Split string to get max page number

maxpgs<-as.numeric(strsplit(pgs,'[.]')[[2]][4])

# Create a list of links to paste page number too

indexlinks<- rep(mainpage,maxpgs)
pagenums<-rep('?page=',maxpgs-1)
pagenums<-paste(pagenums,seq(2,maxpgs),sep='')

# Paste to links to create the final index links

for (i in 1:length(pagenums)){
  indexlinks[i+1]<-paste(indexlinks[i+1],pagenums[i],sep='')
}

# Lets gather all individual doctors links from the indexlinks index
# Use the gather links function to do this 

drlinksindex<-as.vector(0)
for (i in 1:length(indexlinks)){
  temp.links<-gatherlinks('https://www.ratemds.com/best-doctors/on/toronto/cosmetic-plastic-surgeon/',indexlinks[i])
  drlinksindex<-c(drlinksindex,temp.links)
}
rm(temp.links)
drlinksindex<-drlinksindex[-1] # remove the initial 0

# Take the unique entries of drlinks index to remove repeats (sometimes a doctor is listed multiple times on the site)

drlinksindex<-unique(drlinksindex)

# With all the doctor links let's visit each page and create all of their individual links 
# Can use the create links function to do this 

pages<-llply(drlinksindex,createlinks,.progress='text')
pages<-unlist(docdb)

# Now that we have all links to every doctor's pages we can work through them and mine the review stats then mine the review text
# Review stats first
# Use the docdatabase function to do this
# Will complete this in parts to avoid losing internet connection / progress

# May 11 DB1 and DB2 don't match the amount of text reviews
# Refer to trbl shooting difference in review stats / review text for method

# May 12 DB2 matches the amount of text reviews after editing docdatabase to contain a nested function
# For further fail safe checking if the site wasn't mined 
# Fairly slow compared to backup of docdatabase before NESTED but produces consistent results 

DB1<-llply(pages[c(1:200)],docdatabase.nest,.progress='text')
DB1<-as.data.frame(do.call(rbind,DB1))

DB2<-llply(pages[c(201:400)],docdatabase.nest,.progress='text')
DB2<-as.data.frame(do.call(rbind,DB2))

DB3<-llply(pages[c(401:600)],docdatabase.nest,.progress='text')
DB3<-as.data.frame(do.call(rbind,DB3))

DB4<-llply(pages[c(601:786)],docdatabase.nest,.progress = 'text')
DB4<-as.data.frame(do.call(rbind,DB4))

# Lets get the review text, do it 200 at a time 

rtext<-as.character()
for (i in 1:200){
  print(i)
  temp.gather<-massgather(pages[i])
  rtext<-c(rtext,temp.gather)
}

rtext2<-as.character()
for (i in 201:400){
  print(i)
  temp.gather<-massgather(pages[i])
  rtext2<-c(rtext2,temp.gather)
}

rtext3<-as.character()
for (i in 401:600){
  print(i)
  temp.gather<-massgather(pages[i])
  rtext3<-c(rtext3,temp.gather)
}

rtext4<-as.character()
for (i in 601:786){
  print(i)
  temp.gather<-massgather(pages[i])
  rtext4<-c(rtext4,temp.gather)
}

# Bind reviews to corresponding databases of review stats

DB1$Reviews<-rtext
DB2$Reviews<-rtext2
DB3$Reviews<-rtext3
DB4$Reviews<-rtext4

# Bind DocDB together

DocDB<-rbind(DB1,DB2,DB3,DB4)

# Determine whether a review is good or bad and add to the data frame 
# 1 is good, 0 is bad

for (i in 1:dim(DocDB)[1]){
  if (DocDB$Stars[i] <=3){
    DocDB$GorB[i]=0
  }  else {
    DocDB$GorB[i]=1
  }
}

DocDB$GorB<-as.numeric(DocDB$GorB)

# From here go to good or bad review and isolating script

