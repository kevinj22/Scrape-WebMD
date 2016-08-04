# Isolating the good doctors by prop of good reviews, then creating a TDM and searching for key terms 

library(tm)
library(RWeka)


for (i in 1:dim(DocDB)[1]){
  if (DocDB$Stars[i] <=3){
    DocDB$GorB[i]='Bad'
  }  else {
    DocDB$GorB[i]='Good'
  }
}

# Check how many bad reviews for each doctor 
# Rough idea to get the proportion of bad reviews 

docnames<-unique(DocDB$Doctor.Name)

prop<-as.numeric()

for (i in 1:length(docnames)){
  temp.bads<-length(grep(0,DocDB$GorB[DocDB$Doctor.Name==docnames[i]]))
  temp.goods<-length(grep(1,DocDB$GorB[DocDB$Doctor.Name==docnames[i]]))
  temp.prop<-(temp.goods)/(temp.bads+temp.goods)
  prop<-c(prop,temp.prop)
}
rm(temp.bads,temp.goods,temp.prop)

docnames<-as.data.frame(docnames)
docnames$Prop<-prop

# Isolate doctors with 95% or higher good reviews 

allgood<-docnames$docnames[docnames$Prop>=0.95]
allgood

# Count each doctors total amount of reviews 

length(which(DocDB$Doctor.Name==allgood[1]))

relative.lengths<-as.numeric()
for (i in 1:length(allgood)){
  temp.lengths<-length(which(DocDB$Doctor.Name==allgood[i]))
  relative.lengths<-c(relative.lengths,temp.lengths)
}

relative.lengths
allgooddb<-as.data.frame(allgood)
allgooddb$relative.lengths<-relative.lengths
allgooddb<-allgooddb[which(relative.lengths>10),]

allgoodreviews<-data.frame(Doctor.Name=as.character(), Stars=as.numeric(), Reviews=as.character())

for (i in 1:dim(allgooddb)[1]){
  temp.reviews<-DocDB[DocDB$Doctor.Name==allgooddb$allgood[i],c(6,5,7)]
  allgoodreviews<-rbind(allgoodreviews,temp.reviews)
}
rm(temp.reviews)

# Make a Corpus to use

goodCorp<-Corpus(VectorSource(allgoodreviews$Reviews))
goodCorp<-tm_map(goodCorp, content_transformer(tolower))
goodCorp<-tm_map(goodCorp, removePunctuation)
goodCorp<-tm_map(goodCorp,removeNumbers)
goodCorp<-tm_map(goodCorp, removeWords, stopwords('english'))
#testCorp<-tm_map(testCorp,stemDocument)
#testCorp<-tm_map(testCorp, content_transformer(stemCompletion),dictionary=dictCorp,lazy=TRUE)
goodCorp<-tm_map(goodCorp, PlainTextDocument)

# Make a word freq matrix


dtm <- DocumentTermMatrix(goodCorp)
#tdm<-TermDocumentMatrix(goodCorp)
#rm(testCorp)
dtm<-as.matrix(dtm)
word.freq<-colSums(dtm)
word.freq<-sort(word.freq,decreasing=T)
word.freq<-data.frame(word=names(word.freq),frequency=word.freq)
rownames(word.freq)<-NULL

# Other DTM / TDM

tdm <- TermDocumentMatrix(goodCorp, control = list(tokenize = NGramTokenizer, Weka_control(min=1,max=3)))
dtm<- DocumentTermMatrix(goodCorp, control = list(tokenize = NGramTokenizer, Weka_control(min=1,max=3)))


# Convert DTM to matrix, then transpose it to make the term document matrix to get terms as row names
# Or do the TDM directly to matrix

dtm<-as.matrix(dtm)
dtm<-t(dtm)

tdm.mat<-as.matrix(tdm)

# Start looking for documents containing tummy tuck and other important words

which(row.names(tdm.mat)=='tummy tuck')
tucks<-which(tdm.mat[95997,]!=0)
names(tucks)<-NULL
tucks<-allgoodreviews[tucks,]

# Try on more words 

test.words<-c('tummy tuck','weight','weight loss','pain','complication','complications','post op')
searched.reviews<-data.frame(Doctor.Name=as.character(), Stars=as.numeric(), Reviews=as.character())

for (i in 1:length(test.words)){
  temp.locations<-which(tdm.mat[which(row.names(tdm.mat)==test.words[i]),]!=0)
  names(temp.locations)<-NULL
  temp.reviews<-allgoodreviews[temp.locations,]
  searched.reviews<-rbind(searched.reviews,temp.reviews)
}
rm(temp.locations)
rm(temp.reviews)

View(searched.reviews)

# Random stuff

#> length(which(tucks$Doctor.Name=='Dr.Martin.Jugenburg'))
#[1] 51
#> View(allgoodreviews)
#> length(which(allgoodreviews$Doctor.Name=='Dr.Jerome.Edelstein'))
#[1] 179
#> length(which(tucks$Doctor.Name=='Dr.Jerome.Edelstein'))
#[1] 13
#> length(which(tucks$Doctor.Name=='Dr.Robert.Backstein'))
#[1] 33

# Rough stuff 
inspect(dtm[10,1])
findAssocs(tdm,'tummy',0.6)