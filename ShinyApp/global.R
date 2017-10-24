library(shinydashboard)
library(shiny)
library(DT)
library(data.table)
library(leaflet)
library(ggplot2)
library(reshape2)
library(tm)
library(wordcloud)
library(memoise)
library(googleVis)


load("bikes2_dt.rda")
load("census_perc.rda")

# Using "memoise" to automatically cache the results

getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  text <- bikes2_dt[is.na(Incident) == FALSE, Incident]

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    stopwords("SMART"))
  myCorpus = tm_map(myCorpus, removeWords, c("bike", "stolen", "stole", "bikes", "bicycle", "bicycles", "lock",
                                             "locked"))

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})


