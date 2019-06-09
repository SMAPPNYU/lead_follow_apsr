#===============================================================================
#  File:    04-output-data-for-reg.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: extract topic probabilities from LDA output and create dataset
#           for main analysis
#  Data In: 
#           ./data/tweets/text/* & var/lda_results-twokenizer.Rdata & 
#           var/lda-ouput/*
#  Data Out: 
#           data/main-time-series.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(topicmodels)
library(reshape)
K <- 100

# MAIN
#===============================================================================

## MEMBERS OF CONGRESS

load("topics/lda_results-twokenizer.Rdata")

## getting list of dates
fls <- scan("data/dfm/fls-list.txt", what="character", sep="\n")
dates <- substr(fls, 1, 10)
dates <- gsub('_', '-', dates)
party <- substr(fls, 12, 14)
chamber <- gsub('.*_(.*)\\.txt','\\1',fls)

## computing differences across parties
time.series <- t(lda.fit@gamma)
df <- data.frame(chamber = rep(chamber, each=K), 
    date = rep(as.Date(dates), each=K),
    party = rep(party, each=K),
    topic = rep(1:K, 2920), prop=c(time.series),
    stringsAsFactors=F)
df <- aggregate(df$prop, by=list(date=df$date, topic=df$topic, party=df$party),
    FUN=mean)

df <- as.data.frame(cast(df, date + topic ~ party))

## PUBLIC
users <- scan("data/dfm/users-list.txt", what='character')
load("topics/lda-output/lda-public-results.Rdata")
dd <- substr(users[grep('_public', users)], 1, 10)
pub <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  public = c(results$topics), stringsAsFactors=F)

df <- merge(df, pub, all.x=TRUE)

# public (democrats)
dd <- substr(users[grep('_democr', users)], 1, 10)
load("topics/lda-output/lda-dems-results.Rdata")
dem <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  pubdem = c(results$topics), stringsAsFactors=F)

df <- merge(df, dem, all.x=TRUE)

# public (republicans)
dd <- substr(users[grep('_repub', users)], 1, 10)
load("topics/lda-output/lda-reps-results.Rdata")
rep <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  pubrep = c(results$topics), stringsAsFactors=F)

df <- merge(df, rep, all.x=TRUE)

load("topics/lda-output/lda-media-results.Rdata")
media <- data.frame(
  date = rep(as.Date(dd[1:729]), K),
  topic = rep(1:K, each=nrow(results$topics)),
  media = c(results$topics), stringsAsFactors=F)

df <- merge(df, media, all.x=TRUE)

load("topics/lda-output/lda-USrs-results.Rdata")
dd <- seq(as.Date("2013-01-01"), as.Date("2014-12-31"), by="day")
rnd <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  random = c(results$topics), stringsAsFactors=F)

df <- merge(df, rnd, all.x=TRUE)


## save to disk
write.csv(df, file="data/main-time-series-PRE.csv", row.names=FALSE)

