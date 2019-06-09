#===============================================================================
#  File-Name:	01-generating-summaries.R
#  Date:	May 6, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: generate data for html pages in dashboard
#  Data In:
#           ./data/tweets/text/* & var/lda_results-twokenizer.Rdata & 
#           var/lda-ouput/*
#===============================================================================


# PACKAGES
#===============================================================================
library(topicmodels)
library(reshape)
library(ggplot2)
library(gtable)
library(gridExtra)
library(scales)
library(ggdendro)
library(ggthemes)
library(slam)
library(Matrix)
library(tm)


# DATA
#===============================================================================
load("topics/lda_results-twokenizer.Rdata")


###############################################################################
### A) Computing top 10 words for each topic
###############################################################################

K <- 100
num.words <- 15
normalized.topics <- exp(lda.fit@beta) / rowSums(exp(lda.fit@beta))

calculate.specificity <- function(mod) {
  if(!inherits(mod,"LDA") & !inherits(mod,"CTM") ) stop("mod object must inherit from LDA or CTM")
  terms <- posterior(mod)$terms
  topics <- posterior(mod)$topics
  Nwords<-ncol(terms)
  Ntopics<-ncol(topics)
  Ndocs<-nrow(topics)
  ptopic <- apply(topics,2,sum)/Ndocs
  pwords <- apply(terms,2,function(x) sum(x*ptopic))
  numer <- terms*ptopic
  denom  <- matrix(pwords,nrow=Ntopics,ncol=Nwords,byrow=TRUE)
  return(numer/denom)
}

normalized.words <- calculate.specificity(lda.fit)

scores <- apply(normalized.topics, 2, function(x) 
    x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) )
colnames(scores) <- lda.fit@terms
words <- apply(scores, 1, function(x) 
        colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])
f.scores <- apply(scores, 1, function(x) 
        x[order(x, decreasing = TRUE)[1:num.words]])
n.topics <- rep(seq(1, K, 1), each=num.words)
info.df <- data.frame(
    topic = n.topics,
    word = c(words),
    score = c(f.scores),
    stringsAsFactors=F)

info.df$specificity <- NA
for (i in 1:length(info.df$topic)){
    info.df$specificity[i] <- normalized.words[info.df$topic[i], which(colnames(scores) %in% info.df$word[i])]
}
info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))
info.df$order <- factor(order.topics <- rep(seq(1, num.words, 1), times=K), levels=as.character(15:1))


# generating figures
for (k in 1:K){

	cat(k, '\n')

	df <- info.df[info.df$topic==paste0('Topic ', k),]

	p <- ggplot(data=df, 
		aes(y=order, x=specificity, label=word))
	pq <- p + geom_text(aes(size=score), hjust=1) + 
    	scale_size_continuous(range=c(3,5)) + 
    	scale_y_discrete("Top 10 scoring words for each topic",
        	expand=c(0.03, 0.1)) +
    	scale_x_continuous("Specificity of word to each topic",
        	limit=c(min(df$specificity)-.75, 1)) + 
    	theme_bw() +
    	theme(axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	axis.text = element_blank(),
    	axis.ticks = element_blank(),
    	axis.title.y = element_text(size = rel(.6)),
    	axis.title.x = element_text(size = rel(.6)),
    	legend.position = "none") +
    	geom_vline(xintercept = min(df$specificity)-.75, color="grey80", linetype=7) +
    	geom_hline(yintercept = 0.5, color="grey80", linetype=7)

	pq

	ggsave(pq, file=paste0("dashboard/files/img/words-plot-", k, '.png'), height=3.5, width=3)

}

###############################################################################
### B) Computing time series for each topic
###############################################################################

## getting list of dates
fls <- scan("data/dfm/fls-list.txt", what="character", sep="\n")
dates <- substr(fls, 1, 10)
dates <- gsub('_', '-', dates)
party <- substr(fls, 12, 14)
chamber <- gsub('.*_(.*)','\\1',fls)

## computing differences across parties
time.series <- t(lda.fit@gamma)
df <- data.frame(chamber = rep(chamber, each=100), 
    date = rep(as.Date(dates), each=100),
    party = rep(party, each=100),
    topic = rep(1:100, 2920), prop=c(time.series),
    stringsAsFactors=F)

load("topics/lda-output/lda-media-results.Rdata")
dd <- seq(as.Date("2013-01-01"), as.Date("2014-12-30"), by="day")
media <- data.frame(
  topic = rep(1:100, each=729), 
  date = rep(dd, 100),
  prop = c(results$topics),
  stringsAsFactors=F)

## public
users <- scan("data/dfm/users-list.txt", what='character')
load("topics/lda-output/lda-public-results.Rdata")
dd <- substr(users[grep('_public', users)], 1, 10)
pub <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  prop = c(results$topics), stringsAsFactors=F)

# public (democrats)
dd <- substr(users[grep('_democr', users)], 1, 10)
load("topics/lda-output/lda-dems-results.Rdata")
dem <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  prop = c(results$topics), stringsAsFactors=F)

# public (republicans)
dd <- substr(users[grep('_repub', users)], 1, 10)
load("topics/lda-output/lda-reps-results.Rdata")
rep <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  prop = c(results$topics), stringsAsFactors=F)

# random users
dd <- seq(as.Date("2013-01-01"), as.Date("2014-12-31"), by="day")
load("topics/lda-output/lda-USrs-results.Rdata")
rnd <- data.frame(
  date = rep(as.Date(dd), K),
  topic = rep(1:K, each=nrow(results$topics)),
  prop = c(results$topics), stringsAsFactors=F)

## random users
for (k in 1:K){

  # parties
	sbs <- df[df$topic==k,]
	sbs <- aggregate(sbs$prop, by=list(date=sbs$date, party=sbs$party), FUN=mean)
	# media
  mm <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
    media[media$topic==k, c("date", "prop")], all.x=TRUE)
  mm$prop[is.na(mm$prop)] <- 0

  # public
  pp <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
    pub[pub$topic==k, c("date", "prop")], all.x=TRUE)
  pp$prop[is.na(pp$prop)] <- 0
  dd <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
    dem[dem$topic==k, c("date", "prop")], all.x=TRUE)
  dd$prop[is.na(dd$prop)] <- 0
  rr <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
    rep[rep$topic==k, c("date", "prop")], all.x=TRUE)
  rr$prop[is.na(rr$prop)] <- 0

  ## random users
  rnn <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
    rnd[rnd$topic==k, c("date", "prop")], all.x=TRUE)
  rnn$prop[is.na(rnn$prop)] <- 0

  sbs <- data.frame(
		date = unique(sbs$date),
		Democrats = round(sbs$x[sbs$party=="dem"], 3),
		Republicans = round(sbs$x[sbs$party=="rep"], 3),
    Media = round(mm$prop, 3),
    Public = round(pp$prop, 3),
    Democratic_Supporters = round(dd$prop, 3),
    Republican_Supporters = round(rr$prop, 3),
    Random_Users= round(rnn$prop, 3))

	write.csv(sbs, file=paste0("dashboard/files/data/ts-", k, '.csv'), 
		row.names=FALSE, quote=FALSE)

}

###############################################################################
### C) Computing quantities of interest
###############################################################################

qois <- data.frame(
  topic = 1:K,
  prop = NA,
  prop_sen_dems = NA,
  prop_sen_reps = NA,
  prop_house_dems = NA,
  prop_house_reps = NA,
  media = NA,
  public = NA,
  republicans = NA,
  democrats = NA,
  random = NA)

# topic proportions
load("topics/lda_results-twokenizer.Rdata")
qois$prop <- apply(lda.fit@gamma, 2, mean)*100

# by party and chamber
df <- data.frame(chamber = rep(chamber, each=100), 
    date = rep(as.Date(dates), each=100),
    party = rep(party, each=100),
    topic = rep(1:100, 2920), prop=c(time.series),
    stringsAsFactors=F)
agg <- aggregate(df$prop, by=list(topic=df$topic, party=df$party, chamber=df$chamber), FUN=mean)

qois$prop_sen_dems <- agg$x[agg$party=="dem" & agg$chamber=="senate"]*100
qois$prop_sen_reps <- agg$x[agg$party=="rep" & agg$chamber=="senate"]*100
qois$prop_house_dems <- agg$x[agg$party=="dem" & agg$chamber=="house"]*100
qois$prop_house_reps <- agg$x[agg$party=="rep" & agg$chamber=="house"]*100

load("topics/lda-output/lda-media-results.Rdata")
qois$media <- sapply(1:K, function(x) mean(results$topics[,x]))*100

# public
load("topics/lda-output/lda-public-results.Rdata")
qois$public <- sapply(1:K, function(x) mean(results$topics[,x]))*100
load("topics/lda-output/lda-dems-results.Rdata")
qois$democrats <- sapply(1:K, function(x) mean(results$topics[,x]))*100
load("topics/lda-output/lda-reps-results.Rdata")
qois$republicans <- sapply(1:K, function(x) mean(results$topics[,x]))*100
load("topics/lda-output/lda-USrs-results.Rdata")
qois$random <- sapply(1:K, function(x) mean(results$topics[,x]))*100

# saving to disk
save(qois, file="dashboard/qois.rdata")

###############################################################################
### D) Representative tweets
###############################################################################

library(slam)
library(Matrix)
library(tm)

ind <- scan("data/dfm/rs-dtm-indices.txt")
pointers <- scan("data/dfm/rs-dtm-pointers.txt")
values <- scan("data/dfm/rs-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
tweets <- read.csv("data/dfm/tweets-random-sample.csv", stringsAsFactors=F, colClasses="character")

X <- sparseMatrix(j=ind, p=pointers, x=values,
  dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

load('topics/lda-output/lda-rs-results.Rdata')

# deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results$topics <- results$topics[-duplicated,]

# deleting weird tweets
todelete <- grep('xss', tweets$text)
tweets <- tweets[-todelete,]
results$topics <- results$topics[-todelete,]

K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=6)
  rs[[k]] <- tweets[choices,]
  rs[[k]]$topic <- k
}
rs <- do.call(rbind, rs)

# function to display embedded tweet
tw.embed <- function(text, name, screen_name, id_str, created_at, dt, js=FALSE){
    txt <- paste0('<blockquote class="twitter-tweet" data-cards="hidden" data-conversation="none" width="450"><p>',
        text, '</p> ', name, " (@", screen_name, 
        ") <a href='https://twitter.com/", screen_name,
        '/status/', id_str, "'>",
        dt, '</a></blockquote>')
    if (js){
        txt <- paste0(txt, 
            ' <script src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    }
    return(txt)
}

# preparing embed
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], rs$screen_name[i], rs$screen_name[i], rs$id_str[i],
    "", "")

}

save(rs, file="dashboard/rs-tweets.rdata")


###############################################################################
### D) Representative media tweets
###############################################################################

ind <- scan("data/dfm/media-rs-dtm-indices.txt")
pointers <- scan("data/dfm/media-rs-dtm-pointers.txt")
values <- scan("data/dfm/media-rs-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
tweets <- read.csv("data/dfm/media-tweets-random-sample.csv", stringsAsFactors=F, colClasses="character")

X <- sparseMatrix(j=ind, p=pointers, x=values,
  dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

load('topics/lda-output/lda-media-rs-results.Rdata')

# deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results$topics <- results$topics[-duplicated,]

# deleting weird tweets
todelete <- grep('xss', tweets$text)
tweets <- tweets[-todelete,]
results$topics <- results$topics[-todelete,]

K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=6)
  rs[[k]] <- tweets[choices,]
  rs[[k]]$topic <- k
}
rs <- do.call(rbind, rs)

# function to display embedded tweet
tw.embed <- function(text, name, screen_name, id_str, created_at, dt, js=FALSE){
    txt <- paste0('<blockquote class="twitter-tweet" data-cards="hidden" data-conversation="none" width="450"><p>',
        text, '</p> ', name, " (@", screen_name, 
        ") <a href='https://twitter.com/", screen_name,
        '/status/', id_str, "'>",
        dt, '</a></blockquote>')
    if (js){
        txt <- paste0(txt, 
            ' <script src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    }
    return(txt)
}

# preparing embed
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], rs$screen_name[i], rs$screen_name[i], rs$id_str[i],
    "", "")

}

media_rs <- rs
save(media_rs, file="dashboard/media-rs-tweets.rdata")

###############################################################################
### F) MCs using each topic the most
###############################################################################

load('topics/lda-output/lda-mcs-results.Rdata')
mcs <- scan("data/dfm/mcs-list.txt", what='character')

K <- 100
topmcs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=5)
  topmcs[[k]] <- data.frame(screenName = mcs[choices], stringsAsFactors=F)
  topmcs[[k]]$topic <- k
}

topmcs <- do.call(rbind, topmcs)
topmcs$order <- 1:nrow(topmcs)
mcs <- read.csv("data/congress_data.csv", stringsAsFactors=F)

topmcs <- merge(topmcs, 
  mcs[,c("screenName", "party", "state")], all.x=TRUE, sort=FALSE)

topmcs <- topmcs[order(topmcs$order),]
topmcs$text <- paste0(
  ' <a href="http://www.twitter.com/', topmcs$screenName, '">@', topmcs$screenName, ' (',
    topmcs$party, '-', topmcs$state, ')</a>')

save(topmcs, file="dashboard/top-mcs.rdata")

