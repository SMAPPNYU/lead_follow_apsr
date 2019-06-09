#===============================================================================
#  File:    03-running-lda.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: run topic model on tweets by Members of Congress; then apply model
#           to compute posterior model probabilities for all corporsa
#  Data In: 
#           ./data/tweets/text/* & data/dfm/congress-*
#  Data Out: 
#           LDA output
#===============================================================================

# PACKAGES
#===============================================================================
library(slam)
library(Matrix)
library(tm)
library(topicmodels)

# DATA
#===============================================================================
## preparing Congress matrix
fls <- scan("data/dfm/fls-list.txt", what="character", sep="\n")
ind <- scan("data/dfm/congress-dtm-indices.txt")
pointers <- scan("data/dfm/congress-dtm-pointers.txt")
values <- scan("data/dfm/congress-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
                  dims=c(length(fls), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- apply(dtm, 2, sum)
tail(sort(rs),n=100)

# removing empty rows
cs <- row_sums(dtm)
dtm <- dtm[cs>0,]


# MAIN: CONGRESS
#===============================================================================
# running regular LDA
lda.fit <- LDA(dtm, k=100, method="Gibbs",
    	control=list(verbose=50L, iter=2000))

save(lda.fit, file="topics/lda_results-twokenizer.Rdata")


# OTHER GROUPS
#===============================================================================

########################
#### MEDIA ARTICLES ####
########################

load("topics/lda_results-twokenizer.Rdata")

## preparing  matrix
ind <- scan("data/dfm/media-tweets-dtm-indices.txt")
pointers <- scan("data/dfm/media-tweets-dtm-pointers.txt")
values <- scan("data/dfm/media-tweets-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
	dims=c(365*2, length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]
tail(tmp,n=100)

# removing empty rows
cs <- row_sums(dtm)
dtm <- dtm[cs>0,]

## getting posterior estimates
results <- posterior(lda.fit, dtm, 
    control=list(verbose=10L, iter=1000))

# saving output
save(results, file='topics/lda-output/lda-media-results.Rdata')

########################################
#### MEDIA ARTICLES (RANDOM SAMPLE) ####
########################################

load("topics/lda_results-twokenizer.Rdata")

ind <- scan("data/dfm/media-rs-dtm-indices.txt")
pointers <- scan("data/dfm/media-rs-dtm-pointers.txt")
values <- scan("data/dfm/media-rs-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
tweets <- read.csv("data/tweets/media-tweets-random-sample.csv", stringsAsFactors=F, colClasses="character")

X <- sparseMatrix(j=ind, p=pointers, x=values,
    dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm, 
    control=list(verbose=10L, iter=1000))

# saving output
save(results, file='topics/lda-output/lda-media-rs-results.Rdata')

#################################
#### RANDOM SAMPLE OF TWEETS ####
#################################

load("topics/lda_results-twokenizer.Rdata")

## preparing  matrix
ind <- scan("data/dfm/rs-dtm-indices.txt")
pointers <- scan("data/dfm/rs-dtm-pointers.txt")
values <- scan("data/dfm/rs-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
tweets <- read.csv("data/tweets/tweets-random-sample.csv", stringsAsFactors=F, colClasses="character")

X <- sparseMatrix(j=ind, p=pointers, x=values,
	dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm, 
    control=list(verbose=10L, iter=1000))

# saving output
save(results, file='topics/lda-output/lda-rs-results.Rdata')

########################
#### INDIVIDUAL MCS ####
########################

load("topics/lda_results-twokenizer.Rdata")

## preparing  matrix
ind <- scan("data/dfm/mcs-dtm-indices.txt")
pointers <- scan("data/dfm/mcs-dtm-pointers.txt")
values <- scan("data/dfm/mcs-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
mcs <- scan("data/dfm/mcs-list.txt", what='character')

X <- sparseMatrix(j=ind, p=pointers, x=values,
	dims=c(length(mcs), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm, 
    control=list(verbose=10L, iter=500))

# saving output
save(results, file='topics/lda-output/lda-mcs-results.Rdata')

#################################
#### USERS                  ####
#################################

load("topics/lda_results-twokenizer.Rdata")

## preparing  matrix
ind <- scan("data/dfm/users-dtm-indices.txt")
pointers <- scan("data/dfm/users-dtm-pointers.txt")
values <- scan("data/dfm/users-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
users <- scan("data/dfm/users-list.txt", what='character')

X <- sparseMatrix(j=ind, p=pointers, x=values,
	dims=c(length(users), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

#################
## REPUBLICANS
#################

reps <- grep('republicans', users)
dtm2 <- dtm[reps,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2, 
    control=list(verbose=1L, iter=500))

# saving output
save(results, file='topics/lda-output/lda-reps-results.Rdata')

##############
## DEMOCRATS
##############

dems <- grep('democrats', users)
dtm2 <- dtm[dems,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2, 
    control=list(verbose=1L, iter=500))

# saving output
save(results, file='topics/lda-dems-results.Rdata')

##############
## PUBLIC
##############

pub <- grep('_public', users)
dtm2 <- dtm[pub,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2, 
    control=list(verbose=1L, iter=500))

# saving output
save(results, file='topics/lda-output/lda-public-results.Rdata')

#################################
#### RANDOM USERS            ####
#################################

cat("Random sample of users\n")

load("topics/lda_results-twokenizer.Rdata")

## preparing  matrix
ind <- scan("data/dfm/random-users-dtm-indices.txt")
pointers <- scan("data/dfm/random-users-dtm-pointers.txt")
values <- scan("data/dfm/random-users-dtm-values.txt")
words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
users <- scan("data/dfm/random-users-list.txt", what='character')

X <- sparseMatrix(j=ind, p=pointers, x=values,
    dims=c(length(users), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm, 
    control=list(verbose=1L, iter=500))

# saving output
save(results, file='topics/lda-output/lda-USrs-results.Rdata')

