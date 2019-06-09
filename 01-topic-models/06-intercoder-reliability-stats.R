#===============================================================================
#  File-Name:	06-intercoder-reliability.R
#  Date:	January 15, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: compute intercoder reliability stats for coding of political
#     vs non-political topics
#  Data In:
#  - ./data/topic-list-classification.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(psych)

# DATA
#===============================================================================
d <- read.csv("topics/topic-list-classification.csv", stringsAsFactors=F)
coders <- c("C1", "C2", "C3", "C4", "C5")

################################################################################
# computing intercoder reliability stats
################################################################################

## computing intercoder reliability matrix
results <- matrix(1, nrow=length(coders), ncol=length(coders),
                  dimnames=list(coders, coders))

for (row in coders){
  for (col in coders){
    x <- d[,row]
    y <- d[,col]
    results[row, col] <- mean(sapply(1:100, function(z) x[z]==y[z])*1)
  }
}

results

# C1   C2   C3   C4   C5
# C1 1.00 0.77 0.76 0.69 0.66
# C2 0.77 1.00 0.82 0.77 0.68
# C3 0.76 0.82 1.00 0.80 0.78
# C4 0.69 0.77 0.80 1.00 0.72
# C5 0.66 0.68 0.78 0.72 1.00

## average pairwise intercoder reliability
apir <- function(var, ids){
  
  require(gtools)
  response.pairs <- list()
  i <- 1
  for (id in ids){
    resp <- var[ids==id]
    resp <- resp[!is.na(resp)]
    if (length(resp)<2){
      i <- i + 1
      next
    }
    ## making responses unique
    resp <- paste0(resp, '_', 1:length(resp))
    ## combinations
    comb <- combinations(length(resp),2,resp,repeats=TRUE)
    ## removing unique elements
    comb <- matrix(sapply(comb, function(x) gsub('_.', '', x)), ncol=2)
    ## adding it to list
    response.pairs[[i]] <- comb
    i <- i + 1
  } 
  response.pairs <- do.call(rbind, response.pairs)
  apir <- mean(apply(response.pairs, 1, function(x) x[1]==x[2]))
  return(c(round(apir, 2), nrow(response.pairs)))
}

df <- data.frame(
  id = rep(1:100, 5),
  responses = unlist(c(d[,coders])))

apir(df$responses, df$id) ## 0.83

## cronbach's alpha
alpha(d[,coders])

## finding distribution of modal categories
d$consensus <- NA
mode <- function(x){
  tab <- table(x[!is.na(x)])
  names(tab)[which.max(tab)]
}
d$consensus <- apply(d[,-1], 1, mode)

table(d$consensus)
