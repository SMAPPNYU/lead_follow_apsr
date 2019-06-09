#===============================================================================
#  File:    02-choosing-number-topics.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: run multiple LDA models to choose number of topics based on
#           logLikelihood and perplexity
#  Data In: 
#           ./data/tweets/text/* & data/dfm/congress-*
#  Data Out: 
#           appendix figure with cross-validated model fit
#===============================================================================

# PACKAGES
#===============================================================================
library(slam)
library(Matrix)
library(tm)
library(topicmodels)
library(cvTools)
library(reshape)
library(ggplot2)
library(grid)


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


# FUNCTIONS
#===============================================================================
# cross-validated LDA
cvLDA <- function(Ntopics,dtm,K=10) {
  folds<-cvFolds(nrow(dtm),K,1)
  perplex <- rep(NA,K)
  llk <- rep(NA,K)
  for(i in unique(folds$which)){
    cat(i, " ")
    which.test <- folds$subsets[folds$which==i]
    which.train <- {1:nrow(dtm)}[-which.test]
    dtm.train <- dtm[which.train,]
    dtm.test <- dtm[which.test,]
    lda.fit <- LDA(dtm.train, k=Ntopics, method="Gibbs",
                   control=list(verbose=50L, iter=100))
    perplex[i] <- perplexity(lda.fit,dtm.test)
    llk[i] <- logLik(lda.fit)
  }
  return(list(K=Ntopics,perplexity=perplex,logLik=llk))
}


# MAIN
#===============================================================================

## running LDA with different number of topics
K <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130)
results <- list()
i = 1
for (k in K){
    cat("\n\n\n##########\n\n\n ", k, "topics", "\n\n\n")
    res <- cvLDA(k, dtm)
    results[[i]] <- res
    save(results, file="topics/k_topics_results_cv.Rdata")
    i = i + 1
}

save(results, file="topics/k_topics_results_cv.Rdata")


# PLOT
#===============================================================================
df <- data.frame(
    k = rep(K, each=10),
    perp =  unlist(lapply(results, '[[', 'perplexity')),
    loglk = unlist(lapply(results, '[[', 'logLik')),
    stringsAsFactors=F)
min(df$perp)
df$ratio_perp <- df$perp / max(df$perp)
df$ratio_lk <- df$loglk / min(df$loglk)
df <- data.frame(cbind(
    aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
    aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
    stringsAsFactors=F)
names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")

pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
pd$sd <- pd2$value
levels(pd$variable) <- c("Perplexity", "LogLikelihood")

p <- ggplot(pd, aes(x=k, y=value, linetype=variable))
pq <- p + geom_line() + geom_point(aes(shape=variable), 
        fill="white", shape=21, size=1.40) +
    geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width=4) +
    scale_y_continuous("Ratio wrt worst value") +
    scale_x_continuous("Number of topics", 
        breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)) +
    theme_bw() +
    scale_shape_discrete(guide="none") +
    scale_linetype_discrete(guide="none") +
        theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key.size=unit(0.5, "cm"), legend.position=c(0.70,.90), 
    legend.box.just = "left", legend.direction="horizontal",
    legend.title=element_blank()) +
        annotate("text", x=100, y=0.86, label="Perplexity", size=3) +
        annotate("text", x=100, y=0.945, label="logLikelihood", size=3)
pq

ggsave("plots/appendix-choosing-k.pdf", pq, height=4, width=6)







