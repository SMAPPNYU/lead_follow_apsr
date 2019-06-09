#===============================================================================
#  File:    05-adding-random-US-and-merging-subissues.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: this script prepares the final dataset that will be used in most of
#           the analysis: group-level time series describing how much attention
#           each group under analysis paid to each topic during the 113th Cong.
#           In particular, in here:  
#           A) we merge a set of subissues detected in the original output.
#           B) we add to the dataset generated in 01/04.R, the attention paid to 
#           each issues by the users geolocated in the US (estimated in 01/0X.R)
#  Data In: 
#           # Topic attention distribution for each group across time
#           ./data/main-time-series-PRE.csv
#           # Posterior topic distribution for the US-located random sample
#           ./topics/lda-output/lda-USrs-results.Rdata
#  Data Out:
#           # Main time-series used in the analysis
#           ./data/main-time-series.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(rio)

# DATA
#===============================================================================
# - load dataset created in 04-....R (all time-series but the one for the random
#   users geolocated in the United States)
db <- import("data/main-time-series-PRE.csv")

# - load the posterior topic distribution for the US-located random sample
print(load(paste0("topics/lda-output/lda-USrs-results.Rdata")))

# [A] Merging Sub-Issues
#===============================================================================
# - list of subissues to merge
subissues_to_merge <- list(
  # - 2 subissues about "Student Debt" will become topic 101
  c(27, 56),
  # - 2 subissues about "Hobby Lobby Supreme Court Decision" will become 102
  c(11, 74),
  # - 2 subissues about general "Budget Discussion" will become topic 103
  c(38, 59),
  # - 5 subissues about the "Government Shutdown" will become topic 104:
  #     (Nov. 30, 2017: +1 shutdown issue that I forgot: 49)
  c(17, 26, 35, 42, 49)
)

# - a list with the different agendas
groups <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - making sure the dataset is sorted by topic and by date
new_db <- db %>%
  arrange(topic, date) %>%
  mutate(topic = paste0("topic", topic))

# - adding up the attention paid to subissues belonging to the same issue. 
#     Creating new issue codes for these new merged issues.
for (i in 1:length(subissues_to_merge)) {
  # - iterate through groups of subissues to merge
  subissues <- subissues_to_merge[[i]]
  
  # - initializing new empty rows for the new issue
  new_empty_rows <- as.data.frame(matrix(nrow = 730, ncol = ncol(new_db),
                                         data = rep(NA, 730 * ncol(new_db))))
  colnames(new_empty_rows) <- colnames(new_db)
  new_empty_rows$topic <- paste0("topic", (100 + i))
  new_db <- rbind(new_db, new_empty_rows)
  
  # - iterate through the different agendas
  for (group in groups) {
    # - pull the time-series for that group that are realted to the subissues 
    group_db <- new_db[, c("date", "topic", group)]
    colnames(group_db)[3] <- "gr"
    new_issue_group_series <- group_db %>%
      filter(topic %in% paste0("topic", subissues)) %>%
      # - transform the categorical issue variable into dummies\
      spread(topic, gr) %>%
      # - adding up the attention to both subissues
      dplyr::select(-date) %>%
      mutate(merged_issue = rowSums(.)) %>%
      dplyr::select(merged_issue)
    new_db[new_db$topic == paste0("topic", (100 + i)),
           group] <- new_issue_group_series
  }
}

new_db$topic <- gsub("topic", "", new_db$topic)

# [B] Adding issue attention by random users located in the United States
#===============================================================================
model_data <- new_db

# - pulling from the LDA predictions, the posterior topic distributions
top_dist <- results$topics

# - merging the same sub-issues we merged in the previous model data. Adding up
#   the posterior distributions of topics that are sub-issues of the same macro
#   issue

# - 2 subissues about "Student Debt" will become topic 101: #27 and #56
issue101 <- top_dist[,27] + top_dist[,56]

# - 2 subissues about "Hobby Lobby Supreme Court Decision" will become topic 
#   102: #11 and #74
issue102 <- top_dist[,11] + top_dist[,74]

# - 2 subissues about general "Budget Discussion" will become topic 103:
#   #38 and #59
issue103 <- top_dist[,38] + top_dist[,59]

# - 5 subissues about general "Government Shutdown" will become topic 104:
#   #17, #26, #35, #42 and #49
issue104 <- top_dist[,17] + top_dist[,26] + top_dist[,35] + top_dist[,42] +
  top_dist[,49]

top_dist <- cbind(top_dist, issue101)
top_dist <- cbind(top_dist, issue102)
top_dist <- cbind(top_dist, issue103)
top_dist <- cbind(top_dist, issue104)

# - reshaping the topic distributions so they fit the 'model_data' format. 
#   Stacking all the topic-level columns into a single one containing all topic
#   info about this US-located random sample.
# - I already made sure that the new topic distributions are sorted by DATE in
#   the same way the previous model data is.
top_dist_reshaped <- NULL
for (i in 1:ncol(top_dist)) {
  top_series <- top_dist[,i]
  new_df <- data.frame(
    topic = as.character(i),
    random_us = top_series
  )
  top_dist_reshaped <- rbind(top_dist_reshaped, new_df)
}

# - finally, merging these topic probabilities for the new US-located random
#   sample to the previous model data. I checked both DATE and TOPIC align with
#   the model data structure.
random_us <- top_dist_reshaped[,"random_us"]
new_model_data <- cbind(model_data, random_us)
new_model_data <- as.data.frame(new_model_data)

# - sanity check: we expect the time series for the original group of random
#   users and those we geolocated in the US to be highly correlated (the overlap
#   in terms of users is large)
cor.test(new_model_data$random, new_model_data$random_us)
# correlation = 0.99

# - drop topics not coded as politically relevant
pol_issues <- c(3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
                32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51,
                53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
                89, 93, 96, 97, 99, 100,
                # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42
                # new merged issues:
                101, 102, 103, 104)
final_data <- new_model_data %>%
  filter(topic %in% pol_issues)

# OUTPUT
#===============================================================================
# - the main time-series that will be used in the analysis 
#   /!\ (if you uncomment next line you'll ovewrite existing copy of the file)
# write.csv(final_data, "data/main-time-series.csv", row.names = FALSE)