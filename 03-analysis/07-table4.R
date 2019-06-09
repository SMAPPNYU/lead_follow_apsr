#===============================================================================
#  File:    07-table4.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Table 4 of the paper, where we show the correlation
#           between the issue attention distribution of the media and the 
#           political and public groups under study.
#  Data In: 
#           # Topic attention distribution for each group across time
#           ./data/main-time-series.csv
#  Data Out:
#           # Prints output that goes into Table 4 of the paper
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)

# DATA
#===============================================================================
db <- read.csv("data/main-time-series.csv") %>%
  dplyr::select(-random) %>%
  mutate(random = random_us) %>%
  dplyr::select(-random_us)

# DATA WRANGLING -- MAIN
#===============================================================================
# - filtering out the non-political issues from the main data base
pol_issues <- c(3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
                32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51,
                53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
                89, 93, 96, 97, 99, 100,
                # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42
                # new merged issues:
                101, 102, 103, 104)
db_pol <- db %>%
  filter(topic %in% pol_issues)

# - calculate correlation betwee the media agenda and the issue att distr of the
#   rest of groups
rest_groups <- names(db_pol)[which(!(names(db_pol) %in% 
                                       c("date", "topic", "media")))]
media_agenda <- db_pol$media
results <- NULL
for (group in rest_groups) {
  out_cor <- cor.test(media_agenda, db_pol[,group])
  out_cor_pe <- round(out_cor$estimate, 2)
  new_row <- data.frame(
    group = group, 
    cor = out_cor_pe
  )
  results <- rbind(results, new_row)
}

# - give human readble labels to and sort the group labels
results$group <- recode(results$group,
                        `dem` = "Democrats in Congress",
                        `rep` = "Republicans in Congress",
                        `public` = "Attentive Public",
                        `pubdem` = "Democratic Supporters",
                        `pubrep` = "Republican Supporters",
                        `random` = "General Public")

results$group <- factor(results$group, 
                        levels = c(
                          "Democrats in Congress",
                          "Republicans in Congress",
                          "Attentive Public",
                          "Democratic Supporters",
                          "Republican Supporters",
                          "General Public"
                        ))

colnames(results) <- c("Group", "Media")

results <- results %>% 
  arrange(Group)

# OUTPUT: LATEX CODE FOR A TABLE
#===============================================================================
print(results)
#                   Group  Media
#   Democrats in Congress   0.52
# Republicans in Congress   0.63
#        Attentive Public   0.74
#   Democratic Supporters   0.79
#   Republican Supporters   0.79
#          General Public   0.55
