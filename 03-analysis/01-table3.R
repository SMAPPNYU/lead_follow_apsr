#===============================================================================
#  File:    01-table3.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 3 of the paper, where we show the correlation
#           between the issue attention distribution of the different groups
#           under analysis
#  Data In: 
#           ./data/main-time-series.csv
#  Data Out: 
#           prints a table with the data in Table 3 of the paper
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(xtable)

# DATA
#===============================================================================
db <- read.csv("./data/main-time-series.csv")

# DATA WRANGLING
#===============================================================================
# - filtering out the non-political issues from the main data base
pol_issues <- c(3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
                32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51, 
                53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
                89, 93, 96, 97, 99, 100,
                # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42
                # new merged issues:
                101, 102, 103, 104)

db <- db %>%
  filter(topic %in% pol_issues)

# MAIN
#===============================================================================
# - calculate correlation between agenda of Democrats and Republicans in 
#   Congress, and the different groups of the Public as well as the Media
outgroups <- c("dem", "rep")
covgroups <- c("pubdem", "pubrep", "public", "random_us", "media")

results <- NULL
for (polgroup in outgroups) {
  new_col <- NULL
  for (compgroup in covgroups) {
    # - create a dataframe only with the two groups to compare
    comp_df <- data.frame(
      y = db[,polgroup],
      x = db[,compgroup]
    )
    # - remove NAs (only a couple rows for some groups -- last and/or first day)
    comp_df <- na.omit(comp_df)
    
    # - calculate correlation
    cor_out <- round(cor(comp_df)[2,1], 2)
    new_cell <- data.frame(cor_out)
    colnames(new_cell) <- polgroup
    rownames(new_cell) <- compgroup
    new_col <- rbind(new_col, new_cell)
  }
  if (is.null(results)) {
    results <- new_col
  } else {
    results <- cbind(results, new_col)
  }
}

# OUTPUT
#===============================================================================
# - adding human readable labels to the column and row names
rownames(results) <- c("Democratic Supporters", "Republican Supporters",
                       "Attentive Public", "General Public", "Media")
colnames(results) <- c("Democratis in Congress", "Republicans in Congress")

print(results)
#                       Democratis in Congress Republicans in Congress
# Democratic Supporters                   0.69                    0.51
# Republican Supporters                   0.41                    0.77
# Attentive Public                        0.49                    0.52
# General Public                          0.38                    0.34
# Media                                   0.52                    0.63
