#===============================================================================
#  File:    03-figure2.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 2 of the paper, where we show the results of our
#           main VAR model, by showing 15-day Impulse Response Functions for
#           one-time as well as permanent 10-percentage point increases in 
#           attention
#  Data In: 
#           # Topic attention distribution for each group across time
#           ./data/main-time-series.csv
#  Data Out: 
#           # A copy of the VAR model
#           - ./var/main-var.Rdata
#           # A copy of the IRFs
#           - ./var/main-irfs.Rdata
#           # Information about the 15-day IRFs for a one-time and a permanent
#             10-point increase in attention
#           - ./var/onetime-structural-shock-irfs-results.csv
#           # A copy of Figure 2
#           - ./images/figure2.png
#===============================================================================

# PACAKGES
#===============================================================================
library(dplyr)
library(vars)
library(boot)
library(rio)

# INSTRUCTIONS
#===============================================================================
# These script is organized in 3 parts:
# In PART-A we estimate and save the main VAR model of the study as well as the
#           resulting IRFs. I takes between 5-10 minutes to run. /!\ Skip to 
#           PART-B if you don't want to wait nor re-estimate this from scratch.
#
# In PART-B we load the estimated IRFs to calculate 15-day effects for a 
#           one-time and a permanent increase in attention of 10 perc points.
#           It takes about @ minutes to run. /!\ Skip to PART-C if you don't 
#           want to wait nor re-estimate this from scratch, and if you just
#           want to generate Figure 2.
#
# In PART-C we use the data generated in PARTS A/B to create Figure 2.


#===============================================================================
#================ PART A: ESTIMATING VAR and CALCULATING IRFs ==================
#===============================================================================

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

# - creating a list with the variables of interest. For the new iteration with
#   the US-located random sample: getting rid of the previous "random" variable
#   and renamig 'random_us' as 'random'
#variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")
db <- db %>%
  dplyr::select(-random) %>%
  rename(random = random_us)
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - selecting only the varaibles of interest
db <- db[, c("date", "topic", variables)]

# - logit transform all series
for (v in variables) {
  # - pulling the series-agenda for that group
  x <- db[,v]
  # - for some groups the last couple observations for each issues are NA,
  #     making these a 0 
  x[which(is.na(x))] <-0.01
  # - adding 1 percentage point to avoid 0s before the logit transformation
  #x <- x + 0.01
  # - applying the non-linear transformation
  logit_x <- log(x / (1-x))
  db[,v] <- logit_x
}

# - adding "topic" to the list of model variables
variables <- c(variables, "topic")

maindb <- db %>%
  filter(topic %in% pol_issues)

# - creating a list with the variables of interest
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media",
               "topic")

# - a formula object that will facilitate transforming the topic variable from
#     factor to dummies
maindb$topic <- as.character(maindb$topic)
mformula <- formula(paste0("~", 
                           paste0(variables, collapse = " + ")))
model_data <- model.matrix(mformula, maindb[, variables])
model_data <- model_data[, 2:ncol(model_data)] # removing intercept

# - splitting the covariates of interest from the issue dummy variables
X_endogenous <- model_data[, which(!grepl("topic", colnames(model_data)))]
X_exogenous <- model_data[, which(grepl("topic", colnames(model_data)))]

# - estimating the model: 7 lags
var_model_merged <- VAR(y = X_endogenous, p = 7, exogen = X_exogenous)

var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 60, cumulative = TRUE)

# PART-A OUTPUT
#===============================================================================
# - a copy of the VAR model: NO NEED TO SAVE
save(var_model_merged, file = "var/var_model-MAIN.Rdata")

# - a copy of the IRFs
save(var_irfs_cum_merged, file = "var/var_irfs-MAIN.Rdata")

#===============================================================================
#======== PART B: CALCULATING ONE-TIME and PERMANENT 10-POINT EFFECTS ==========
#===============================================================================
# - load the IRFs calculated in PART A
print(load("var/var_irfs-MAIN.Rdata"))
var_irfs <- var_irfs_cum_merged

# - a list with the endogenous variables of the model
var_irfs <- var_irfs_cum_merged
variables <- names(var_irfs$irf)

# - a list with the elements of interest from the IRF object
elements_to_pull <- c("irf", "Upper", "Lower")

# - initializing and filling a dataset with the IRF info
irf_data <- NULL
for (el in elements_to_pull) {
  new_irf_info <- var_irfs[el][[1]]
  for (out in variables) {
    new_irf_var_data <- as.data.frame(new_irf_info[out][[1]])
    # - take inverse logit to transform the effects to percentage point changes
    new_irf_var_data_transf <- as.data.frame(
      sapply(1:ncol(new_irf_var_data), function(j)
        inv.logit(new_irf_var_data[,j]) - 0.5))
    colnames(new_irf_var_data_transf) <- colnames(new_irf_var_data)
    new_irf_var_data_long <- new_irf_var_data_transf %>%
      gather(cov, value)
    new_irf_var_data_long$out <- out
    new_irf_var_data_long$day <- rep(1:nrow(new_irf_var_data), 
                                     length(unique(new_irf_var_data_long$cov)))
    new_irf_var_data_long$e_type <- el
    irf_data <- rbind(irf_data, new_irf_var_data_long)
  }
}

# - give easier labels to the estimate types (e.g. Lower --> lwr)
irf_data$e_type <- recode(irf_data$e_type,
                          `irf` = "pe",
                          `Lower` = "lwr", 
                          `Upper` = "upr")

# MAIN
#===============================================================================
# - initializing a output dataset where we'll add info about one-time AND 
#   permanent shocks
new_irf_data <- NULL

# - a vector with the name of the variables
variables <- unique(irf_data$cov)

# - deciding the number of days to simulate
DAYS <- 60

irf_data <- irf_data %>%
  filter(day <= (DAYS + 1))

# - iterating through covariates
for (covariate in variables) {
  # -iterating through outcomes
  for (outcome in variables) {
    # - skipping when covariate and response are the same
    if (covariate != outcome) {
      # - initializing a cummulative-shocks matrix for this scenario: two 3-dim 
      #     matrix, one matrix for the covariate and one matrix for the response,
      #     and one dimension for the point estimate and the two other dimensions
      #     for the lower and upper bounds of the estimate
      cov_mat <- array(0, dim = c(DAYS, DAYS, 3))
      out_mat <- array(0, dim = c(DAYS, DAYS, 3))
      
      # - pull the full 15-day IRFs for the endogenous covariate
      cov_resp <- irf_data %>%
        filter(cov == covariate, out == covariate) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - pull the full 15-day IRFs for the particular outcome variable
      out_resp <- irf_data %>%
        filter(cov == covariate, out == outcome) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - transforming the 15-day IRFs for the covariate and outcome to a wide
      #   3-column format (one column per estimate type: pe, lwr, upr)
      or_cov_resp <- cov_resp %>%
        dplyr::select(day, value, e_type) %>%
        spread(e_type, value) %>%
        dplyr::select(-day)
      
      or_out_resp <- out_resp %>%
        dplyr::select(day, value, e_type) %>%
        spread(e_type, value) %>%
        dplyr::select(-day)
      
      # - fill up the first rows of the scenario matrices with the original 
      #   1-day shock responses
      cov_mat[1,,1:3] <- or_cov_resp %>%
        as.matrix()
      out_mat[1,,1:3] <- or_out_resp %>%
        as.matrix()
      
      for (i in 2:DAYS) {
        # - iterating through the rest of the 15 days, beyond the 1st one
        # - chekcing first how much attention the covariate group is predicted 
        #   to pay to the issue in day i-1
        cov_att_pe <- sum(cov_mat[,(i-1),2])
        
        # - calculating how big a new shock needs to be in order for the 
        #   covariate group to keep its attention to 100%
        cov_new_shock <- 1 - cov_att_pe
        
        # - re-scaling the original 100 percentage point shock to the new shock
        cov_new_resp <- or_cov_resp[1:(DAYS-(i-1)),] * cov_new_shock
        out_new_resp <- or_out_resp[1:(DAYS-(i-1)),] * cov_new_shock
        
        # - adding the response to this new shock to the scenario matrices
        cov_mat[i,i:DAYS,1:3] <- cov_new_resp %>%
          as.matrix()
        out_mat[i,i:DAYS,1:3] <- out_new_resp %>%
          as.matrix()
      }
      # - saving the output for this cov --> out 
      new_rows <- rbind(
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,1]),
          out = outcome,
          day = 1:DAYS,
          e_type = "lwr",
          data_type = "structural"),
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,2]),
          out = outcome,
          day = 1:DAYS,
          e_type = "pe",
          data_type = "structural"),
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,3]),
          out = outcome,
          day = 1:DAYS,
          e_type = "upr",
          data_type = "structural")
      )
      new_irf_data <- rbind(new_irf_data, new_rows)
    }
  }
}

# - merging this new type of IRFs with the "regular" 1-time-shock IRFs
irf_data$data_type <- "one_time_shock"
irf_data <- irf_data %>%
  # - correct the original data for the fact that day 1 is just pre-setting
  filter(day != 1) %>% 
  mutate(day = day -1)

all_irf_data <- rbind(irf_data, new_irf_data)

# - removing from the dataset cases in which covariate and outcome are the same
all_irf_data <- all_irf_data %>%
  filter(cov != out)

# - a wide version of the dataset, with a separate column for each estimate type
all_irf_data_wide <- all_irf_data %>%
  spread(e_type, value)

# - save a copy of this dataset (uncomment to overwrite the file)
#write.csv(all_irf_data_wide, paste0(data_path, "all_irf_data_wide.csv"),
#          row.names = FALSE)

# - simulate one-time and structural shocks of 10 instead of 100 percentage pts.
#   Present the results in 0-100 scale instead of 0-1
all_irf_data_wide <- all_irf_data %>%
  mutate(value = (value / 10) * 100) %>%
  spread(e_type, value)

# - humand readble labels for the covariates and outcomes
all_irf_data_wide$cov <- recode(all_irf_data_wide$cov,
                                `dem` = "Democrats in Congress",
                                `rep` = "Republicans in Congress",
                                `pubdem` = "Democratic Supporters",
                                `pubrep` = "Republican Supporters",
                                `public` = "Attentive Public",
                                `random` = "General Public",
                                `media` = "Media")

all_irf_data_wide$out <- recode(all_irf_data_wide$out,
                                `dem` = "Democrats in Congress",
                                `rep` = "Republicans in Congress",
                                `pubdem` = "Democratic Supporters",
                                `pubrep` = "Republican Supporters",
                                `public` = "Attentive Public",
                                `random` = "General Public",
                                `media` = "Media")

# - reorder the levels of the outcome and covariate factor variables
all_irf_data_wide$out <- factor(all_irf_data_wide$out,
                                levels = c("Democrats in Congress",
                                           "Republicans in Congress",
                                           "Democratic Supporters",
                                           "Republican Supporters",
                                           "Attentive Public",
                                           "General Public",
                                           "Media"))

all_irf_data_wide$cov <- factor(all_irf_data_wide$cov,
                                levels = c("Democrats in Congress",
                                           "Republicans in Congress",
                                           "Democratic Supporters",
                                           "Republican Supporters",
                                           "Attentive Public",
                                           "General Public",
                                           "Media"))

# - better labels for the data type
all_irf_data_wide$data_type <- recode(
  all_irf_data_wide$data_type,
  `one_time_shock` =  "Effect of a one time 10 percentage point attention increase at day 0",
  `structural` = "Effect of a structural 10 percentage point attention increase at day 0")


# PART-B OUTPUT
#===============================================================================
# - saving the information about the 15-day effects of a one-time and permanent
#   10 percentage point increase in attention
#   
write.csv(all_irf_data_wide,
          "var/onetime-structural-shock-irfs-results.csv",
          row.names = FALSE)



#===============================================================================
#======================== PART C: GENERATING FIGURE 3 ==========================
#===============================================================================
# - load the ouptput of PART B
final_input <- import("var/onetime-structural-shock-irfs-results.csv")


# DATA WRANLGING
#===============================================================================
# - exploring only 15-day effects, and removing the Media from the analysis.
plot_db <- final_input %>%
  filter(day == 15)

# - relabel the covariates so they fit/look better in the plot
plot_db$cov <- recode(plot_db$cov,
                      `Democrats in Congress` = "Democrats\nin Congress",
                      `Republicans in Congress` = "Republicans\nin Congress",
                      `Democratic Supporters` = "Democratic\nSupporters",
                      `Republican Supporters` = "Republican\nSupporters",
                      `Attentive Public` = "Attentive\nPublic",
                      `General Public` = "General\nPublic")

plot_db$out <- recode(plot_db$out,
                      `Democrats in Congress` = "Democrats\nin Congress",
                      `Republicans in Congress` = "Republicans\nin Congress",
                      `Democratic Supporters` = "Democratic\nSupporters",
                      `Republican Supporters` = "Republican\nSupporters",
                      `Attentive Public` = "Attentive\nPublic",
                      `General Public` = "General\nPublic")

# - reordering the covariate and outcome categories
plot_db$cov <- 
  plot_db$cov <- factor(plot_db$cov,
                        levels = rev(c("Democrats\nin Congress",
                                       "Republicans\nin Congress",
                                       "Democratic\nSupporters",
                                       "Republican\nSupporters",
                                       "Attentive\nPublic",
                                       "General\nPublic", 
                                       "Media")))

plot_db$out <- factor(plot_db$out,
                      levels = c("Democrats\nin Congress",
                                 "Republicans\nin Congress",
                                 "Democratic\nSupporters",
                                 "Republican\nSupporters",
                                 "Attentive\nPublic",
                                 "General\nPublic",
                                 "Media"))

# - re-phrase the shock labels
plot_db$data_type <- ifelse(
  grepl("one time", plot_db$data_type),
  "The effect of a one time 10 percentage point increase in day 0             ",
  "The effect of a permanent 10 percentage point increase in day 0"
)

# OUTPUT -- FIGURE 2
#===============================================================================
png("images/figure2.png", width = 1600, height = 700)
ggplot(plot_db,
       aes(x = cov, y = pe, ymin = lwr, ymax = upr, col = data_type)) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), 
               size = 2.5) +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous("\n15-day Responses (in percentage points)",
                     limits = c(0, 8), expand = c(0,0)) +
  scale_color_manual("",values = c("gray60", "gray10")) +
  theme(
    panel.spacing = unit(1.05, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 18),
    axis.text.y = element_text(hjust=0),
    strip.text = element_text(size = 20),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
dev.off()

