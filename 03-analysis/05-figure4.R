#===============================================================================
#  File:    05-figure4.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 4 of the paper, where we show the issue-level
#           IRFs
#  Data In: 
#           # Topic attention distribution for each group across time
#           ./data/main-time-series.csv
#           # Human-redable labels for our topic codes
#           - ./data/pa2our_topics_crosswalk_merged_subissues.csv
#  Data Out: 
#           # A copy of each of the issue-level IRFs
#           - ./var/issue-level/*.Rdata
#           # A copy of Figure 4
#           - ./images/figure4.png
#===============================================================================


# PACAKGES
#===============================================================================
library(dplyr)
library(vars)
library(tidyr)
library(ggplot2)
library(boot)


# DATA
#===============================================================================
# Time series with attention distribution for all groups across time
db <- read.csv("data/main-time-series.csv")

# A list with the issues we coded as being political
pol_issues <- c(3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
                32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 50, 51, 
                53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
                89, 93, 96, 97, 99, 100,
                # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42, 49
                # new merged issues:
                101, 102, 103, 104)

# INSTRUCTIONS
#===============================================================================
# These script is organized in 2 parts:
# In PART-A we estimate and save all issue-level IRFs. This part takes about 10
#           minutes to run. /!\ Skip to PART-B if you don't want to wait nor 
#           recreate this IRFs from scratch, and you if just want to generate 
#           Figure 4. 
#
# In PART-B we load the estimated issue-evel IRFs (the repo already contains a
#           copy of them, so PART-A is not required) and generates Figure 4.


#===============================================================================
#=============== PART A: ESTIMATING ISSUE-LEVEL VARs and IRFs ==================
#===============================================================================

# DATA WRANGLING
#===============================================================================
# - filtering out the non-political issues from the main data base
db <- db %>%
  filter(topic %in% pol_issues)

# - creating a list with the variables of interest. For the new iteration with
#   the US-located random sample: getting rid of the previous "random" variable
#   and renamig 'random_us' as 'random'
#variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")
db <- db %>%
  dplyr::select(-random) %>%
  rename(random = random_us)
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - selecting only the varaibles of interest (excluding the lags for now)
db <- db[, c("date", "topic", variables)]

# - logit transform all series
for (v in variables) {
  # - pulling the series-agenda for that group
  x <- db[,v]
  # - for some groups the last couple observations for each issues are NA,
  #     making these a 0 
  x[which(is.na(x))] <-0 
  # - adding 1 percentage point to avoid 0s before the logit transformation
  x <- x + 0.01
  # - applying the non-linear transformation
  logit_x <- log(x / (1-x))
  #print(length(which(is.na(logit_x)))) # - uncomment to check there are no NAs
  db[,v] <- logit_x
}

for (top in unique(db$topic)) {
  maindb <- db %>%
    filter(topic == top)
  # - data to matrix
  mformula <- formula(paste0("~", 
                             paste0(variables, collapse = " + ")))
  model_data <- model.matrix(mformula, maindb[, variables])
  model_data <- model_data[, 2:ncol(model_data)] # removing intercept
  
  # - only fitting endogenous variable in this case, no topic dummies
  X_endogenous <- model_data
  
  # - estimating the model, 7 lags of each endogenous variable
  var_model <- VAR(y = X_endogenous, p = 7)
  
  # - calculating cummulative 15 day IRFs
  var_irfs_cum <- irf(var_model, n.ahead = 60, cumulative = TRUE)
  
  # OUTPUT
  #=============================================================================
  save(var_irfs_cum, 
       file = paste0("var/issue-level/var_irfs_topic_", top, "_logit-UPD.Rdata"))
}


#===============================================================================
#======================== PART B: GENERATING FIGURE 4 ==========================
#===============================================================================
# - load dataset with human-readble labels for the topics
pa2our <- read.csv("data/pa2our_topics_crosswalk_merged_subissues.csv")

# - a list with variables of interest
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - initializing an empty dataset where to put all IRF info by topic
irf_data <- NULL

total <- length(pol_issues)
counter <- 0
for (top in pol_issues) {
  # - update counter and report progress
  counter <- counter + 1
  print(paste0("[", counter, "/", total, "]"))
  file_name <- paste0("var/issue-level/var_irfs_topic_", top, "_logit-UPD.Rdata")
  load(file_name) # object name: 'var_irfs_cum'
  
  # - iterating through endogenous covariates and endogenous responses
  covs <- resps <- names(var_irfs_cum$irf)
  for (covariate in covs) {
    for (response in resps) {
      cum_days_n <- nrow(var_irfs_cum$irf[[covariate]])
      new_rows <- data.frame(
        issue_num = top,
        cov = rep(covariate, cum_days_n),
        out = response,
        day = 1:cum_days_n,
        pe = NA, lwr = NA, upr = NA
      )
      
      # - iterating through estimate info (point estimate and lwr and upr CIs)
      for (estimate in c("irf", "Lower", "Upper")) {
        cov_irf_est <- as.data.frame(
          var_irfs_cum[[estimate]][[covariate]]
        )[[response]]
        # - inverting the logit transformation
        if (estimate == "irf") {
          new_rows$pe <- inv.logit(cov_irf_est) - 0.5 
        } else if (estimate == "Lower") {
          new_rows$lwr <- inv.logit(cov_irf_est) - 0.5
        } else {
          new_rows$upr <- inv.logit(cov_irf_est) - 0.5
        }
      }
      #print(paste0(covariate, " -> ", response))
      # - appending new rows to the main dataset with all IRF info
      irf_data <- rbind(irf_data, new_rows)
    }
  }
}

# - a version only keeping both parties in Congress, supporters of both parties,
#     and attentive publics
irf_plot <- irf_data %>%
  filter(cov %in% c("dem", "rep", "pubdem", "pubrep", "public", "random"),
         out %in% c("dem", "rep", "pubdem", "pubrep", "public", "random"))

# - removing rows where covariate and response are the same variable, and also
#     removoing info about how public agendas influence public agendas, and how
#     political agendas influence political agendas
agenda_type <- data.frame(
  var = c("dem", "rep", "pubdem", "pubrep", "public", "random"),
  type = c("pol", "pol", "pub", "pub", "pub", "pub")
)
cov_agenda_type <- agenda_type %>%
  rename(cov = var, cov_agenda_type = type)
out_agenda_type <- agenda_type %>%
  rename(out = var, out_agenda_type = type)

cov_agenda_type$cov <- as.character(cov_agenda_type$cov)
out_agenda_type$out <- as.character(out_agenda_type$out)
irf_plot$cov <- as.character(irf_plot$cov)
irf_plot$out <- as.character(irf_plot$out)

irf_plot <- left_join(irf_plot, cov_agenda_type)
irf_plot <- left_join(irf_plot, out_agenda_type)

irf_plot <- irf_plot %>%
  filter(cov_agenda_type != out_agenda_type)

# - merging to the dataset a human readable name for the topics
pa2our_tomerge <- pa2our %>%
  dplyr::select("our_", "our_topic_label") %>%
  rename(issue_num = our_, label = our_topic_label)

pa2our_tomerge$issue_num <- as.character(pa2our_tomerge$issue_num)
irf_plot$issue_num <- as.character(irf_plot$issue_num)

irf_plot <- left_join(irf_plot, pa2our_tomerge)

# - providing better labels to the outcome and response variables
irf_plot$out <- recode(irf_plot$out,
                       `dem` = "Democrats\nin Congress",
                       `rep` = "Republicans\nin Congress",
                       `pubdem` = "Democratic\nSupporters",
                       `pubrep` = "Republican\nSupporters",
                       `public` = "Attentive\nPublic",
                       `random` = "General\nPublic")

irf_plot$cov <- recode(irf_plot$cov,
                       `dem` = "Democrats\nin Congress",
                       `rep` = "Republicans\nin Congress",
                       `pubdem` = "Democratic\nSupporters",
                       `pubrep` = "Republican\nSupporters",
                       `public` = "Attentive\nPublic",
                       `random` = "General\nPublic")


# - relevel these variables
irf_plot$out <- factor(irf_plot$out,
                       levels = c(
                         "Democrats\nin Congress",
                         "Republicans\nin Congress",
                         "Democratic\nSupporters",
                         "Republican\nSupporters",
                         "Attentive\nPublic",
                         "General\nPublic"))

irf_plot$cov <- factor(irf_plot$cov,
                       levels = c(
                         "Democrats\nin Congress",
                         "Republicans\nin Congress",
                         "Democratic\nSupporters",
                         "Republican\nSupporters",
                         "Attentive\nPublic",
                         "General\nPublic"))

# - excluding the IRFs that cross 0 in order to more clearly see the ones that 
#     matter: too many dots otherwise
plot_db <- irf_plot %>%
  # - only keeping IRF that don't cross 0
  filter(day == 15) %>%
  arrange(out, cov, pe) %>%
  mutate(label = factor(label, levels = unique(label))) %>%
  filter(sign(lwr) == sign(upr))
  

# - sort by the issues in which Democratic supporters are more likely to lead 
#   the attention of Democrats in Congress

# PLOT -- FIGURE 4
#===============================================================================
png("images/figure4.png", width = 1600, height = 1400)
ggplot(plot_db %>% 
         mutate(pe = (pe * 100)/10, lwr = (lwr * 100)/10, upr = (upr * 100)/10),
       aes(x = label, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(col = cov), alpha = 0.4, size = 1.05) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~out, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("\nThe effect of a 10 percentage point increase in attention by the covariate group, measured in percentage point change") +
  scale_color_manual("", values = c("blue", "red", 
                                    "blue4", "red4", 
                                    "gray50", "orange2")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14, margin = margin(t = 20), vjust = 5)
  )
dev.off()

