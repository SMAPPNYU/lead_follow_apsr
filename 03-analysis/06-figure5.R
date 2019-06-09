#===============================================================================
#  File:    06-figure5.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 5 of the paper, where we show the correlation
#           between issue-level responsiveness and issue salience.
#  Data In: 
#           # Topic attention distribution for each group across time
#           ./data/main-time-series.csv
#           # A copy of each of the issue-level IRFs
#           - ./var/issue-level/*.Rdata
#           # Human-redable labels for our topic codes
#           - ./data/pa2our_topics_crosswalk_merged_subissues.csv
#  Data Out: 
#           # Figure 5
#           - ./images/figure5.png
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)
library(ggrepel)

# DATA -- DATA WRANGLING
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

# - load dataset with human-readble labels for the topics
pa2our <- read.csv("data/pa2our_topics_crosswalk_merged_subissues.csv")

# IRF data
#===============================================================================
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
  file_name <- paste0("var/issue-level/var_irfs_topic_", top, 
                      "_logit-UPD.Rdata")
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
                       `dem` = "Democratic MCs",
                       `rep` = "Republican MCs",
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
                         "Democratic MCs",
                         "Republican MCs",
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
irf_toplot <- irf_plot %>%
  # - only keeping IRF that don't cross 0
  filter(day == 15) %>%
  arrange(out, cov, pe) %>%
  mutate(label = factor(label, levels = unique(label)))

# AVG topic attention
#===============================================================================
# - calculating average attention to each topic by each group
db <- db %>%
  filter(topic %in% pol_issues) %>%
  mutate(random = random_us) %>%
  dplyr::select(-random_us)


# - adding human readble topic labels to the dataset
db$topic <- as.character(db$topic)
pa2our <- pa2our %>%
  rename(topic = our_, label = our_topic_label) %>%
  dplyr::select(topic, label)
pa2our$topic <- as.character(pa2our$topic)

db <- left_join(db, pa2our)

# - calculating the average by political issue and by group
db_long <- db %>%
  dplyr::select(label, rep, dem, pubdem, pubrep, public, random, media) %>%
  gather(group, att, -label)

out_db <- db_long %>%
  group_by(group, label) %>%
  summarize(av = mean(att, na.rm = TRUE)) %>%
  as.data.frame()

# - provide readble labels to the groups
out_db$group <- recode(factor(out_db$group),
                       `dem` = "Democratic MCs",
                       `rep` = "Republican MCs",
                       `pubdem` = "Democratic\nSupporters",
                       `pubrep` = "Republican\nSupporters",
                       `public` = "Attentive\nPublic",
                       `random` = "General\nPublic",
                       `media` = "Media")

# - re-level the group variable: first the political agendas and then the public
#   ones
out_db$group <- factor(out_db$group,
                       levels = c(
                         "Deomcratic MCs",
                         "Republican MCs",
                         "Democratic\nSupporters",
                         "Republican\nSupporters",
                         "Attentive\nPublic",
                         "General\nPublic",
                         "Media"
                       ))

# - sort the labels in descending order by Democrats in Congress attention
out_db <- out_db %>%
  arrange(group, av)

out_db$label <- factor(out_db$label, levels = unique(out_db$label))

out_db <- out_db %>%
  rename(cov = group)

out_db$cov <- as.character(out_db$cov)
out_db$label <- as.character(out_db$label)
irf_toplot$cov <- as.character(irf_toplot$cov)
irf_toplot$label <- as.character(irf_toplot$label)

main <- left_join(irf_toplot, out_db) %>%
  mutate(label_av = NA,
         label_av = ifelse(
           (cov == "Democratic\nSupporters" & out == "Democratic MCs" &
              label %in% c("Gun Violence")) |#, "Affordable Care Act")) |
             (cov == "Republican\nSupporters" & out == "Republican MCs" &
                label %in% c("IRS Scandal")),
           #"Obamacare (Website and Implementation)")),
           as.character(label), label_av),
         label_av = ifelse(grepl("IRS", label_av), "IRS", 
                           as.character(label_av))) %>%
  mutate(pe = (pe * 100)/10, lwr = (lwr * 100)/10, 
         upr = (upr * 100)/10, av = av * 100)

main$cov <- factor(main$cov,
                       levels = rev(c(
                         "Democratic MCs",
                         "Republican MCs",
                         "Democratic\nSupporters",
                         "Republican\nSupporters",
                         "Attentive\nPublic",
                         "General\nPublic",
                         "Media"
                       )))

# PLOT -- FIGURE 5
#===============================================================================
png("images/figure5.png", width = 1200, height = 600)
ggplot(main %>%
         filter(out %in% c("Democratic MCs", "Republican MCs")),
       aes(x = av , y = pe, ymin = lwr, ymax = upr, 
           col = cov, fill = cov)) +
  geom_pointrange(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, alpha = 0.4) +
  geom_hline(yintercept = 0) +
  ylab("15-day Impulse Response Functions \n(in percentage points)\n") +
  xlab("\nAverage daily attention during the 113th Congress") +
  scale_color_manual("",
                    values = c("gray30", "orange2", "red2", "blue2")) +
  scale_fill_manual("",
                   values = c("gray30", "orange2", "red2", "blue2")) +
  scale_x_continuous(breaks = seq(0, 5, 1),
                     labels = paste0(seq(0, 5, 1), "%")) +
  facet_grid(out ~ cov) +
  geom_text_repel(aes(label = label_av), col = "black", size = 5,
                 ylim = c(2.3, 3)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90"),
    strip.text = element_text(size = 14),
    strip.background = element_rect(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.spacing = unit(2, "lines")
  )
dev.off()
