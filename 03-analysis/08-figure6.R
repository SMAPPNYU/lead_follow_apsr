#===============================================================================
#  File:    08-figure6.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 6 of the paper, where we show the ability of the
#           the different groups under study to lead the agenda of the media,
#           and viceversa.
#  Data In: 
#           # Information about the 15-day IRFs for a one-time and a permanent
#             10-point increase in attention
#           - ./var/onetime-structural-shock-irfs-results.csv
#  Data Out: 
#           # Figure 6
#           - ./images/figure6.png
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(rio)
library(ggplot2)

# DATA
#===============================================================================
db <- import("var/onetime-structural-shock-irfs-results.csv")

# DATA WRANLGING
#===============================================================================
# - exploring 15-day effects only
plot_db_00 <- db %>%
  filter(day == 15) %>%
  # - looking only at one time 10 point effects
  filter(data_type == 'Effect of a one time 10 percentage point attention increase at day 0') %>%
  # - only exploring the MEDIA EFFECTS
  filter(cov == "Media") %>%
  mutate(data_type = "Media --> (group)") %>%
  rename(x = cov, y = out)

# - exploring 15-day effects only
plot_db_01 <- db %>%
  filter(day == 15) %>%
  # - looking only at one time 10 point effects
  filter(data_type == 'Effect of a one time 10 percentage point attention increase at day 0') %>%
  # - only exploring HOW MUCH THE PUBLIC INFLUENCE THE MEDI
  filter(out == "Media") %>%
  mutate(data_type = "(group) --> Media") %>%
  rename(x = out, y = cov)

plot_db <- rbind(plot_db_00, plot_db_01)

# - relabel the covariates so they fit/look better in the plot
plot_db$y <- recode(plot_db$y,
                    `Democrats in Congress` = "Democrats in Congress",
                    `Republicans in Congress` = "Republicans in Congress",
                    `Democratic Supporters` = "Democratic Supporters",
                    `Republican Supporters` = "Republican Supporters",
                    `Attentive Public` = "Attentive Public",
                    `General Public` = "General Public")

# - reordering the covariate and outcome categories
plot_db$y <- factor(plot_db$y,
                    levels = rev(c("Democrats in Congress",
                                   "Republicans in Congress",
                                   "Democratic Supporters",
                                   "Republican Supporters",
                                   "Attentive Public",
                                   "General Public", 
                                   "Media")))

plot_db$x <- factor(plot_db$x,
                    levels = c("Media --> (group)", "(group) --> Media"))



# PLOT
#===============================================================================
png("images/figure6.png", width = 1200, height = 400)
ggplot(plot_db,
       aes(x = y, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(aes(x = y, xend = y, y = lwr, yend = upr), 
               size = 4, alpha = 0.6) +
  #geom_segment(alpha = 0.8, size = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(~data_type) +
  coord_flip() +
  geom_vline(xintercept = 15) +
  xlab("") +
  scale_y_continuous("\nThe 15-day effect of a one time 10 percentage point increase in attention in day 0",
                     expand = c(0,0.001)) +
  theme(
    panel.spacing = unit(1.05, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(hjust=0),
    strip.text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 16)
  )
dev.off()
