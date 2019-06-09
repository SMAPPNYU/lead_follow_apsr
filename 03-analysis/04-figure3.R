#===============================================================================
#  File:    04-figure3.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: to replicate Figure 3 of the paper, where we explore in more detail
#           the ability of politicians v. groups of the public to lead the 
#           agenda of the other; and viceversa.
#  Data In: 
#           # Information about the 15-day IRFs for a one-time and a permanent
#             10-point increase in attention
#           - ./var/onetime-structural-shock-irfs-results.csv
#  Data Out: 
#           # A copy of Figure 3
#           - ./images/figure3.png
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
# - exploring only 15-day one-time effects only, and excluding the Media
new_db <- db %>%
  filter(day == 15,
         grepl("one time", data_type),
         cov != "Media",
         out != "Media") 

# - adding a new varaible, whether the covariate belongs to a public or 
#     politician group
new_db <- new_db %>%
  mutate(agenda_type = ifelse(grepl("Congress", cov), "political", "public"))

# - a list of the groups in the two different type of agendas
public_groups <- unique(new_db$cov[new_db$agenda_type == "public"])
political_groups <- unique(new_db$cov[new_db$agenda_type == "political"])

# - preparing the data to plot
plot_db <- NULL
for (pubgroup in public_groups) {
  for (polgroup in political_groups) {
    # - pulling the political response to the public change and viceversa
    pub_to_pol_db <- new_db %>% 
      filter(cov == pubgroup, out == polgroup)
    pol_to_pub_db <- new_db %>%
      filter(cov == polgroup, out == pubgroup)
    
    # - combining the responses, adding some new variables, and adding it to
    #   the new dataset to plot
    new_row = rbind(
      data.frame(
        pubgroup = pubgroup,
        polgroup = polgroup,
        direction = "Political response to a change in the Public agenda",
        pe = pub_to_pol_db$pe,
        lwr = pub_to_pol_db$lwr,
        upr = pub_to_pol_db$upr
      ),
      data.frame(
        pubgroup = pubgroup,
        polgroup = polgroup,
        direction = "Public response to a change in the Political agenda",
        pe = pol_to_pub_db$pe,
        lwr = pol_to_pub_db$lwr,
        upr = pol_to_pub_db$upr
      )
    )
    plot_db <- rbind(plot_db, new_row)
  }
}

# - reordering the covariate and outcome categories
plot_db$pubgroup <- recode(plot_db$pubgroup,
                           `Democratic Supporters` = "Democratic\nSupporters",
                           `Republican Supporters` = "Republican\nSupporters",
                           `Attentive Public` = "Attentive\nPublic",
                           `General Public` = "General\nPublic")

plot_db$pubgroup <- factor(plot_db$pubgroup,
                           levels = c("Democratic\nSupporters",
                                      "Republican\nSupporters",
                                      "Attentive\nPublic",
                                      "General\nPublic"))


plot_db$polgroup <- recode(plot_db$polgroup,
                           `Democrats in Congress` = "Democrats\nin Congress",
                           `Republicans in Congress` = "Republicans\nin Congress")


plot_db$polgroup <- factor(plot_db$polgroup,
                           levels = rev(c("Democrats\nin Congress",
                                          "Republicans\nin Congress")))


# PLOT
#===============================================================================
png("images/figure3.png", width = 1600, height = 400)
ggplot(plot_db,
       aes(x = polgroup, y = pe, ymin = lwr, ymax = upr, col = direction)) +
  geom_segment(aes(x = polgroup, xend = polgroup, y = lwr, yend = upr), 
               size = 4, alpha = 1) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ pubgroup, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("\n15-day effect of a one time 10 percentage point increase") +
  scale_color_manual("", values = c("gray70", "gray30")) +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 20),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(fill = "gray80", color = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text.y = element_text(hjust=0)
  )
dev.off()
