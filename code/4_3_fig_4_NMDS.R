##############################################

# 4.3 Fig. 4 NMDS plot

##############################################

# Run first community composition script
# source("3_3_community_composition.R")

# Prep for plotting ------------------------------------------------------------

# Get habitat
status_type <- locality_ab_EPT %>%
  dplyr::distinct(Status,locality) %>%
  dplyr::mutate(across(.cols = c(locality,Status), .fns = as.factor)) %>%
  dplyr::arrange(Status,locality)

# Extract NMDS scores for sites (localities)
nmds_SiteScores <- as.data.frame(scores(nmds)$sites) %>% # get nmds scores 
  tibble::rownames_to_column(var = "locality") %>%  # change rownames (site) to a column 
  left_join(status_type) # join habitat type (grouping variable) to each site 
# Extract NMDS scores for species  
nmds_SpeciesScores <- as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
Status_Centroid <- nmds_SiteScores %>% 
  dplyr::group_by(Status) %>% 
  dplyr::summarise(axis1 = mean(NMDS1), axis2 = mean(NMDS2)) %>% 
  ungroup()

# Rename lakes
nmds_SiteScores <- nmds_SiteScores %>%
  mutate(locality = dplyr::recode(locality,
                                  "Kilvatnet" = "Jonsvatnet",
                                  "Gjoljavatnet" = "Gjøljavatnet",
                                  "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

# extract convex hull
habitat_hull <- nmds_SiteScores %>% 
  dplyr::group_by(Status) %>%
  dplyr::slice(chull(NMDS1, NMDS2)) %>%
  ungroup()

nmds_stress <- nmds$stress

# Plot -------------------------------------------------------------------------

# Set colors
control_color <- "#E6E6E6"
regulated_color <- "#7A7A7A"

# use ggplot to plot 
jpeg(here::here("results","figures","fig_4_NMDS.jpg"), 
     width = 18, height = 12, units="cm", res=300)

ggplot() + 
  # add species scores
  geom_point(data = nmds_SpeciesScores, 
             aes(x=NMDS1, y=NMDS2), colour = "darkgrey", size = 1) +
  # add convex hull
  geom_polygon(data = habitat_hull, 
               aes(x = NMDS1, y = NMDS2, fill = Status, group = Status), 
               alpha = 0.6) +
  # add centroid 
  geom_point(data = Status_Centroid, 
             aes(x = axis1, y = axis2, color = Status), 
             size = 5, shape = 17) +
  # add site scores
  geom_point(data = nmds_SiteScores, 
             aes(x=NMDS1, y=NMDS2, colour = Status), size = 2) + 
  geom_text(data = nmds_SiteScores, 
            aes(x=NMDS1, y=NMDS2, label = locality), size = 3) +
  # add stress value
  annotate("text", x = 0.5, y = 0.65, 
           label = paste("2d stress =", round(nmds_stress, 3))) +
  # edit theme
  labs(x = "NMDS1", y = "NMDS2") + 
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, linewidth = .5),
        axis.line = element_line(color = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(.25, "cm")) +
  scale_color_manual(values = c("regulated" = regulated_color, "control" = control_color)) +
  scale_fill_manual(values  = c("regulated" = regulated_color, "control" = control_color))+
  scale_x_continuous(expand = expansion(add = (c(0,0.1))))


dev.off()
