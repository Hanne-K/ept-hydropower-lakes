##############################################

# 4.4 Fig. 5 Forest plot traits

##############################################

# Run first trait scripts
# source("3_6_trait_locomotion.R")
# source("3_7_trait_armour.R")

# Figure: Forest plot ----------------------------------------------------------

# Aim: Visualize model results using a forest plot. Show the CI on log scale,
# to make them symmetrical and correctly show the uncertainty

# Sources used to make decisions on plotting scale
# https://github.com/YzwIsALaity/Forest-Plot-Tutorial-in-R
# https://andrewpwheeler.com/2013/10/26/odds-ratios-need-to-be-graphed-on-log-scales/

## Data preparations ----

# Plotting the log odds ratios from these summary dfs, from scripts 3_6 and 3_7
results_loc_df
results_armour_df

# Rename factor levels
results_loc_df <- results_loc_df %>%
  dplyr::mutate(locomotion = dplyr::recode(locomotion,
                                           "wsw" = "full water swimmer",
                                           "tat" = "temporarily attached",
                                           "crw" = "crawler",
                                           "bur" = "burrower"))

# Reorder by ratio value to make reading plot easier
results_loc_df <- results_loc_df %>%
  dplyr::arrange(desc(ratio)) %>%
  dplyr::mutate(locomotion = reorder(locomotion,-ratio))

results_armour_df <- results_armour_df %>%
  dplyr::arrange(desc(ratio)) %>%
  dplyr::mutate(material = reorder(material,-ratio))


## Figures ----

# Locomotion
p1 <- ggplot(results_loc_df, aes(x = ratio, y = locomotion)) +
  geom_point(shape = 18, size = 3) +                            
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),                  
                 height = 0.25) +                                   
  scale_x_continuous(trans = 'log',                                 
                     limits = c(0.03, 330),
                     labels = scales::label_number(),
                     breaks = c(0.1,1,10,100,1000)) +  
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 1,                                        
             color = "red",                                         
             linetype = "dashed",                                   
             alpha = 0.5)+
  theme_classic() +
  theme()

# Armour
p2 <- ggplot(results_armour_df, aes(x = ratio, y = material)) +
  geom_point(shape = 18, size = 3) +                            
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),                  
                 height = 0.25) +                                   
  scale_x_continuous(trans = 'log',                                 
                     limits = c(0.03, 330),
                     labels = scales::label_number(),
                     breaks = c(0.1,1,10,100,1000)) +  
  labs(x = "Ratio (control / regulated)", y = NULL)+
  geom_vline(xintercept = 1,                                        
             color = "red",                                         
             linetype = "dashed",                                   
             alpha = 0.5)+
  theme_classic()


# One figure, size to fit a4 page
jpeg(here::here("results","figures","fig_5_forestplot_traits.jpg"), 
     width = 19, height = 12, units="cm", res=300)

combined_plot <- ggpubr::ggarrange(p1,p2,ncol = 1, nrow = 2, align = "v", labels = c("A","B"))
combined_plot

dev.off()