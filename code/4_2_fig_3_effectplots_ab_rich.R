#####################################################

# 4.2 Fig. 3 effect plots for abundance & richness

#####################################################

# Effectplot -------------------------------------------------------------------

## Shape symbols ----
lake_shapegroup <- function(lake_name){
  case_when(
    lake_name %in% c("Roksetvatnet", "Storvatnet") ~ "Roksetvatnet/Storvatnet",
    lake_name %in% c("Barsetvatnet", "Gjoljavatnet") ~ "Barsetvatnet/Gjøljavatnet",
    lake_name %in% c("Jonsvatnet","Kilvatnet","Stor-Drakstsjoen") ~ "Jonsvatnet/Stor-Drakstsjøen",
    .default = c("Unknown group")
  )
}

lake_shapegroup_symbols <- c("Roksetvatnet/Storvatnet" = 24, # triangle
                             "Barsetvatnet/Gjøljavatnet" = 21, # circle
                             "Jonsvatnet/Stor-Drakstsjøen" = 22) # square

## Species abundance ----

# Dataframe
event_ab_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   eventDate = as.Date(paste0(unique(eventDate), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "),
                   sum_individualCount = as.numeric(sum(individualCount[taxonRank == "SPECIES"], na.rm = TRUE))
  )

# Order levels in year factor
event_ab_df$year <- factor(event_ab_df$year, levels = c("2023","2024"))

# Run top ranked model
Abundance_nb_3 <- lme4::glmer.nb(formula = sum_individualCount ~ Status + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

# Extract and save effects
# Extract estimated means
emm_status <- emmeans::emmeans(Abundance_nb_3, ~ Status, type = "response") 
emm_status_ab_df <- as.data.frame(emm_status)

# Add shape group to the df
event_ab_df$shape_group <- lake_shapegroup(event_ab_df$locality)

# Plot
fig_emmeans_ab_mod_3 <- ggplot(event_ab_df, aes(x = Status, 
                                                y = sum_individualCount, 
                                                color = Status,
                                                fill = Status,
                                                shape = shape_group))+
  geom_jitter(width = 0.1, alpha = 0.7, size = 2)+
  scale_color_manual(values = c("darkgrey","black"))+
  scale_fill_manual(values = c("#E6E6E6","#7A7A7A"))+
  scale_shape_manual(name = "Lake", values = lake_shapegroup_symbols) +
  theme_classic()+
  theme(legend.position = "none")

fig_emmeans_ab_mod_3 <- fig_emmeans_ab_mod_3 +
  geom_point(data = emm_status_ab_df,
             aes(x = Status, y = response),
             inherit.aes = FALSE,
             size = 3, 
             color = "black") +
  geom_errorbar(data = emm_status_ab_df,
                aes(x = Status, ymin = asymp.LCL, ymax = asymp.UCL),
                inherit.aes = FALSE,
                width = 0.1) +
  labs(y = "Abundance",
       x = "Regulation status")

p1_marg <- ggMarginal(fig_emmeans_ab_mod_3, 
                      groupColour = TRUE, 
                      groupFill = TRUE, 
                      margins = "y")


## Species richness ----

# Dataframe
event_sp_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "),
                   N_species = length(unique(taxonKey[taxonRank == "SPECIES"], na.rm = TRUE)))

# Order levels in year factor
event_sp_df$year <- factor(event_sp_df$year, levels = c("2023","2024"))

# Run top ranked model
## Best model: status as explanatory variable
SpRich_poisson_2 <- lme4::glmer(formula = N_species ~ Status + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")

# Extract and save effects
emm_status <- emmeans::emmeans(SpRich_poisson_2, ~ Status, type = "response") 
emm_status_sp_df <- as.data.frame(emm_status)

# Add shape group to the df
event_sp_df$shape_group <- lake_shapegroup(event_sp_df$locality)

# Plot
fig_emmeans_spRich_mod_2 <- ggplot(event_sp_df, aes(x = Status, 
                                                    y = N_species, 
                                                    color = Status,
                                                    fill = Status,
                                                    shape = shape_group))+
  geom_jitter(width = 0.1, alpha = 0.7, size = 2)+
  scale_color_manual(values = c("darkgrey","black"))+
  scale_fill_manual(values = c("#E6E6E6","#7A7A7A"))+
  scale_shape_manual(name = "Lake", values = lake_shapegroup_symbols) +
  theme_classic()+
  theme(legend.position = "none")

fig_emmeans_spRich_mod_2 <- fig_emmeans_spRich_mod_2 +
  geom_point(data = emm_status_sp_df,
             aes(x = Status, y = rate),
             inherit.aes = FALSE,
             size = 3, 
             color = "black") +
  geom_errorbar(data = emm_status_sp_df,
                aes(x = Status, ymin = asymp.LCL, ymax = asymp.UCL),
                inherit.aes = FALSE,
                width = 0.1) +
  scale_y_continuous(breaks = seq(0,15, by = 5))+
  coord_cartesian(ylim = c(0,15)) +
  labs(y = "Species richness",
       x = "Regulation status")

p2_marg <- ggMarginal(fig_emmeans_spRich_mod_2, 
                      groupColour = TRUE, 
                      groupFill = TRUE, 
                      margins = "y")

## Save as one figure ----

jpeg(here::here("results","figures","fig_3_effectplots_ab_rich.jpg"), 
     width = 16, height = 10, units="cm", res=300)

combined_fig <- cowplot::plot_grid(p1_marg,p2_marg, 
                          ncol = 2, 
                          align = "h",
                          labels = c("A","B"))
combined_fig

dev.off()
