###################################################

# 5.5 Fig S5 caterpillar plots abundance & richness

###################################################

# Caterpillar plots, random effects structure for highest ranked models. 
# Including 95% CI, the use can be discussed for random effects.

# Abundance --------------------------------------------------------------------

# Add the events missing EPT species records
event_ab_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   eventDate = as.Date(paste0(unique(eventDate), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "), # should this be factor or integer?
                   sum_individualCount = as.numeric(sum(individualCount[taxonRank == "SPECIES"], na.rm = TRUE))
  )

# Order levels in year factor
event_ab_df$year <- factor(event_ab_df$year, levels = c("2023","2024"))

# Run model
Abundance_nb_3 <- lme4::glmer.nb(formula = sum_individualCount ~ Status + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

# Extract random effects and convert to a data frame
random_effects_df <- as.data.frame(ranef(Abundance_nb_3))

# Calculate 95% confidence intervals
random_effects_df <- random_effects_df %>%
  mutate(
    lower_ci = condval - 1.96 * condsd,
    upper_ci = condval + 1.96 * condsd
  )

# Order the subjects by random effect value
random_effects_df$grp <- forcats::fct_reorder(random_effects_df$grp, random_effects_df$condval)

# Rename lakes
random_effects_df <- random_effects_df %>%
  mutate(grp = dplyr::recode(grp,
                             "Kilvatnet" = "Jonsvatnet",
                             "Gjoljavatnet" = "Gjøljavatnet",
                             "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

# Plot
p1 <- ggplot(random_effects_df, aes(x = grp, y = condval, ymin = lower_ci, ymax = upper_ci)) +
  geom_pointrange() +
  coord_flip() + # Makes it a horizontal plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  scale_y_continuous(limits = c(-1.5,2), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5))+
  labs(
    x = "Lake",
    y = "Random effect estimate (mean \u00B1 95% CI)"
  ) +
  theme_classic()

# Richness ---------------------------------------------------------------------

# Create dataframe with data grouped by sampling event
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

## Best model: status as explanatory variable
SpRich_poisson_2 <- lme4::glmer(formula = N_species ~ Status + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")

# Extract random effects and convert to a data frame
random_effects_df2 <- as.data.frame(ranef(SpRich_poisson_2))

# Calculate 95% confidence intervals
random_effects_df2 <- random_effects_df2 %>%
  mutate(
    lower_ci = condval - 1.96 * condsd,
    upper_ci = condval + 1.96 * condsd
  )

# Order the subjects by random effect value
random_effects_df2$grp <- forcats::fct_reorder(random_effects_df2$grp, random_effects_df2$condval)

# Rename lakes
random_effects_df2 <- random_effects_df2 %>%
  mutate(grp = dplyr::recode(grp,
                             "Kilvatnet" = "Jonsvatnet",
                             "Gjoljavatnet" = "Gjøljavatnet",
                             "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

# Plot
p2 <- ggplot(random_effects_df2, aes(x = grp, y = condval, ymin = lower_ci, ymax = upper_ci)) +
  geom_pointrange() +
  coord_flip() + # Makes it a horizontal plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Adds a vertical line at 0
  scale_y_continuous(limits = c(-1.5,2), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5))+
  labs(
    x = "Lake",
    y = "Random effect estimate (mean \u00B1 95% CI)"
  ) +
  theme_classic()

# Combine figures --------------------------------------------------------------

jpeg(here::here("results","figures","fig_S5_caterpillarplot_ab_rich.jpg"), 
     width = 22, height = 8, units="cm", res=300)

combined_plot <- ggpubr::ggarrange(p1,p2,ncol = 2, nrow = 1, labels = c("A","B"))
combined_plot

dev.off()
