##############################################

# 4.1 Fig. 2 rarefaction curve

##############################################

# Sample-based rarefaction curves that show the accumulation of species with
# increasing number of samples

## Create community matrix ----

# Create community matrix (lacking events with zero counts, so n events = 56)
comm <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(locality, internal_eventID, scientificName) %>%
  dplyr::summarise(abundance = sum(individualCount), .groups = "drop") %>%
  pivot_wider(
    names_from = scientificName,
    values_from = abundance,
    values_fill = 0
  )

# Add on events that have zero reported species counts (n events = 60)
# all events
all_events <- occurrences_df %>%
  dplyr::select(locality,internal_eventID) %>%
  dplyr::distinct()

# missing events added
comm <- full_join(comm,all_events, by = c("locality","internal_eventID")) %>%
  replace(is.na(.), 0)

# split by lake and run specaccum in one step
accum_list <- by(comm, comm$locality, function(x) {
  vegan::specaccum(x[,-c(1,2)], method="random")
})

# Combine and make dataframe
accum_df <- do.call(rbind, lapply(names(accum_list), function(name) {
  acc <- accum_list[[name]]
  data.frame(
    sites = acc$sites,
    richness = acc$richness,
    sd = acc$sd,
    Locality = name
  )
}))

# Locality as factor
accum_df$Locality <- as.factor(accum_df$Locality)

# Colors
# Old blue yellow: "#56B4E9" "#E69F00"
control_color <- "#E6E6E6"
regulated_color <- "#7A7A7A"


# Rename lake
accum_df <- accum_df %>%
  mutate(Locality = dplyr::recode(Locality,
                                  "Kilvatnet" = "Jonsvatnet",
                                  "Gjoljavatnet" = "Gjøljavatnet",
                                  "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

# Manually specify colors
mycols <- c(
  "Barsetvatnet" = control_color, 
  "Jonsvatnet" = control_color,
  "Roksetvatnet" = control_color,  
  "Gjøljavatnet" = regulated_color,  
  "Storvatnet" = regulated_color,  
  "Stor-Drakstsjøen" = regulated_color   
)

# Get last point for each curve
label_df <- accum_df %>%
  group_by(Locality) %>%
  slice_max(sites)

# Create figure
fig_sp_acc_curve <- ggplot(accum_df, aes(x = sites, y = richness, color = Locality)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd, fill = Locality),
              alpha = 0.7, colour = NA) +
  scale_color_manual(values = mycols) +
  scale_fill_manual(values = mycols) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14), 
                     expand = expansion(add = (c(0,3))))+
  geom_text(
    data = label_df,
    aes(label = Locality),
    color = "black",
    hjust = -0.1,
    show.legend = FALSE,
    size = 5
  ) +
  labs(x = "Number of samples", y = "Species richness") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") 


# Save figure
jpeg(here::here("results","figures","fig_2_rarefaction_curve.jpg"), 
     width = 21, height = 14, units="cm", res=300)

fig_sp_acc_curve

dev.off()