####################################################

# 5.6 Fig S6 locomotion and armour abundance barplot

####################################################

# Create dataframes ------------------------------------------------------------

## Locomotion -----

# First summarize individualcount for each species and locality
locomotion_lake_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::select(Status,locality,internal_eventID,scientificName,individualCount,locomotion) %>%
  dplyr::group_by(locality,scientificName) %>%
  dplyr::summarise(Status = paste0(unique(Status), sep = ""),
                   locomotion = paste0(unique(locomotion), sep = ""),
                   individualCount_species = sum(individualCount)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(.cols = c(Status,locality,locomotion), .fns = as.factor))

# Order localities
locomotion_lake_df$locality <- factor(locomotion_lake_df$locality, 
                                      levels = c("Barsetvatnet","Roksetvatnet","Kilvatnet",
                                                 "Gjoljavatnet","Storvatnet","Stor-Drakstsjoen"))
# Rename localities
locomotion_lake_df <- locomotion_lake_df %>%
  mutate(locality = dplyr::recode(locality,
                                  "Kilvatnet" = "Jonsvatnet",
                                  "Gjoljavatnet" = "Gjøljavatnet",
                                  "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

## Armour ----

# Summarise species per lake
armour_lake_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(locality,scientificName) %>%
  dplyr::summarise(Status = paste0(unique(Status), sep = ""),
                   order = paste0(unique(order), sep = ""),
                   material = paste0(unique(material), sep = ""),
                   individualCount_species = sum(individualCount)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(.cols = c(Status,locality,material), .fns = as.factor))

# Order localities
armour_lake_df$locality <- factor(armour_lake_df$locality, 
                                  levels = c("Barsetvatnet","Roksetvatnet","Kilvatnet",
                                             "Gjoljavatnet","Storvatnet","Stor-Drakstsjoen"))
# Rename localities
armour_lake_df <- armor_lake_df %>%
  mutate(locality = dplyr::recode(locality,
                                  "Kilvatnet" = "Jonsvatnet",
                                  "Gjoljavatnet" = "Gjøljavatnet",
                                  "Stor-Drakstsjoen" = "Stor-Drakstsjøen"))

# Order levels of material
armour_lake_df$material <- factor(armour_lake_df$material, levels = c("sclerotized","silk","mineral","mixed","vegetation"))


# Figures ----------------------------------------------------------------------

# Locomotion
locomotion_colors <- c("#8C510A","#DFC27D","#80CDC1", "#01665E")

p2_locomotion <- ggplot(locomotion_lake_df, aes(fill = locomotion, y = individualCount_species, x = locality)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(fill = "Locomotion", x = "Lake", y = "Abundance") +
  scale_fill_brewer(palette = "BrBG", labels = c("bur","crw","tat","wsw"))+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  theme_classic()+
  theme(legend.position = "top",
        plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = "cm"),
        legend.text = element_text(size = 8))+
  facet_wrap(~Status, scales = "free_x")


armour_colors <- c("#F6E8C3","#DFC27D","#8C510A","#35978F","#01665E")

p2_armour <- ggplot(armour_lake_df, aes(fill = material, y = individualCount_species, x = locality)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(fill = "Armour", x = "Lake", y = "Abundance") +
  scale_fill_manual(values = armour_colors, labels = c("scl","sil","min","mix","veg"))+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  theme_classic()+
  theme(legend.position = "top", 
        plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = "cm"),
        legend.text = element_text(size = 8))+
  facet_wrap(~Status, scales = "free_x")

# One figure
jpeg(here::here("results","figures","fig_S6_barplot_traits_abundance.jpg"), width = 19, height = 12, units="cm", res=300)

combined_plot <- ggpubr::ggarrange(p2_locomotion,p2_armour,ncol = 2, nrow = 1, labels = c("A","B"))
combined_plot

dev.off()

