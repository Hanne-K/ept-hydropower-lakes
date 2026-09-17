#############################################

# 6.2 sample variability check

#############################################

# Evaluating the contribution of spatial distance to community differences

# Distance-decay relationship --------------------------------------------------

# With a simple linear regression using all pairwise distances is not 
# statistically valid because the pairwise comparisons are not independent.
# Therefore, the distance-decay relationship is only used to produce a descriptive figure.

## Community matrix (abundances) ----

# Total sampling effort per lake           
sampling_effort_loc <- occurrences_highertaxa_df %>%
  group_by(locality) %>%
  summarize(sampling_effort_loc = length(unique(internal_eventID)))

# Create dataframe
locality_ab_EPT <- occurrences_df %>%
  filter(taxonRank == "SPECIES") %>%
  group_by(locality,scientificName) %>%
  summarise(individualCount_species = sum(individualCount),
            Status = paste0(unique(Status), sep = ""))

# Add sampling effort
locality_ab_EPT <- merge(x = locality_ab_EPT, y = sampling_effort_loc) 

# Create community matrix
com_matrix <- locality_ab_EPT %>%
  dplyr::mutate(across(.cols = c(locality,Status,scientificName), .fns = as.factor)) %>%
  pivot_wider(names_from = scientificName, # pivot wide
              values_from = individualCount_species,
              values_fill = 0) %>%
  tibble::column_to_rownames(var = "locality") # change our column "site" to our rownames

# Bray-curtis
bc <- vegdist(com_matrix[,2:46], method = "bray")

## Calculating geographical distances ----

coordinate_df <- occurrences_df %>%
  dplyr::select(locality,decimalLatitude,decimalLongitude) %>%
  dplyr::distinct()

distances <- geosphere::distm(coordinate_df[, c("decimalLongitude","decimalLatitude")])
distances <- as.dist(distances)

# Distance-decay plot ----

# df
dist_decay_df <- data.frame(
  geog_dist = as.vector(distances),
  bray_curtis = as.vector(bc)
)

# Plot
ggplot(dist_decay_df, aes(geog_dist, bray_curtis)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(x = "Geographic distance (m)",
       y = "Bray-Curtis dissimilarity")

# Mantel test ----
mantel(bc,
       distances,
       method = "pearson",
       permutations = 9999)

# Mantel statistic r: 0.1296 
# Significance: 0.24722 