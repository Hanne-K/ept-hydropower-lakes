##############################################

# 3.3 Community composition

##############################################

# Create community matrix ------------------------------------------------------

# Df summarizing the abundance of each species per locality 

# Create df
locality_ab_EPT <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(locality,scientificName) %>%
  dplyr::summarise(individualCount_species = sum(individualCount),
            Status = paste0(unique(Status), sep = "")) %>%
  dplyr::ungroup()

# Create community matrix
com_matrix <- locality_ab_EPT %>%
  dplyr::mutate(across(.cols = c(locality,Status,scientificName), .fns = as.factor)) %>%
  tidyr::pivot_wider(names_from = scientificName, # pivot wide
              values_from = individualCount_species,
              values_fill = 0) %>%
  tibble::column_to_rownames(var = "locality") # change our column "site" to our rownames

head(com_matrix)

# Run NMDS ---------------------------------------------------------------------

set.seed(2) # fixed seed for reproducibility
nmds <- vegan::metaMDS(com_matrix[,2:46],
                       distance = "bray",
                       k = 2) # 2 dimensions
# by default square-root transforms, followed by Wisconsin double standardization
print(nmds)
plot(nmds)

# Evaluate fit: Shepard plot
plot(nmds$diss,nmds$dist)

# PERMANOVA --------------------------------------------------------------------

# 1. Distance matrix
dist_matrix <- vegan::vegdist(com_matrix[,2:46], method = "bray")
print(dist_matrix)

# 2. PERMANOVA
# Using adonis from vegan package
permanova_result <- adonis2(com_matrix[,2:46] ~ com_matrix$Status, permutations = 999)
print(permanova_result2)

# 3. PERMDISP
# Testing homogeneity of group dispersions
disp <- betadisper(dist_matrix, com_matrix$Status)
anova(disp) # parametric
vegan::permutest(disp) # permutation, reported
