##############################################

# 3.4 Beta diversity partitioning

##############################################

# Create pa matrix -------------------------------------------------------------

# Prepare presence-absence dataframe where data is summarized per locality 

com_matrix_pa <- occurrences_df %>% # species counts for each locality
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(Status,locality,scientificName) %>%
  dplyr::summarise(individualCount_species = sum(individualCount)) %>%
  ungroup() %>%
  dplyr::mutate(across(.cols = c(Status,locality,scientificName), .fns = as.factor)) %>% # make factors
  pivot_wider(names_from = scientificName, # pivot wider
              values_from = individualCount_species,
              values_fill = 0) %>% # add 0 for all zero counts
  tibble::column_to_rownames(var = "locality") %>% # change our column "site" to our rownames
  dplyr::arrange(Status) %>% # arrange to make sure our grouping variable is in order: control, regulated
  dplyr::select(-Status) %>% # remove to create the matrix
  dplyr::mutate(across(everything(), ~ ifelse(. > 0,1,0)))

# Total beta -------------------------------------------------------------------

# Computes the basic quantities needed for computing the multiple-site beta diversity measures and pairwise dissimilarity matrices.
beta_all <- betapart::betapart.core(com_matrix_pa)

beta_all$sumSi
beta_all$St # 45 species in total in the dataset

# Total beta for all lakes
beta_all_multi <- beta.multi(beta_all, index.family = "jaccard")
beta_all_multi
#$beta.JTU
#[1] 0.7156863

#$beta.JNE
#[1] 0.1161978

#$beta.JAC
#[1] 0.8318841

# Percent turnover
(0.7156863/0.8318841)*100

# Within group beta ------------------------------------------------------------
# Subset presence–absence data by group
#group <- c("regulated","regulated","regulated","unregulated","unregulated","unregulated")

group <- c("control","control","control","regulated","regulated","regulated")


pa_control <- com_matrix_pa[group == "control", ]
pa_regulated  <- com_matrix_pa[group == "regulated", ]

# Compute betapart core objects
beta_control_core <- betapart.core(pa_control)
beta_regulated_core  <- betapart.core(pa_regulated)

# Calculate multi-site Jaccard partitioning
beta_control <- beta.multi(beta_control_core, index.family = "jaccard")
beta_regulated  <- beta.multi(beta_regulated_core, index.family = "jaccard")

# Results for each group
beta_control
beta_regulated

# percentages
beta_control$beta.JTU/beta_control$beta.JAC
beta_regulated$beta.JTU/beta_regulated$beta.JAC


# Between group beta partitioning ----------------------------------------------

# Corrected code, removing error message
# No built-in function, need to do it manually
beta_parts <- betapart::beta.pair(com_matrix_pa, index.family = "jaccard")

# Convert dist to matrix
beta_jtu <- as.matrix(beta_parts$beta.jtu)
beta_jne <- as.matrix(beta_parts$beta.jne)
beta_jac <- as.matrix(beta_parts$beta.jac)

group <- as.vector(group)

pairwise_groups <- combn(unique(group), 2, simplify = FALSE)

for (g in pairwise_groups) {
  sites_g1 <- which(group == g[1])
  sites_g2 <- which(group == g[2])
  
  between_pairs <- expand.grid(sites_g1, sites_g2)
  
  turnover_mean <- mean(beta_jtu[cbind(between_pairs$Var1, between_pairs$Var2)])
  nested_mean   <- mean(beta_jne[cbind(between_pairs$Var1, between_pairs$Var2)])
  total_mean    <- mean(beta_jac[cbind(between_pairs$Var1, between_pairs$Var2)])
  
  cat(paste0(
    g[1], " vs ", g[2], " — Total: ", round(total_mean, 3),
    " | Turnover: ", round(turnover_mean, 3),
    " | Nestedness: ", round(nested_mean, 3), "\n"
  ))
}

# Percent
(0.529/0.775)*100 # turnover
(0.226/0.775)*100 # nestedness

# PERMANOVA --------------------------------------------------------------------

# Total beta diversity
adonis2(beta_parts$beta.jac ~ group, permutations = 999)
# not significant

# Turnover
adonis2(beta_parts$beta.jtu ~ group, permutations = 999)
# not significant

# Nestedness
adonis2(beta_parts$beta.jne ~ group, permutations = 999)
# not significant

# Testing homogeneity of dispersion --------------------------------------------

# (PERMANOVA assumes similar within-group dispersion.
# If this is violated, significance may reflect variance differences, not centroid shifts.)

# Total beta diversity
bd_jac <- betadisper(beta_parts$beta.jac, group)
anova(bd_jac) # redundant?
permutest(bd_jac)
# Pr(>F) 0.3014
# not significant, Control and impact lakes have similar dispersion in Jaccard space.

# Turnover
bd_jtu <- betadisper(beta_parts$beta.jtu, group)
anova(bd_jtu)
permutest(bd_jtu)
# Pr(>F) 0.8014

# Nestedness
bd_jne <- betadisper(beta_parts$beta.jne, group)
anova(bd_jne)
permutest(bd_jne)
# Pr(>F) 0.2014

# All were non-significant: Control and impact lakes have similar within-group variability in community composition, for all components of beta diversity.
