##############################################

# 3.5 Indicator species analysis

##############################################

# Create community matrix ------------------------------------------------------

# Create df with abundances per lake
locality_ab_EPT <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(locality,scientificName) %>%
  dplyr::summarise(individualCount_species = sum(individualCount),
            Status = paste0(unique(Status), sep = "")) %>%
  dplyr::ungroup()

# Create community matrix
com_matrix <- locality_ab_EPT %>%
  dplyr::mutate(across(.cols = c(locality,Status,scientificName), .fns = as.factor)) %>%
  pivot_wider(names_from = scientificName, # pivot wide
              values_from = individualCount_species,
              values_fill = 0) %>%
  tibble::column_to_rownames(var = "locality") # change our column "site" to our rownames

# get group as vector
group <- com_matrix$Status

# Remove metadata
com_matrix <- com_matrix %>%
  dplyr::select(-c("Status"))

# Run analysis -----------------------------------------------------------------

set.seed(2) # for reproducibility
indval_result_lakes <- indicspecies::multipatt(com_matrix,
                                               group,
                                               func = "IndVal.g",
                                               duleg = TRUE,
                                               control = how(nperm = 999))

summary(indval_result_lakes)

# Look closer at results
summary(indval_result_lakes, alpha = 1)
indval_result_lakes$sign

# Investigate df
indval_result_lakes_df <- as.data.frame(indval_result_lakes$sign)

indval_result_lakes_df <- indval_result_lakes_df %>%
  dplyr::arrange(desc(stat),p.value)
