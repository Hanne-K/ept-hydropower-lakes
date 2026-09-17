##############################################

# 5.8 Table S3 Species list

##############################################

# Table with abundances for each species and lake 

# Create dataframe
EPT_ab_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(scientificName,locality) %>%
  dplyr::summarise(sum_individualCount = sum(individualCount)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(.cols = c(scientificName,locality), .fns = as.factor)) %>%
  tidyr::pivot_wider(names_from = locality,
                     values_from = sum_individualCount,
                     values_fill = 0) 

# Save as excel
writexl::write_xlsx(EPT_ab_df, path = here::here("results","tables","table_S3_species_list.xlsx"))