##############################################

# 4.5 Table 2 Summary statistics EPT

##############################################

# Summary statistics for EPT species abundance and richness per lake -----------

# Summarise the per-sample species abundance and richness
event_ab_sp_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   eventDate = as.Date(paste0(unique(eventDate), collapse = ", ")),
                   sum_individualCount = as.numeric(sum(individualCount[taxonRank == "SPECIES"], na.rm = TRUE)),
                   N_species = length(unique(taxonKey[taxonRank == "SPECIES"], na.rm = TRUE))
  )

# Calculate lake level summary statistics
EPT_locality_stats_df <- event_ab_sp_df %>%
  dplyr::group_by(Status,locality) %>%
  dplyr::summarise(mean_ab = mean(sum_individualCount),
                   SD_ab = sd(sum_individualCount),
                   min_ab = min(sum_individualCount),
                   max_ab = max(sum_individualCount),
                   mean_sp = mean(N_species),
                   SD_sp = sd(N_species),
                   min_sp = min(N_species),
                   max_sp = max(N_species)
  )

# Save as excel
writexl::write_xlsx(EPT_locality_stats_df, 
                    path = here::here("results","tables","table_2_summary_statistics.xlsx"))

