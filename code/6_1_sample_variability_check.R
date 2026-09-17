#############################################

# 6.1 sample variability check

#############################################

# Assessing how variable replicates within an event are.
# Comparing this variation to that between events within a lake.
# The comparisons were done early in the study, and the method for doing so
# is documented here.

# Fist event in 2023: 3 replicates processed per lake.
print(unique(occurrences_df$internal_parentEventID)) 
print(unique(occurrences_df$internal_eventID))
occurrences_df$periodNumber

# Prepare data -----------------------------------------------------------------

event1 <- occurrences_df %>% 
  dplyr::filter(year == 2023 & periodNumber == 1)

# Summary df
data_1 <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   internal_parentEventID = as.factor(paste0(unique(internal_parentEventID), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "),
                   N_species = length(unique(taxonKey[taxonRank == "SPECIES"], na.rm = TRUE)),
                   N_ind = as.numeric(sum(individualCount[taxonRank == "SPECIES"], na.rm = TRUE))
  )

# Three levels: locality/parent event/event (replicate)
# These should all be factors
data_1$internal_eventID <- as.factor(data_1$internal_eventID)

# Comparing variability using coefficients of variation ------------------------

# Within-event variation
within_cv <- data_1 %>%
  filter(year == 2023 & periodNumber == 1) %>%
  group_by(locality) %>%
  summarise(mean_rich = mean(N_species),
            sd_rich = sd(N_species),
            cv_rich = sd_rich/mean_rich,
            mean_abund = mean(N_ind),
            sd_abund = sd(N_ind),
            cv_abund = sd_abund/mean_abund)

# Between-event variation
between_cv <- data_1 %>%
  group_by(locality, internal_parentEventID) %>%
  summarise(
    mean_rich = mean(N_species),
    mean_abund = mean(N_ind),
    .groups = "drop"
  ) %>%
  group_by(locality) %>%
  summarise(
    sd_rich = sd(mean_rich),
    mean_rich = mean(mean_rich),
    cv_rich = sd_rich / mean_rich,
    
    sd_abund = sd(mean_abund),
    mean_abund = mean(mean_abund),
    cv_abund = sd_abund / mean_abund
  )

# Comparison
comparison_df <- within_cv %>%
  dplyr::select(locality, cv_rich_within = cv_rich, cv_abund_within = cv_abund) %>%
  dplyr::left_join(
    between_cv %>%
      dplyr::select(locality, cv_rich_between = cv_rich, cv_abund_between = cv_abund),
    by = "locality"
  )

variability_comparison_df <- comparison_df %>%
  dplyr::mutate(
    ratio_rich = cv_rich_within / cv_rich_between,
    ratio_abund = cv_abund_within / cv_abund_between
  )

# The ratios are consistently below 1, supporting that the within-event 
# variation is smaller than the between-event variation.
# We have 1 exception (Gjøljavantet, richness), but I do not consider this
# enough to discredit the overall conclusion.
# Based on this comparison, we choose to go through samples from all events
# rather than going through all replicates from a reduced number of events.

# Save 
save(variability_comparison_df, file = here::here("results","tables","variability_comparison.rda"))
