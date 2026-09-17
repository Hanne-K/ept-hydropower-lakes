#############################################

# 2.7 data tidy and merge

#############################################

# Load correct files from data scripts if not already loaded -------------------

# Load occurrence dataset
load(file = here::here("data","source_data","occurrences","occurrences_df_v2.rda"))
# Load occurrence dataset which summarises the total number of EPT found per event
load(file = here::here("data","source_data","occurrences","occurrences_highertaxa_df_v2.rda"))
# Load event dataset
load(file = here::here("data","source_data","occurrences","events_df.rda"))

# Load locomotion file
load(file = here::here("data","derived_data","locomotion","locomotion_EPT.rda"))
# Load armour file
load(file = here::here("data","derived_data","armour","armour_df.rda"))

# Load temperature dataset
load(file = here::here("data","derived_data","temperature","temperature_data_v3.rda"))

# Add temperature data ---------------------------------------------------------

# Make sure date column is on the same format
temperature_data_v3$date <- as.Date(temperature_data_v3$eventDate)
occurrences_df$eventDate <- as.Date(occurrences_df$eventDate)
occurrences_highertaxa_df$eventDate <- as.Date(occurrences_highertaxa_df$eventDate)

# Add internal_parentEventID to each relevant temperature datapoint
# Make dataframe with parent events and daily avg temperature
events_df$eventDate <- as.Date(events_df$eventDate)
# Only parent events
parentEvents_df <- events_df %>%
  dplyr::select(eventDate,locality,internal_parentEventID) %>%
  distinct() # 54 unique

# Only daily average temp 
daily_avg_temp <- temperature_data_v3 %>%
  dplyr::select(eventDate,locality,daily_avg_temp) %>%
  distinct()

parentEvents_df <- inner_join(x = parentEvents_df, y = daily_avg_temp[,c("eventDate","locality","daily_avg_temp")],
                              by = c("eventDate","locality")) # 48 events we have temperature data for

# Add the daily average temperature to each sampling event
occurrences_df <- dplyr::left_join(x = occurrences_df, y = parentEvents_df[,c("internal_parentEventID","daily_avg_temp")], 
                                   by = "internal_parentEventID") # 644

occurrences_highertaxa_df <- dplyr::left_join(x = occurrences_highertaxa_df, y = parentEvents_df[,c("internal_parentEventID","daily_avg_temp")], by = "internal_parentEventID") # 412


# Add locomotion data ----------------------------------------------------------

# Add data to all species occurrences
occurrences_df <- dplyr::left_join(x = occurrences_df, 
                                   y = locomotion_EPT[,c("scientificName","locomotion")], 
                                   join_by(scientificName))

# Add armour data --------------------------------------------------------

# Add data on body armour (construction type and material) to all species
occurrences_df <- dplyr::left_join(x = occurrences_df, 
                                   y = armour_df[, c("scientificName","construction","material")], 
                                   by = "scientificName")

# Order levels of material
occurrences_df$material <- factor(occurrences_df$material, levels = c("sclerotized","silk","mineral","mixed","vegetation"))

# Remove replicates and save final dataset version -----------------------------

# Two replicates were removed to ensure even sampling effort, see main
# for further details.
occurrences_df <- occurrences_df %>%
  dplyr::filter(!internal_eventID %in% c("Stor-Drakstsjoen_24_05_2023_sample_4",
                                         "Stor-Drakstsjoen_24_05_2023_sample_5"))

occurrences_highertaxa_df <- occurrences_highertaxa_df %>%
  dplyr::filter(!internal_eventID %in% c("Stor-Drakstsjoen_24_05_2023_sample_4",
                                         "Stor-Drakstsjoen_24_05_2023_sample_5"))

# Save -------------------------------------------------------------------------

# Save final datasets for analysis
save(occurrences_df, file = here::here("data","derived_data","occurrences","occurrences_df_v3.rda"))
save(occurrences_highertaxa_df, file = here::here("data","derived_data","occurrences","occurrences_highertaxa_df_v3.rda"))
