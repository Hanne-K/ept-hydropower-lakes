################################################

# 2.2 data trait locomotion and surface relation

################################################

# Load raw trait data: locomotion and surface relation
# including all available data for EPT species from freshwaterecology.info (download date 08.10.2025)
locomotion_database_EPT <- read.csv(file = here::here("data","source_data","locomotion","locomotion_EPT_freshwaterecologyinfo.csv"),
                                    sep = ",",quote = "", header = TRUE, fileEncoding = "UTF-8")

# Create own locomotion trait dataset ------------------------------------------

# Include species in our dataset and assign one locomotion category 
# per species, where the assigned category is the one with highest score.
locomotion_EPT <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::select(order,genus,scientificName) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(order,genus,scientificName) # order alphabetically

# Add column for locomotion
locomotion_EPT$locomotion <- c("wsw","crw","crw","wsw","wsw", # 1-5 A.inopinatus - C. dipterum
                               "bur","crw","crw","crw","crw", # 6-10 E. vulgata - L. marginata
                               "crw","wsw","wsw","wsw","wsw", # 11-15 L. vespertina - S. lacustris
                               "crw","crw","crw","crw","crw", # 16-20 D. nanseni - A. obsoleta
                               "crw","crw","crw","crw","tat", # 21-25 A. obscurata - C. flavidus
                               "tat","crw","crw","crw","tat", # 26-30 C. trimaculatus - H. dubius
                               "tat","crw","crw","crw","crw", # 31-35 H. picicornis - L. stigma
                               "crw","crw","crw","crw","crw", # 36-40 L. vittatus - O. ochracea
                               "tat","tat","crw","crw","tat","crw") # 41-46 P. flavomaculatus

# Save file
save(locomotion_EPT, file = here::here("data","derived_data","locomotion","locomotion_EPT.rda"))

# Notes:

# Limnephilidae: all data on this family classifies them as crawlers (crw), so will assign our four Limnephilus species and A. obscurata as crw

# Metretopus borealis: No records were found in the database. Few species in the family, distinct.
# Based on morphology and known diet, either crawler or full water swimmer is likely.
# Siphlonuridae, which M. borealis earlier was thought to belong to, is listed as wsw. 
# Will also assume wsw is the main locomotion and surface relation category for M. borealis.
