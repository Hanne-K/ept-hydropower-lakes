#############################################

# 2.1 data occurrences

#############################################

# Load occurrence data for the orders Ephemeroptera, Plecoptera, and Trichoptera
# Sampling method: Standardised 1-minute kick sampling

# The data can also be accessed through GBIF, and is part of the NTNU University Museum dataset

# Citation for GBIF dataset: Hårsaker K, Daverdin M, Kjærstad G (2026). Freshwater benthic invertebrates ecological collection NTNU University Museum. Version 1.1603. Norwegian University of Science and Technology. Sampling event dataset https://doi.org/10.15468/k1pumk accessed via GBIF.org on 2026-09-17.

# Load occurrence dataset
load(file = here::here("data","source_data","occurrences","occurrences_df_v2.rda"))

# Load occurrence dataset which summarises the total number of EPT found per event
load(file = here::here("data","source_data","occurrences","occurrences_highertaxa_df_v2.rda"))
