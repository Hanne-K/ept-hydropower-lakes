#############################################

# 2.3 data trait body armour

#############################################

# Constructing a dataset containing information for each 
# EPT species in the study samples for the column "material". 
# For Tricoptera, an additional column with information on "construction" 
# is also added for additional analyses.
# Categorization categories are based on those used by similar studies and
# taxonomic literature

# construction: factor with 2 levels ("portable","nonportable")

# material: factor with 5 levels ("sclerotized","silk","mineral","mixed",
# "vegetation")

# Trichoptera ------------------------------------------------------------------

# Get list of species in dataset
species_T <- occurrences_df %>%
  dplyr::filter(order == "Trichoptera" & taxonRank == "SPECIES") %>%
  dplyr::select(scientificName) %>%
  dplyr::distinct()

# construction: portable, nonportable, none (no species of this type present)
# material: sclerotized (no T put in this category), mineral, vegetation, silk, 
# mixed

# Categorizations done based on morphological descriptions found in 
# taxonomic literature: Rinne A, Wiberg-Larsen P (2017) Trichoptera Larvae of Finland: A Key to the Caddis Larvae of Finland and Nearby Countries. Trificon 
# For all species literature traits were compared and confirmed in own samples.

# Ephemeroptera and Plecoptera -------------------------------------------------

# No armor information in freshwaterecology.info
# Using another trait database from the US: A Database of Lotic Invertebrate Traits for North America, https://pubs.usgs.gov/ds/ds187/

traits_database <- read.delim(here::here("data","source_data","armour","InvertTraitsTable_v1.txt"))

# Look at all armor categories
print(unique(traits_database$Armor))

# Investigate categorization
traits_sel <- traits_database %>%
  dplyr::select(Taxon,Family,Armor) %>%
  dplyr::filter(Armor == "Partly sclerotized")

traits_sel <- traits_database %>%
  dplyr::select(Taxon,Family,Armor) %>%
  dplyr::filter(Armor == "Soft")

# Based on categorization done here, none of our species would qualify as 
# "soft", they all are either partly sclerotized or build protable cases
# or silk-based fixed retreats.

# Therefore, if we follow this system the following categorization would be 
# used for "Armour"
# None: soft bodied (n = 0)
# Poor: partly sclerotized (Ephemeroptera, Plecoptera, 
# Trichoptera only for the completely free-living)
# Good: four subcategories: silk, vegetation, mixed, mineral.

# However, we want to distinguish between material types for Trichoptera 
# construction.

# The final category levels are therefore 
# material: factor with 5 levels ("sclerotized","silk","mineral","mixed",
# "vegetation")

# All Ephemeroptera and Plecoptera will be assigned level "sclerotized",
# which is in line with the database above and other literature.

# Armour df --------------------------------------------------------------------

# List of unique species
armour_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::select(order,scientificName) %>%
  dplyr::distinct()

# Add info for Trichoptera

# Load Trichoptera construction trait df
constructions_T <- readxl::read_excel(path = here::here("data","source_data","armour","constructions_T.xlsx"),sheet = "Data")

armour_df <- dplyr::left_join(x = armour_df, y = constructions_T, by = "scientificName")

# Add info for Ephemeroptera and Plecoptera 
armour_df <- armour_df %>%
  dplyr::mutate(material = if_else(order %in% c("Ephemeroptera","Plecoptera"),"sclerotized",material))

# Save
save(armour_df, file = here::here("data","derived_data","armour","armour_df.rda"))
