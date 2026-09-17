#############################################

# 2.5 data water levels

#############################################

# Load water level data for unregulated lakes, and for the regulated ones 
# where data was available

# Water level data unregulated lakes -------------------------------------------

# Load raw data
load(file = here::here("data","source_data","waterlevels","GPS_waterlevels.rda"))


# Water level data regulated lakes ---------------------------------------------

# Load raw data 
# Data from the web portal Sildre, by The Norwegian Water and Energy Directorate
# Downloaded 04.02.2025 from https://sildre.nve.no/
gjolja_wl <- read.csv(file = here::here("data","source_data","waterlevels","waterlevels_Gjolja.csv"),
                      sep = ";",
                      header = FALSE)

stordr_wl <- read.csv(file = here::here("data","source_data","waterlevels","waterlevels_Stor_Drakst.csv"),
                      sep = ";",
                      header = FALSE)

## Tidy structure ----

# Lake Stor-Drakstsjøen
# Fix headers
stordr_wl <- stordr_wl[-(1:2), ] # remove rows

# Set new column names
new_names <- c("Date","Waterlevel_m","Corrected","Controlled")

stordr_wl <- stordr_wl %>%
  setNames(new_names)

# Set formats
stordr_wl$Date <- as.Date(stordr_wl$Date) # date
stordr_wl <- stordr_wl %>%
  mutate(Waterlevel_m = as.numeric(gsub(",", ".", Waterlevel_m))) # numeric
stordr_wl$Waterlevel_m <- as.numeric(stordr_wl$Waterlevel_m)

# Add year
stordr_wl$year <- format(stordr_wl$Date, "%Y")

# Lake Gjøljavatnet

# Fix headers
gjolja_wl <- gjolja_wl[-(1:2), ] # remove rows

# Set new column names
new_names <- c("Date","Waterlevel_m","Corrected","Controlled")

gjolja_wl <- gjolja_wl %>%
  setNames(new_names)

# Set formats
gjolja_wl$Date <- as.Date(gjolja_wl$Date) # date
gjolja_wl <- gjolja_wl %>%
  mutate(Waterlevel_m = as.numeric(gsub(",", ".", Waterlevel_m))) # numeric
gjolja_wl$Waterlevel_m <- as.numeric(gjolja_wl$Waterlevel_m)

# Add year
gjolja_wl$year <- format(gjolja_wl$Date, "%Y")

# Save tidy versions as .rda
save(stordr_wl, file = here::here("data","derived_data","waterlevels","stordr_wl.rda"))
save(gjolja_wl, file = here::here("data","derived_data","waterlevels","gjolja_wl.rda"))
