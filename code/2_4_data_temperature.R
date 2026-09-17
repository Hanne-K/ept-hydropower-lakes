#############################################

# 2.4 data temperature

#############################################

# Load raw temperature data
# File contains all logged temperature datapoints captured by all deployed HOBO temperature loggers.
# In general, two loggers were placed in each lake: one on fastened to the shore (str), and the other
# fastened to a rope attached to a buoy-anchor system (bl) allowing the logger to hang 1.5 meters below the
# water surface.
load(here::here("data","source_data","temperature","temperature_data.rda"))


# Filter time period and add mean daily temp.(temperature_data_v2) -------------

## Keep only valid logging period ----

# Import excel sheet with logging period info
logger_info <- readxl::read_excel(path = here("data","source_data","temperature","HOBO_loggers_overview.xlsx"),
                                  sheet = "logger_info") # relevant columns: date_time_start and date_time_stop

# Filter the start and stop datetimes: for each logger, keep only datapoints within the date_time_start and date_time_stop interval
# Add start and stop 
temperature_data_v2 <- left_join(x = temperature_data, 
                                 y = dplyr::select(logger_info, 
                                                   c("locality","date_time_start","date_time_stop","logger_id")),
                                 by = "logger_id")

# Filter datapoints, go from 16227 to 12918 observations
temperature_data_v2 <- temperature_data_v2 %>%
  subset(date_time >= date_time_start & date_time <= date_time_stop)

## Add daily mean temp. ----

# Add date as separate column
temperature_data_v2 <- temperature_data_v2 %>%
  mutate(eventDate = as.Date(date_time), .after = date_time,
         year = as.numeric(format(date_time, "%Y")),
         month = as.numeric(format(date_time, "%m")),
         day = as.numeric(format(date_time, "%d")),
         month_day = paste(strftime(date_time, format = "%m-%d"),sep="-")) 

# Add average daily temperature
temperature_data_v2 <- temperature_data_v2 %>%
  group_by(eventDate,logger_id) %>%
  mutate(daily_avg_temp = round(mean(temperature_C, ),digits = 3),
         daily_avg_temp_sd = round(sd(temperature_C),digits = 3))

# Save
save(temperature_data_v2, file = here::here("data","derived_data","temperature","temperature_data_v2.rda"))

# Remove unwanted loggers (temperature_data_v3) --------------------------------

# Keep the shore zone loggers for all lakes except Stor-Drakstsjoen 2024, where we use the open water logger.
# We checked that shore zone and open water loggers show close to the same temperature fluctuations.
# We keep only one logger per lake.

selected_logger_ids <- c("Barsetvatn_2023_str_21140198","Gjoljavatn_2023_str_21140205","Kilvatn_2023_str_21140206",
                         "Roksetvatn_2023_str_21508212","Stordrakstsj_2023_str_21102653","Storvatn_2023_str_21140212",
                         "Barsetvatnet_2024_str_21774702","Gjoljavatnet_2024_str_21798976","Kilvatn_2024_str_21774708",
                         "Roksetvatn_2024_str_21798975","Stordrakstsj_2024_bl_21774707","Storvatnet_2024_str_21774704")

# Final temperature dataset: one logger per lake and year, only valid time points, 7041 observations
temperature_data_v3 <- temperature_data_v2 %>%
  filter(logger_id %in% selected_logger_ids)

# Save
save(temperature_data_v3, file = here::here("data","derived_data","temperature","temperature_data_v3.rda"))
