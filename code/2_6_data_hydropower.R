#############################################

# 2.6 data hydropower

#############################################

# Import data from The Norwegian Water and Energy Directorate (NVE)
# on hydropower reservoirs in Norway, from https://nedlasting.nve.no/gis/
# Data download specifications: UTM zone 33 (reccommended for the whole of Norway),
# geojson file, overlapping entire country, reservoirs, WGS84 (UTM) with lat long.
# Download date: 03.06.2023

## Reservoir data from NVE (reservoir = magasin) -------------------------------

# Load file
magasin_sf <- geojsonsf::geojson_sf(here::here("data","source_data","hydropower_reservoirs_NVE","NVEData","Vannkraft_Magasin.geojson"))