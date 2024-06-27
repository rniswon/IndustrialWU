library(sf)
library(dplyr)
library(ggplot2)
library(plotly)

# Get User Inputs 
counties_shapefile_path <- "data/cb_2018_us_county_500k.shp"
cbp_data_csv_path <- "data/CBP2021.CB2100CBP-Data.csv"

# Load shapefile
counties_sf <- st_read(counties_shapefile_path)

# Filter counties outside of CONUS
oconus_fips <- c('02', '15', '72', '66', '78', '60', '69')

counties_SF <- counties_sf %>%
  filter(!STATEFP %in% oconus_fips)

# Load data
cbp_data <- read.table(cbp_data_csv_path, header = TRUE, sep = ",")

# Filter by naics code to only include manufacturing 
cbp_data <- cbp_data %>%
  mutate(ESTAB = as.numeric(ESTAB)) %>%
  filter(between(NAICS2017, 310000, 330000)) %>%
  group_by(GEO_ID) %>%
  summarise(Establishment_Count = sum(ESTAB, na.rm = TRUE))

# Formatting shapefile geoid 
counties_sf$GEOID <- paste0('0500000US', counties_sf$GEOID)

cbp_map <- merge(counties_sf, cbp_data, by.x = 'AFFGEOID', by.y = 'GEO_ID')

manufacturing_map <- ggplot(data = cbp_map) + 
  geom_sf(aes(fill = Establishment_Count, text = paste("County:", NAME, "<br>",
                                                       "Establishments:", Establishment_Count))) + 
  scale_fill_viridis_c(option = 'plasma', trans = 'log') + 
  theme_minimal() + 
  labs(fill = "Number of Establishments",
       title = "Number of Manufacturing Establishments by County")

plotly_map <- ggplotly(manufacturing_map)

plotly_map
