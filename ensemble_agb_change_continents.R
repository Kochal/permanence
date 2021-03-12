#!/home/akoch/.conda/envs/R_env/bin/R
library(raster); library(sf); library(exactextractr); library(dplyr)
input_args <- commandArgs(trailingOnly = TRUE)

rs.file <- input_args[1] # agb
shp.file <- input_args[2] # '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
out.file <- input_args[3]

rs <- raster::stack(rs.file)
shp <- read_sf(shp.file)

subcontinent <- c('Bangladesh', 'Bhutan', 'India', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka')
shp$CONTINENT[shp$SOVEREIGNT %in% subcontinent] <- 'Indian subcontinent'

shp_agg <- shp %>% 
  group_by(CONTINENT) %>%
  summarise()

agb_total <- exact_extract(rs, shp_agg, 'sum') 

area.df <- as.data.frame(agb_total)
area.df$CONTINENT <- shp_agg$CONTINENT

area.df$geometry <- NULL
write.csv(area.df, out.file, row.names = FALSE)
