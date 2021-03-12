#!/home/akoch/.conda/envs/R_env/bin/R
library(raster); library(sf); library(exactextractr); library(dplyr)
input_args <- commandArgs(trailingOnly = TRUE)

rs.file <- input_args[1] # burnt area
shp.file <- input_args[2] # '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
out.file <- input_args[3]

rs <- raster::stack(rs.file)
shp <- read_sf(shp.file)

shp_agg <- shp %>% 
  group_by(SOVEREIGNT) %>%
  summarise()

burnt_total <- exact_extract(rs, shp_agg, 'sum') 
burnt_total <- burnt_total * 1e-10 # m^-2 to Mha
area.df <- as.data.frame(burnt_total)
area.df$Country <- shp_agg$SOVEREIGNT
area.df <- merge.data.frame(area.df, shp[, c('SOVEREIGNT', 'CONTINENT')], 
                            by.x = 'Country', by.y = 'SOVEREIGNT')
area.df$geometry <- NULL
write.csv(area.df, out.file, row.names = FALSE)

