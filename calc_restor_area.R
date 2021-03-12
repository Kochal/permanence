library(raster); library(exactextractr); library(sf); library(dplyr)

wdir <- '/home/terraces/projects/LPJ_futuretropics/'
shp.file <- '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
area.file <- paste0(wdir, 't3cover_area_mapdiff.nc')

shp <- read_sf(shp.file)

shp_agg <- shp %>% 
  group_by(SOVEREIGNT) %>%
  summarise()

subcontinent <- c('Bangladesh', 'Bhutan', 'India', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka')
shp$CONTINENT[shp$SOVEREIGNT %in% subcontinent] <- 'Indian subcontinent'

shp_continent <- shp %>% 
  group_by(CONTINENT) %>%
  summarise()

r <- raster::stack(area.file)
cntr <- exactextractr::exact_extract(r, shp_agg, 'sum')
continent <- exactextractr::exact_extract(r, shp_continent, 'sum')

cntr.df <- as.data.frame(cntr)
names(cntr.df) <- 'area_m2'
cntr.df$Region <- shp_agg$SOVEREIGNT

continent.df <- as.data.frame(continent)
names(continent.df) <- 'area_m2'
continent.df$Region <- shp_continent$CONTINENT

out.df <- rbind(cntr.df, continent.df)
write.csv(out.df, paste0(wdir, 'restoration_area.csv'), row.names = FALSE)

## calculate "permanent" restoration area ##################################
wdir <- '/home/terraces/projects/LPJ_futuretropics/permanence/'
shp.file <- '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
area.files <- list.files(wdir, 'gridarea', full.names = TRUE)

shp <- read_sf(shp.file)

Area2Df <- function(x){
  print(x)
  nm <- gsub(wdir, '', x)
  nm <- unlist(strsplit(x, '_'))
  ssp <- nm[2]
  scenario <- paste(nm[3:4], collapse = '_')
  r <- raster::stack(x)
  df <- exact_extract(r, shp, 'sum')
  df <- as.data.frame(df)
  names(df) <- c('Mha')
  df$ssp <- ssp
  df$scenario <- scenario
  df$country <- shp$ADMIN
  df$continent <- shp$CONTINENT
  return(df)
}

area.df <- lapply(area.files, FUN = Area2Df)
area.df <- do.call('rbind', area.df)
write.csv(area.df, paste0(wdir, 'permanent_restoration_area.csv'), row.names = FALSE)
