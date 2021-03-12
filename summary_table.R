#!/home/akoch/.conda/envs/R_env/bin/R
library(raster); library(sf); library(exactextractr); library(dplyr)
input_args <- commandArgs(trailingOnly = TRUE)

rs.file <- input_args[1]
shp.file <- input_args[2] # '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
out.file <- input_args[3]

rs <- raster::stack(rs.file)
shp <- read_sf(shp.file)

shp <- shp %>% 
  group_by(SOVEREIGNT) %>%
  summarise()

agb_mean <- exact_extract(rs, shp, 'mean') # gC m^-2
agb_mean <- as.data.frame(agb_mean) * 1e-4 # MgC ha^-1

a <- area(rs[[1]])
rs_area <- rs * a * 1e6
agb_total <- exact_extract(rs_area, shp, 'sum') 
agb_total <- as.data.frame(agb_total) * 1e-6 # MgC
area_sum <- exact_extract(a * 1e-4, shp, 'sum') # Mha

SummaryStats <- function(df, type){
  r_mean <- rowMeans(df)
  r_sd <- apply(df, 1, FUN = sd)
  r_ci <- 1.96 * (r_sd / sqrt(ncol(df)))
  out.df <- data.frame(Country=shp$SOVEREIGNT, mean=r_mean, sd=r_sd, ci95=r_ci)
  names(out.df) <- c('Country', paste(type, c('mean', 'sd', 'ci95'), sep = '_'))
  out.df <- out.df[complete.cases(out.df),]
  return(out.df)
}

agb_mean.df <- SummaryStats(agb_mean, type = 'AGBmean')
agb_total.df <- SummaryStats(agb_total, type = 'AGBtotal')
agb.df <- merge.data.frame(agb_mean.df, agb_total.df, by = 'Country')

area.df <- data.frame(Country=shp$SOVEREIGNT, area_ha=area_sum)

agb.df <- merge.data.frame(agb.df, area.df, by = 'Country')

write.csv(agb.df, out.file, row.names = FALSE)