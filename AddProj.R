library(sf)

wdir <- '/home/terraces/projects/LPJ_futuretropics/for_Katie'
sf.file <- list.files(wdir, '.shp', full.names = TRUE)
AddProj <- function(x){
  sf <- read_sf(x)
  st_crs(sf) <- 'WGS84'
  st_write(sf, x, append = FALSE)  
}
lapply(sf.file, AddProj)

FlipZeros <- function(x){
  sf <- read_sf(x)
  sf$DN <- sf$DN - 1
  st_write(sf, x, append = FALSE)  
}