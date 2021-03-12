library(ncdf4)

wdir <- '/home/ucfaako/Documents/future_forests/data/'

source.file <- paste0(wdir, 'afforestation_mask_tropics_global.nc')
target.file <- paste0(wdir, 'restoration_mask.nc')

source.nc <- nc_open(source.file)
target.nc <- nc_open(target.file, write = TRUE)

var.nc <- ncvar_get(source.nc, 'Band1')
lon <- ncvar_get(source.nc, 'lon')
lat <- ncvar_get(source.nc, 'lat')

NearestIndx <- function(x, value){
  indx <- min(max(which(x <= value)), min(which(x == value)))  
  return(indx)
}

lat_bounds <- c(NearestIndx(lat, -36.5):NearestIndx(lat,37))
lon_bounds <- c(NearestIndx(lon, -125):NearestIndx(lon,179.95))

lat <- lat[lat_bounds]
lon <- lon[lon_bounds]
var.nc <- var.nc[lon_bounds, lat_bounds]

ncvar_put(target.nc, varid = 'mask', vals = var.nc)
ncvar_put(target.nc, varid = 'lon', vals = var.nc)
ncvar_put(target.nc, varid = 'mask', vals = var.nc)


nc_close(source.nc)
nc_close(target.nc)
