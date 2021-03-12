library(terra)
r <- rast('/home/ucfaako/Documents/carbon_offsets/data/biomes/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif')
r[r<18] <- NA
r[r>=19 & r<22] <- NA
r[r>22] <- NA
r[r>1] <- 1

writeRaster(r, '/home/ucfaako/Documents/future_forests/data/savanna.tif', 
             overwrite =TRUE,
             wopt = list(progress = TRUE, memfrac = 0.75, gdal = 'COMPRESS=LZW'))

library(ncdf4)
sav.nc <- nc_open('/home/ucfaako/Documents/future_forests/data/savanna_30sec.nc')
sav.nc <- ncvar_get(sav.nc, 'Band1')

mask.nc <- nc_open('/home/ucfaako/Documents/future_forests/data/afforestation_mask_tropics_global_sav.nc', write = TRUE)
var.nc <- ncvar_get(mask.nc, 'Band1')
var.nc[var.nc > 0] <- 1
var_sav.nc <- var.nc * sav.nc
var_sav.nc[var_sav.nc!=1] <- NA
ncvar_put(mask.nc, varid = 'Band1', vals = var_sav.nc)
nc_close(mask.nc)

mask.nc <- nc_open('/home/ucfaako/Documents/future_forests/data/afforestation_mask_tropics_global_nosav.nc', write = TRUE)
var.nc <- ncvar_get(mask.nc, 'Band1')
sav.nc <- nc_open('/home/ucfaako/Documents/future_forests/data/afforestation_mask_tropics_global_sav.nc')
sav.nc <- ncvar_get(sav.nc, 'Band1')
var.nc[var.nc > 0] <- 1
sav.nc[is.na(sav.nc)] <- 0
var_sav.nc <- var.nc - sav.nc
var_sav.nc[var_sav.nc!=1] <- NA
ncvar_put(mask.nc, varid = 'Band1', vals = var_sav.nc)
nc_close(mask.nc)
