#!/home/akoch/.conda/envs/R_env/bin/R

library(ncdf4)

nc.file <- input_args[1] # land use file
nc <- nc_open(nc.file)
out.file <- input_args[2] # land use file for LPJ

compression <- 1
## create Netcdf ##################################################
lon <- as.array(seq(-179.75, 179.75, 0.5))
lat <- as.array(seq(-89.75, 89.75, 0.5))
time <- inv(-150:250)

londim <- ncdim_def(name = 'lon', units = 'degrees_east', vals = lon) # nneds to be changed outside R
latdim <- ncdim_def(name = 'lat', units = 'degrees_north', vals = lat)
timedim <- ncdim_def(name = 'time',units = 'years before 1950', vals = as.integer(time), unlim = TRUE)

# define variables
fillvalue <- -32768

var_def <- list()
for (VAR in c('lu_crop', 'lu_past', 'lu_urb')){
  var_def[[VAR]] <- ncvar_def(name = VAR, 
                       units = 'fraction', 
                       longname = 'Fraction of grid cell',
                       missval = fillvalue, 
                       chunksizes =  as.integer(c(1, 360, 720)), 
                       dim = list(londim, latdim, timedim), 
                       compression = compression,
                       prec = 'short')
}

## ag_burn
var_def[['ag_burn']] <- ncvar_def(name = 'ag_burn', 
                     units = 'fraction', 
                     longname = 'Fraction of grid cell',
                     missval = fillvalue, 
                     chunksizes =  as.integer(c(1, 360, 720)), 
                     compression = compression,
                     dim = list(londim, latdim, timedim), 
                     prec = 'short')

## hg_presence
fillvalue_b <- as.integer(-128)
var_def[['hg_presence']] <- ncvar_def(name = 'hg_presence', 
                                      units = 'fraction', 
                                      longname = 'Fraction of grid cell',
                                      missval = fillvalue_b, 
                                      chunksizes =  as.integer(c(1, 360, 720)), 
                                      dim = list(londim, latdim, timedim), 
                                      compression = compression,
                                      prec = 'byte')

## put variables
var <- list()
for (i in c('lu_crop', 'lu_past', 'lu_urbn')){
  var[[i]] <- ncvar_get(nc, varid = i)
  var[[i]] <- var[[i]] * 1e4
}


for (i in c('ag_burn', 'hg_presence')){
  var[[i]] <- ncvar_get(nc, varid = i)
  var[[i]] <- as.integer(var[[i]] * 0)
}

## create nc with variables on disk
ncout <- nc_create(filename = out.file, 
                   vars = var_def, force_v4=TRUE)

####
for (i in 1:5){
  ncvar_put(nc = ncout, varid = var_def[[i]], vals = var[[i]])
}

for (VAR in c('lu_crop', 'lu_past', 'lu_urb')){
  ncatt_put(ncout, varid = VAR, attname = 'scale_factor', attval = 1e-4, prec = 'float')
  ncatt_put(ncout, varid = VAR, attname = 'add_offset', attval = 0, prec = 'float')
  ncatt_put(ncout, varid = VAR, attname = 'missing_value', attval = fillvalue, prec = 'float')
  ncatt_put(ncout, varid = VAR, attname = 'valid_range', attval = as.integer(c(0, 10000)), prec = 'short')
  ncatt_put(ncout, varid = VAR, attname = '_Storage', attval = 'chunked')
  ncatt_put(ncout, varid = VAR, attname = '_ChunkSizes', attval =  as.integer(c(1, 360, 720)))
  ncatt_put(ncout, varid = VAR, attname = '_Endianness', attval = 'little')
}

####
VAR <- 'ag_burn'

ncatt_put(ncout, varid = VAR, attname = 'scale_factor', attval = 1, prec = 'float')
ncatt_put(ncout, varid = VAR, attname = 'add_offset', attval = 0, prec = 'float')
ncatt_put(ncout, varid = VAR, attname = 'missing_value', attval = fillvalue, prec = 'float')
ncatt_put(ncout, varid = VAR, attname = 'valid_range', attval = as.integer(c(0, 10000)), prec = 'short')
ncatt_put(ncout, varid = VAR, attname = '_Storage', attval = 'chunked')
ncatt_put(ncout, varid = VAR, attname = '_ChunkSizes', attval =  as.integer(c(1, 360, 720)))
ncatt_put(ncout, varid = VAR, attname = '_Endianness', attval = 'little')

##
VAR <- 'hg_presence'

ncatt_put(ncout, varid = VAR, attname = 'scale_factor', attval = 1, prec = 'float')
ncatt_put(ncout, varid = VAR, attname = 'add_offset', attval = 0, prec = 'float')
ncatt_put(ncout, varid = VAR, attname = 'missing_value', attval = fillvalue_b, prec = 'byte')
ncatt_put(ncout, varid = VAR, attname = 'valid_range', attval = as.integer(c(0, 1)), prec = 'short')
ncatt_put(ncout, varid = VAR, attname = '_Storage', attval = 'chunked')
ncatt_put(ncout, varid = VAR, attname = '_ChunkSizes', attval =  as.integer(c(1, 360, 720)))
ncatt_put(ncout, varid = VAR, attname = '_Endianness', attval = 'little')

nc_close(ncout)
nc_close(nc)