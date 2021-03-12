library(ncdf4)

SetLU1850 <- function(nc, var){
  x <- ncvar_get(nc, var)
  x1850 <- x[,,1]
  for (i in 1:dim(x)[3]){
    x[,,i] <- x1850
  }
  ncvar_put(nc = nc, varid = var, vals = x)
}

nc.file <- '/home/akoch/terraces/datasets/dgvm_input/landuse/luhv2_states_fixed1850.nc'

nc <- nc_open(nc.file, write = TRUE)

SetLU1850(nc, var = 'lu_crop')
SetLU1850(nc, var = 'lu_past')
SetLU1850(nc, var = 'lu_urbn')

#ncatt_put(nc, varid = 0, attname = 'history', attval = 'Set all land use to 1850 using R.')
nc_close(nc)


##############################################################################
for (SSP in c('ssp126', 'ssp245', 'ssp370', 'ssp585')){
  nc1.file <- paste0('/home/akoch/terraces/datasets/dgvm_input/landuse/',SSP,'.nc')
  nc2.file <- paste0('/home/akoch/terraces/datasets/dgvm_input/landuse/tmp.nc')
  
  nc1 <- nc_open(nc1.file, write = TRUE)
  nc2 <- nc_open(nc2.file)

  for (varname in c('lu_crop', 'lu_past', 'lu_urbn')){
    
    x2 <- ncvar_get(nc2, varid = varname)
    ncvar_put(nc1, varid = varname, vals = x2)
  }
  
  nc_close(nc1)
  nc_close(nc2)
}
##FIXED LAND USE ##############################################################
nc_fix.file <- '/home/akoch/terraces/datasets/dgvm_input/landuse/landuse_fix2014.nc'
nc <- nc_open(nc_fix.file, write = TRUE)
past.var <- ncvar_get(nc, 'lu_past')
crop.var <- ncvar_get(nc, 'lu_crop')
past2014.var <- past.var[,,164]
crop2014.var <- crop.var[,,164]

for (i in 164:dim(past.var)[3]){
  past.var[,,i] <- past2014.var
  crop.var[,,i] <- crop2014.var
}
ncvar_put(nc = nc, varid = 'lu_past', vals = past.var)
ncvar_put(nc = nc, varid = 'lu_crop', vals = crop.var)
nc_close(nc)
## RESTORATION ###############################################################
past_2014.file <- '/home/akoch/terraces/datasets/dgvm_input/landuse/lu_past_restor_2014.nc'
restor_2014.file <- '/home/akoch/terraces/datasets/dgvm_input/landuse/landuse_restor_fix2014.nc'

past.nc <- nc_open(past_2014.file)
past_restor.var <- ncvar_get(past.nc, 'lu_past')

nc <- nc_open(restor_2014.file, write = TRUE)
past.var <- ncvar_get(nc, 'lu_past')

for (i in 180:dim(past.var)[3]){
  past.var[,,i] <- past_restor.var
}

annual_change <- (past.var[,,170] - past.var[,,180]) / 11
for (i in 171:180){
  past.var[,,i] <- past.var[,,i-1] - annual_change
}

ncvar_put(nc = nc, varid = 'lu_past', vals = past.var)
nc_close(nc)

## DO THE SAME FOR CLIMATE ####################################################
library(ncdf4)
nc.files <- list.files(path = '/home/akoch/terraces/datasets/dgvm_input/climate/', pattern = 'fixclim_2014', full.names = TRUE)
for (i in nc.files){
  nc <- nc_open(i, write = TRUE)
  
  SetCLIMATE2014 <- function(nc, var){
    x <- ncvar_get(nc, var)
    x2014 <- x[,,1956:1967]
    for (i in seq(1956, 3001, 12)){
      x[,,i:(i+11)] <- x2014
    }
    ncvar_put(nc = nc, varid = var, vals = x)
  }
  
  SetCLIMATE2014(nc, var = 'lght')
  SetCLIMATE2014(nc, var = 'tmp')
  SetCLIMATE2014(nc, var = 'dtr')
  SetCLIMATE2014(nc, var = 'pre')
  SetCLIMATE2014(nc, var = 'wet')
  SetCLIMATE2014(nc, var = 'cld')
  SetCLIMATE2014(nc, var = 'wnd')
  
  nc_close(nc)
  print(i)
}