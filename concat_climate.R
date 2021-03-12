# for SSP in ssp245 ssp370 ssp585; do for MOD in BCC-CSM2-MR IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 FGOALS-g3 MIROC6 ACCESS-CM2 ACCESS-ESM1-5 AWI-CM-1-1-MR EC-Earth3-Veg INM-CM4-8 INM-CM5-0 CanESM5; do ncks --mk_rec_dmn time -O /home/terraces/datasets/dgvm_input/climate/${MOD}_${SSP}_1850-2100_30min_global_ctl.nc /home/terraces/datasets/dgvm_input/climate/${MOD}_${SSP}_1850-2100_30min_global_ctl.nc; done; done

library(ncdf4); library(abind)

wdir <- '~/terraces/projects/LPJ_futuretropics/'

spinup.files <- list.files(wdir, 'spinup', full.names = TRUE)

for (i in 1:length(spinup.files)){
  for (SSP in c('ssp126')){ # , 'ssp245', 'ssp370', 'ssp585'
    models.files <- list.files(wdir, 
                               paste0('_spinup_', SSP, '.nc'), 
                               full.names = TRUE)
    nc1.file <- paste0(spinup.files[[i]])
    nc2.file <- paste0(models.files[[i]])
    
    nc1 <- nc_open(nc1.file)
    nc2 <- nc_open(nc2.file, write = TRUE)
    
    nc1_date_incr <- rev(seq(as.Date('1850-1-1'), by='-1 month',length=length(ncvar_get(nc1, 'time'))))
    nc1_date_incr_1850 <- as.numeric(nc1_date_incr)--43464
    nc2_date <- ncvar_get(nc2, 'time')
    
    tx <- c(nc1_date_incr_1850, nc2_date)
    
    for (VAR in c('cld', 'dtr', 'lght', 'pre', 'tmp', 'wet')){
      var1 <- ncvar_get(nc1, VAR)
      var2 <- ncvar_get(nc2, VAR)

      ncvar_put(nc2, varid = VAR, vals = var1, start = c(1, 1, 1), 
                count = dim(var1))
      ncvar_put(nc2, varid = VAR, vals = var2, start = c(1, 1, 3000), 
                count = c(720, 360, 6011))
      print(VAR)
    }
    
    ncvar_put(nc2, varid = 'time', vals = tx, start = c(1), count = length(tx))
    
    nc_close(nc1)
    nc_close(nc2)
  }
}

