library(ncdf4)
# load file
wdir <- '/home/ucfaako/Documents/future_forests/data/'
ghg.file <- list.files(path = wdir, pattern = 'ghg_ssp')
ghg.file2 <- list.files(path = wdir, 
                        pattern = 'mole-fraction-of-carbon-dioxide-in-air')
for (ssp in c('ssp126', 'ssp245', 'ssp370', 'ssp585')){
  nc <- nc_open(paste0(wdir, ghg.file[grep(ssp, ghg.file)]), write = TRUE)
  nc2 <- nc_open(paste0(wdir, ghg.file2[grep(ssp, ghg.file2)]))
  nc_var <- ncvar_get(nc, 'co2')
  nc_var2 <- ncvar_get(nc2, 'mole_fraction_of_carbon_dioxide_in_air')
  time_var <- 1950 - ncvar_get(nc, 'time') 
  
  nc_var[which(time_var==2015):which(time_var==2500)] <- nc_var2[1, ]
  
  ncvar_put(nc, varid = 'co2', vals = nc_var)
  nc_close(nc)
  nc_close(nc2)
}

## fixed co2

wdir <- '/home/ucfaako/Documents/future_forests/data/'
ghg.file <- paste0(wdir, 'ghg_ssp126.nc')

ghg.nc <- nc_open(ghg.file, write = TRUE)

tx <- ncvar_get(ghg.nc, 'time')
ghg.a <- ncvar_get(ghg.nc, 'co2')

ghg.a[tx < 1950 - 2014] <- ghg.a[tx==1950 - 2014]

ncvar_put(ghg.nc, 'co2', ghg.a)
nc_close(ghg.nc)
