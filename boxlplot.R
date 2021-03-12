library(ncdf4); library(ggplot2); library(reshape); library(ggpubr)
source('/home/akoch/scripts/R_code/fun_Utility.R')

wdir <- '/home/terraces/projects/LPJ_futuretropics/restorgains/'

nc2090s <- list()
nc2040s <- list()
for (SSP in c('ssp126', 'ssp245', 'ssp370', 'ssp585')){
  f2090s <- paste0(wdir, 'restorgains_ens_', SSP, '_2090s.nc')
  f2040s <- paste0(wdir, 'restorgains_ens_', SSP, '_2040s.nc')
  
  nc2090s[[SSP]] <- nc_read(f2090s)
  nc2040s[[SSP]] <- nc_read(f2040s)
}

df <- data.frame(decade=rep(c('2040', '2090'), each=13*4), 
           scenario=rep(rep(c('ssp126', 'ssp245', 'ssp370', 'ssp585'), each=13), times=2),
           d_agb = c(do.call('rbind', nc2040s), do.call('rbind', nc2090s)))

p <- ggplot(df, aes(x=scenario, y=d_agb, fill=decade)) + geom_boxplot() + theme_bw(size = 13)