## Adjust CMIP data to reforestation using Duveiller et al (2020) dataset 
# http://dx.doi.org/10.17632/8z3hc8jm7h.1
# assuming that impact of evergreen forests to grassland conversion is 
# 1) reversable (Duveillier et al 2018, Nat. Comm.) 
# 2) evenly distributed over the year
# 3) linearly increases with land cover change 
###############################################################################
library(ncdf4); library(terra); library(RColorBrewer)
source('/home/akoch/scripts/R_code/fun_Utility.R')
source('/home/akoch/scripts/R_code/fun_ClimHelpers.R')
# source('/home/ucfaako/Documents/R_code/fun_Utility.R')
# source('~/Documents/R_code/fun_ClimHelpers.R')
## Input ######################################################################
wdir <- '/home/terraces/datasets/CMIP6/lumip/'
wdir2 <- '/home/terraces/datasets/CMIP6/scenarioMIP/'
wdir3 <- '/home/akoch/data/future_forests/biophysical_scaling/'
# wdir <- '~/Documents/future_forests/data/ssp_lumip/'

models <- c('BCC-CSM2-MR', 'IPSL-CM6A-LR', 'MPI-ESM1-2-LR', 'NorESM2-LM')


for (mod in models){
  # climate files
  ssp370_126.file <- list.files(path = wdir, 
                                pattern = glob2rx(paste0('*', mod, '_ssp370-ssp126Lu_r1i1p1f1*201501-210012.nc')), 
                                recursive = TRUE, full.names = TRUE)
  ssp370.file <- list.files(path = wdir2, 
                            pattern = glob2rx(paste0('*', mod, '_ssp370_r1i1p1f1*201501-210012.nc')), 
                            recursive = TRUE, full.names = TRUE)
  
  tas_ssp370_126 <- nc_read(ssp370_126.file[grep('tas_', ssp370_126.file)], varname = 'tas')
  tas_ssp370 <- nc_read(ssp370.file[grep('tas_', ssp370.file)], varname = 'tas')
  
  ############################################################################
  if (any(grepl('pr_', ssp370_126.file))){
    pr_ssp370_126 <- nc_read(ssp370_126.file[grep('pr_', ssp370_126.file)], varname = 'pr')
    pr_ssp370 <- nc_read(ssp370.file[grep('pr_', ssp370.file)], varname = 'pr')
  } else {
    pr_ssp370_126 <- tas_ssp370_126
    pr_ssp370_126[] <- 0
    pr_ssp370 <- pr_ssp370_126
  }
 
  ############################################################################
  if (any(grepl('tasmin_', ssp370_126.file))){
    tasmin_ssp370_126 <- nc_read(ssp370_126.file[grep('tasmin_', ssp370_126.file)], varname = 'tasmin')
    tasmin_ssp370 <- nc_read(ssp370.file[grep('tasmin_', ssp370.file)], varname = 'tasmin')
  } else {
    tasmin_ssp370_126 <- tas_ssp370_126
    tasmin_ssp370_126[] <- 0
    tasmin_ssp370 <- tasmin_ssp370_126
  }
  
  ############################################################################
  if (any(grepl('tasmax_', ssp370_126.file))){
    tasmax_ssp370_126 <- nc_read(ssp370_126.file[grep('tasmax_', ssp370_126.file)], varname = 'tasmax')
    tasmax_ssp370 <- nc_read(ssp370.file[grep('tasmax_', ssp370.file)], varname = 'tasmax')
  } else {
    tasmax_ssp370_126 <- tas_ssp370_126
    tasmax_ssp370_126[] <- 0
    tasmax_ssp370 <- tasmax_ssp370_126
  }
  
  ############################################################################
  if (any(grepl('clt_', ssp370_126.file))){
    clt_ssp370_126 <- nc_read(ssp370_126.file[grep('clt_', ssp370_126.file)], varname = 'clt')
    clt_ssp370 <- nc_read(ssp370.file[grep('clt_', ssp370.file)], varname = 'clt')
  } else {
    clt_ssp370_126 <- tas_ssp370_126
    clt_ssp370_126[] <- 0
    clt_ssp370 <- clt_ssp370_126
  }
  
  ############################################################################
  if (any(grepl('sfcWind_', ssp370_126.file))){
    sfcWind_ssp370_126 <- nc_read(ssp370_126.file[grep('sfcWind_', ssp370_126.file)], varname = 'sfcWind')
    sfcWind_ssp370 <- nc_read(ssp370.file[grep('sfcWind_', ssp370.file)], varname = 'sfcWind')
  } else {
    sfcWind_ssp370_126 <- tas_ssp370_126
    sfcWind_ssp370_126[] <- 0
    sfcWind_ssp370 <- sfcWind_ssp370_126
  }
  
  
  ############################################################################
  cveg_ssp370_126 <- nc_read(ssp370_126.file[grep('cVeg_', ssp370_126.file)], varname = 'cVeg')
  cveg_ssp370 <- nc_read(ssp370.file[grep('cVeg_', ssp370.file)], varname = 'cVeg')
  
  # land use
  lu_ssp126.file <- paste0(wdir3, 'landuse_ssp126_', mod, '_R.nc')
  lu_ssp370.file <- paste0(wdir3, 'landuse_ssp370_', mod, '_R.nc')
  
  lu_ssp126 <- nc_read(lu_ssp126.file, 'landuse')
  lu_ssp370 <- nc_read(lu_ssp370.file, 'landuse')
  
  
  # tropics mask
  tropics.file <- paste0(wdir3, 'tropical_upland_mask_NA_', mod, '_R.nc')
  tropics_mask <- nc_read(tropics.file)
  ## climate impacts  #########################################################
  ## fixed climate impact based on Duveillier that linearly increases with time
  ## alternative:
  ## ssp restor tas and precip responses with increased warming
  ## are there non-linearities? e.g. is lulcc warming is the same under 
  ## ssp1 and ssp3?
  
  ## deforestation/reforestation temperature signals ###########################
  ## reforestation ssp370 climate+ssp126 land use vs ssp370 both
  
  # make sure they all have the same time length (some ssps are 85 others 86 years)
  tlen <- min(dim(tas_ssp370_126)[3], 
      dim(pr_ssp370_126)[3], 
      dim(tasmin_ssp370_126)[3], 
      dim(tasmax_ssp370_126)[3],
      dim(clt_ssp370_126)[3],
      dim(sfcWind_ssp370_126)[3],
      dim(cveg_ssp370_126)[3])

  tas_diff <- tas_ssp370_126[,,1:tlen] - tas_ssp370[,,1:tlen]
  pr_diff <- pr_ssp370_126[,,1:tlen] - pr_ssp370[,,1:tlen]
  tasmin_diff <- tasmin_ssp370_126[,,1:tlen] - tasmin_ssp370[,,1:tlen] 
  tasmax_diff <- tasmax_ssp370_126[,,1:tlen] - tasmax_ssp370[,,1:tlen] 
  dtr_diff <- (tasmax_ssp370_126[,,1:tlen] - tasmin_ssp370_126[,,1:tlen]) - 
    (tasmax_ssp370[,,1:tlen] - tasmin_ssp370[,,1:tlen])
  clt_diff <- clt_ssp370_126[,,1:tlen] - clt_ssp370[,,1:tlen]
  sfcWind_diff <- sfcWind_ssp370_126[,,1:tlen] - sfcWind_ssp370[,,1:tlen]
  lu_diff <- lu_ssp370 - lu_ssp126 # lu > 0 = less land use in ssp126/refor
  cveg_diff <- cveg_ssp370_126[,,1:tlen] - cveg_ssp370[,,1:tlen] # cveg > 0 = refor
  
  ## aggregate to annual data
  tas_diff_ann <- nc_annual(tas_diff, 'mean')
  pr_diff_ann <- nc_annual(pr_diff, 'sum')
  tasmin_diff_ann <- nc_annual(tasmin_diff, 'mean')
  tasmax_diff_ann <- nc_annual(tasmax_diff, 'mean')
  dtr_diff_ann <- nc_annual(dtr_diff, 'mean')
  sfcWind_diff_ann <- nc_annual(sfcWind_diff, 'mean')
  clt_diff_ann <- nc_annual(clt_diff, 'mean')
  cveg_diff_ann <- nc_annual(cveg_diff, 'mean')
  cveg_ann <- nc_annual(cveg_ssp370, 'mean')
  
  # mask areas with dagb >=50 Mg C ha by 2100
  tlen <- dim(cveg_diff_ann)[3]
  refor_mask_2100 <- cveg_diff_ann[,,tlen]
  refor_mask_2100[refor_mask_2100 < 5] <- NA
  refor_mask_2100[refor_mask_2100 >= 5] <- 1
  
  refor_mask_2100_tropics <- refor_mask_2100 * tropics_mask
  ind <- which(!is.na(lu_diff[,,1] * refor_mask_2100_tropics)) # lu has 3 grid cells less
  
  Arr2Dfts <- function(var.a){
    var_masked.m <- apply(var.a, 3, function(x) x[ind])
    var_masked.df <- as.data.frame(t(var_masked.m))
    var_masked.df$year <- 2015:2015+dim(var.a)[3]
    var_masked.df <- reshape::melt.data.frame(var_masked.df, id.vars = 'year')
    return(var_masked.df)
  }
  
  dtas_refor.df <- Arr2Dfts(tas_diff_ann)
  dpr_refor.df <- Arr2Dfts(pr_diff_ann)
  dtasmax_refor.df <- Arr2Dfts(tasmax_diff_ann)
  dtasmin_refor.df <- Arr2Dfts(tasmin_diff_ann)
  ddtr_refor.df <- Arr2Dfts(dtr_diff_ann)
  dsfcWind_refor.df <- Arr2Dfts(sfcWind_diff_ann)
  dclt_refor.df <- Arr2Dfts(clt_diff_ann)
  dcveg_refor.df <- Arr2Dfts(cveg_diff_ann)
  cveg_refor.df <- Arr2Dfts(cveg_ann)
  lu_refor.df <- Arr2Dfts(lu_ssp370)
  luc_refor.df <- Arr2Dfts(lu_diff)
  
  library(ggplot2)
  tas_cveg.df <- cbind(dtas_refor.df, 
                       dcveg_refor.df$value, 
                       #luc_refor.df$value,
                       dpr_refor.df$value,
                       dtasmax_refor.df$value,
                       dtasmin_refor.df$value,
                       ddtr_refor.df$value,
                       dsfcWind_refor.df$value,
                       dclt_refor.df$value)
  names(tas_cveg.df) <- c('year', 'gridcell', 'd_tas', 
                          'd_cveg',  
                          #'luc', 
                          'd_pr', 
                          'd_tasmax', 'd_tasmin', 'd_dtr', 
                          'd_sfcWind', 'd_clt')
  
  tas_cveg.dfm <- reshape::melt.data.frame(tas_cveg.df, id.vars = c('year', 'gridcell', 'd_cveg'))
  
  ## Calculate climate trend from land use change
  vars <- c(expression(Delta*'tas'), expression(Delta*'tasmin'), expression(Delta*'tasmax'), 
            expression(Delta*'dtr'), expression(Delta*'clt'), expression(Delta*'sfcWind'))
  names(vars) <- c('d_tas', 'd_tasmin', 'd_tasmax', 'd_dtr', 'd_clt', 'd_sfcWind')
  
  p <- ggplot(tas_cveg.dfm, aes(d_cveg, value, colour = gridcell)) + 
    geom_line(stat = 'smooth', method = 'lm', se = FALSE, size = 0.5, alpha = 0.5) + scale_color_manual(values = rep('darkgrey', length(ind))) + 
    theme_bw() + theme(legend.position = 'none') + 
    facet_grid(variable~., labeller = labeller(variable = vars, d_cveg = expression(Delta*'cVeg')))
  ggsave(filename = paste0(wdir3, 'climate_luc_', mod, '.pdf'), plot = p, device = 'pdf', dpi = 300)
  
  tas_coef.v <- vector()
  tasmin_coef.v <- vector()
  tasmax_coef.v <- vector()
  pr_coef.v <- vector()
  dtr_coef.v <- vector()
  sfcWind_coef.v <- vector()
  clt_coef.v <- vector()
  
  for (i in unique(tas_cveg.df$gridcell)){
    tas_coef.v <- append(tas_coef.v, coef(lm(d_tas~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    tasmin_coef.v <- append(tasmin_coef.v, coef(lm(d_tasmin~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    tasmax_coef.v <- append(tasmax_coef.v, coef(lm(d_tasmax~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    dtr_coef.v <- append(dtr_coef.v, coef(lm(d_dtr~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    pr_coef.v <- append(pr_coef.v, coef(lm(d_pr~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    sfcWind_coef.v <- append(sfcWind_coef.v, coef(lm(d_sfcWind~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
    clt_coef.v <- append(clt_coef.v, coef(lm(d_clt~d_cveg, tas_cveg.df[tas_cveg.df$gridcell %in% i, ]))[2])
  }
  
  coef2matrix <- function(coef.v){
    tropics_mask[!is.na(tropics_mask)] <- fillvalue
    coef.m <- tropics_mask
    coef.m[ind] <- coef.v
    return(coef.m)
  }
  
    ## define dimensions
    nlon <- dim(tropics_mask)[1]
    dlon <- 360 / nlon
    lon <- as.array(seq(0, 360 - dlon, dlon))
    nlat <- dim(tropics_mask)[2]
    dlat <- 180 / nlat
    lat <- as.array(seq(-90, 90 - dlat, dlat))
    
    londim <- ncdim_def('lon','degrees_east',as.double(lon)) 
    latdim <- ncdim_def('lat','degrees_north',as.double(lat)) 
    
    # define variables
    fillvalue <- 1e32
    dlname <- '2m air temperature coefficient'
    tas_def <- ncvar_def('tas','deg_C_100perc_luc',
                         list(londim, latdim),
                         fillvalue, dlname, prec='float')
    
    dlname <- 'maximum daily temperature coefficient'
    tasmax_def <- ncvar_def('tasmax','deg_C_100perc_luc',
                            list(londim, latdim),
                            fillvalue, dlname, prec='float')
    
    dlname <- 'minimum daily temperature coefficient'
    tasmin_def <- ncvar_def('tasmin','deg_C_100perc_luc',
                            list(londim, latdim),
                            fillvalue, dlname, prec='float')
    
    dlname <- 'daily temperature range coefficient'
    dtr_def <- ncvar_def('dtr','deg_C_100perc_luc',
                            list(londim, latdim),
                            fillvalue, dlname, prec='float')
    
    dlname <- 'precipitation coefficient'
    pr_def <- ncvar_def('pr','kg_m2_s_100perc_luc',
                        list(londim, latdim),
                        fillvalue, dlname, prec='float')
    
    dlname <- '10m surface wind speed coefficient'
    sfcWind_def <- ncvar_def('sfcWind','m_s_100perc_luc',
                        list(londim, latdim),
                        fillvalue, dlname, prec='float')
    
    dlname <- 'cloud cover coefficient'
    clt_def <- ncvar_def('clt','perc_100perc_luc',
                             list(londim, latdim),
                             fillvalue, dlname, prec='float')
    
    ## create nc with variables on disk
    ncout <- nc_create(filename = paste0(wdir3, 'coeffs_', mod, '.nc'), 
                       vars = list(tas_def, tasmax_def, tasmin_def, dtr_def, 
                                   sfcWind_def, pr_def, clt_def), force_v4=TRUE)
    
    ## put variables
    ncvar_put(ncout, tas_def, coef2matrix(tas_coef.v))
    ncvar_put(ncout, tasmax_def, coef2matrix(tasmax_coef.v))
    ncvar_put(ncout, tasmin_def, coef2matrix(tasmin_coef.v))
    ncvar_put(ncout, dtr_def, coef2matrix(dtr_coef.v))
    ncvar_put(ncout, pr_def, coef2matrix(pr_coef.v))
    ncvar_put(ncout, sfcWind_def, coef2matrix(sfcWind_coef.v))
    ncvar_put(ncout, clt_def, coef2matrix(clt_coef.v))
    
    ncatt_put(ncout,0, 'title', 'Climate trends (coefficients) for LUC')
    
    nc_close(ncout)
}

############################################################################
library(ncdf4)
nc2df <- function(nc.file){
  print(nc.file)
  nc <- nc_open(nc.file)
  s <- unlist(strsplit(nc.file, '_'))
  df <- data.frame(tas=ncvar_get(nc, 'tas'),
                   tasmin=ncvar_get(nc, 'tasmin'),
                   tasmax=ncvar_get(nc, 'tasmax'),
                   dtr=ncvar_get(nc, 'dtr'),
                   pr=ncvar_get(nc, 'pr'),
                   sfcWind=ncvar_get(nc, 'sfcWind'),
                   clt=ncvar_get(nc, 'clt'),
                   model=s[4],
                   continent=gsub('.nc', '', s[6]),
                   type=s[5])
  nc_close(nc)
  return(df)
}

nc.files <- list.files('~/data/future_forests/biophysical_coeffs', full.names = TRUE)
nc.files <- nc.files[grep(paste0(c('global', 'Africa', 'Americas', 'Asia'), collapse = '|'), nc.files)]


nc.dfs <- lapply(nc.files, nc2df)
nc.dfs <- do.call('rbind', nc.dfs)
write.csv(nc.dfs, '~/data/future_forests/biophysical_coeffs/model_coeffs.csv')


library(ggplot2) # boxplots
df <- read.csv('/home/ucfaako/Documents/future_forests/data/model_coeffs.csv')
df$X <- NULL
df <- reshape::melt.data.frame(df, id.vars = c('model', 'continent', 'type'))
df.summary <- aggregate.data.frame(df$value, by = list(continent=df$continent, variable=df$variable, type=df$type), FUN = mean)
df.summary[df.summary$type=='means',]
df.summary[df.summary$type=='qt25',]
df.summary[df.summary$type=='qt75',]

ggplot(df, aes(value, model)) + geom_col(width = 0.1) + 
  facet_grid(continent~variable, scales = 'free_x') + theme_bw()

'biogeophyscial_response.pdf'
