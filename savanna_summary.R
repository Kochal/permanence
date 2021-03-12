library(ncdf4); library(ggplot2); library(raster); library(exactextractr); library(sf); library(dplyr)
source('/home/akoch/scripts/R_code/fun_Utility.R')

wdir <- '/home/terraces/projects/LPJ_futuretropics/fire/'

nosav.file <- list.files(wdir, 't3_burnedf_nosav', full.names = TRUE)
sav.file <- list.files(wdir, 't3_burnedf_sav', full.names = TRUE)

nosav.file <- nosav.file[grep('adj', nosav.file)]
sav.file <- sav.file[grep('adj', sav.file)]

continent.file <- '/home/akoch/data/carbon_offsets/ne_10m_admin_0_countries.shp'
shp <- read_sf(continent.file)

subcontinent <- c('Bangladesh', 'Bhutan', 'India', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka')
shp$CONTINENT[shp$SOVEREIGNT %in% subcontinent] <- 'Indian subcontinent'
shp$CONTINENT[shp$CONTINENT=='Seven seas (open ocean)'] <- 'Africa'
shp$CONTINENT[shp$CONTINENT=='North America'] <- 'South America'


shp_agg <- shp %>% 
  group_by(CONTINENT) %>%
  summarise()

df.l <- list()
dfout.l <- list()
decades <- c(2030, 2050, 2090)
ts <- c(7:16, 27:36 ,77:86)

continent_scale <- TRUE

for (i in 1:length(sav.file)){
  for (yr in 1:3){
    fname <- gsub(wdir, '', sav.file[[i]])
    fname <- unlist(strsplit(fname, '_'))
    ssp <- substr(fname[length(fname)], nchar(fname[length(fname)])-8, nchar(fname[length(fname)])-3)
    model <- fname[length(fname)-1]
    
    if (continent_scale){
      sav.nc <- raster::stack(sav.file[[i]])
      sav_2090s.nc <- stackApply(sav.nc[[ts[yr]]], 1, mean)
      sav_2090s.nc <- exactextractr::exact_extract(sav_2090s.nc, shp_agg)
      sav_2090s.nc <- lapply(sav_2090s.nc, FUN = function(x) x$value[!is.na(x$value) & x$value>0])
      names(sav_2090s.nc) <- unique(shp_agg$CONTINENT)
      
      nosav.nc <- raster::stack(nosav.file[[i]])
      nosav_2090s.nc <- stackApply(nosav.nc[[ts[yr]]], 1, mean)
      nosav_2090s.nc <- exactextractr::exact_extract(nosav_2090s.nc, shp_agg)
      nosav_2090s.nc <- lapply(nosav_2090s.nc, FUN = function(x) x$value[!is.na(x$value) & x$value>0])
      names(nosav_2090s.nc) <- unique(shp_agg$CONTINENT)
      
      n_nosav <- lapply(nosav_2090s.nc, length)
      n_sav <- lapply(sav_2090s.nc, length)
      
      nosav_mean <- lapply(nosav_2090s.nc, mean)
      sav_mean <- lapply(sav_2090s.nc, mean)
      
      nosav_sd <- lapply(nosav_2090s.nc, sd)
      sav_sd <- lapply(sav_2090s.nc, sd)
      
      out <- list()
      cn_out <- list()
      for (cn in 1:length(sav_2090s.nc)){
        if (n_nosav[[cn]] > 5 && n_sav[[cn]] > 5){
          ttest <- t.test(sav_2090s.nc[[cn]], nosav_2090s.nc[[cn]], var.equal = FALSE)
          out[[cn]] <- round(ttest$p.value, 4)
        } else {
          out[[cn]] <- NA
        }
        cn_out[[cn]] <- data.frame(nosav_mean=nosav_mean[[cn]], 
                                 sav_mean=sav_mean[[cn]], 
                                 nosav_ci=(1.96 * (nosav_sd[[cn]] / sqrt(n_nosav[[cn]]))), 
                                 sav_ci=(1.96 * (sav_sd[[cn]] / sqrt(n_sav[[cn]]))), 
                                 p = out[[cn]],
                                 n_sav = n_sav[[cn]],
                                 n_nosav = n_nosav[[cn]],
                                 ssp = ssp,
                                 model = model,
                                 time = decades[yr],
                                 region = names(sav_2090s.nc)[[cn]])
      }
      df.l[[yr]] <- do.call('rbind', cn_out)
    } else {
      nosav.nc <- nc_read(nosav.file[[i]])
      sav.nc <- nc_read(sav.file[[i]])
      
      nosav_2090s.nc <- apply(nosav.nc[,,ts[yr]], 1:2, mean, na.rm=T)
      nosav_2090s.nc <- nosav_2090s.nc[!is.na(nosav_2090s.nc) & nosav_2090s.nc>0]
      n_nosav <- length(nosav_2090s.nc)
      
      sav_2090s.nc <- apply(sav.nc[,,ts[yr]], 1:2, mean, na.rm=T)
      sav_2090s.nc <- sav_2090s.nc[!is.na(sav_2090s.nc) & sav_2090s.nc>0]
      n_sav <- length(sav_2090s.nc)
      
      out <- t.test(sav_2090s.nc, nosav_2090s.nc, var.equal = FALSE)
      
      df.l[[yr]] <- data.frame(nosav_mean=mean(nosav_2090s.nc), 
                               sav_mean=mean(sav_2090s.nc), 
                               nosav_ci=(1.96 * (sd(nosav_2090s.nc) / n_nosav)), 
                               sav_ci=(1.96 * (sd(sav_2090s.nc) / n_sav)), 
                               p = round(out$p.value, 4),
                               n_sav = n_sav,
                               n_nosav = n_nosav,
                               ssp = ssp,
                               model = model,
                               time = decades[yr],
                               region = 'global')
    }
    }
    
    
  dfout.l[[i]] <- do.call('rbind', df.l)
}
final.df <- do.call('rbind', dfout.l)
if (continent_scale){
  write.csv(final.df, paste0(wdir, 'savanna_burning_continents_adj.csv'), row.names = FALSE)
} else {
  write.csv(final.df, paste0(wdir, 'savanna_burning_global_adj.csv'), row.names = FALSE)
}

if (continent_scale){
  final.df <- read.csv(paste0(wdir, 'savanna_burning_continents_adj.csv'))
} else {
  final.df <- read.csv(paste0(wdir, 'savanna_burning_global_adj.csv'))
}

final.df <- reshape::melt.data.frame(final.df, measure.vars = c('nosav_mean', 'sav_mean'))

ssp <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
ssp_labs <- c('SSP1-26', 'SSP2-45', 'SSP3-70', 'SSP5-85')
names(ssp_labs) <- ssp

final.df$variable <- as.character(final.df$variable)
final.df$variable[final.df$variable=='sav_mean'] <- 'savanna'
final.df$variable[final.df$variable=='nosav_mean'] <- 'no savanna'
final.df$variable <- factor(final.df$variable, levels = c('savanna', 'no savanna'))
final.df$time <- paste0(final.df$time, 's')

ssp_colours <- c('#66A36C', '#3DACC6', '#D28E54', '#C3474E')
bp <- ggplot(final.df, aes(x = ssp, y = value * 100, color = ssp, linetype = variable)) + geom_boxplot() 
bp <- bp + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs)
bp <- bp + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
bp <- bp + labs(title = 'Burned restoration area in- and outside savannas', x = '', y = 'Burned proportion of restoration area (%)')
bp <- bp + facet_wrap(. ~ time, ncol = 3) + theme_bw(base_size = 13) + theme(legend.title = element_blank())
ggsave(paste0(wdir, 'burned_restor_sav_nosav.pdf'), 
       plot = bp, device = 'pdf', width = 170, height = 120, units = 'mm', dpi = 300) 

ts <- c('2030s', '2050s', '2090s')
summary.df <- data.frame(region=rep(as.character(unique(final.df$region)), each = 12),
                         decade=rep(ts, length.out = 84),
                         ssp=rep(ssp, each = 3))

summary.df$mmm_sav <- NA
summary.df$mmm_nosav <- NA
summary.df$ci95_sav <- NA
summary.df$ci95_nosav <- NA
summary.df$n_min_sav <- NA
summary.df$n_max_sav <- NA
summary.df$n_min_nosav <- NA
summary.df$n_max_nosav <- NA

final.df$time[final.df$time=='2100s'] <- '2090s'

for (continent in unique(summary.df$region)){
  for (SSP in unique(summary.df$ssp)){
    for (yr in unique(summary.df$decade)){
      nosav <- final.df[final.df$variable=='no savanna' & final.df$ssp==SSP & final.df$time==yr & final.df$region==continent,]
      sav <- final.df[final.df$variable=='savanna' & final.df$ssp==SSP & final.df$time==yr & final.df$region==continent,]
      summary.df$mmm_sav[summary.df$ssp==SSP & summary.df$decade==yr & summary.df$region==continent] <- 
        weighted.mean(sav$value, sav$n_sav)
      summary.df$mmm_nosav[summary.df$ssp==SSP & summary.df$decade==yr & summary.df$region==continent] <- 
        weighted.mean(nosav$value, nosav$n_nosav)
      
      summary.df$ci95_sav[summary.df$ssp==SSP & summary.df$decade==yr & summary.df$region==continent] <- 
        weighted.mean(sav$sav_ci, sav$n_sav)
      summary.df$ci95_nosav[summary.df$ssp==SSP & summary.df$decade==yr & summary.df$region==continent] <- 
        weighted.mean(nosav$nosav_ci, nosav$n_sav)
      
    }
  }
}

if (continent_scale){
  write.csv(summary.df, paste0(wdir, 'savanna_burning_continents_summary_adj.csv'), row.names = FALSE)
} else {
  write.csv(summary.df, paste0(wdir, 'savanna_burning_global_summary_adj.csv'), row.names = FALSE)
}



df <- read.csv(paste0(wdir, 'savanna_burning_continents_adj.csv'))
df_ssp <- df[df$time!=2050,]
df_ssp <- aggregate.data.frame(df_ssp[,c('n_sav', 'n_nosav')], FUN = sum, by = list(df_ssp$ssp, df_ssp$model, df_ssp$time))
df_ssp[which.min(df_ssp$n_sav),]
df_ssp[which.min(df_ssp$n_nosav),]

df_ssp[which.max(df_ssp$n_sav),]
df_ssp[which.max(df_ssp$n_nosav),]