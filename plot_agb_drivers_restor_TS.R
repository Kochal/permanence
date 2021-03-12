library(ncdf4); library(ggplot2); library(reshape)
source('/home/akoch/scripts/R_code/fun_Utility.R')
# source('~/Documents/R_code/fun_Utility.R')

# wdir <- '/home/ucfaako/Documents/future_forests/data/'
carbon <- 't3C'
carbon_dir <- 'totc'
carbon_name <- 'Total carbon'
wdir <- paste0('/home/terraces/projects/LPJ_futuretropics/', carbon_dir, '_restor/')

co2_1850_restor.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_restor_fixco2_1850_fire_ssp*_ensmean.nc')), full.names = TRUE)
co2_2014_restor.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_restor_fixco2_2014_fire_ssp*_ensmean.nc')), full.names = TRUE)
restor.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_restor_fire_ssp*_ensmean.nc')), full.names = TRUE)
ctl.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_ctl_fire_ssp*_ensmean.nc')), full.names = TRUE)
co2_1850_ctl.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_ctl_fixco2_1850_fire_ssp*_ensmean.nc')), full.names = TRUE)
co2_2014_ctl.file <- list.files(wdir, pattern = glob2rx(paste0(carbon, '_ctl_fixco2_2014_fire_ssp*_ensmean.nc')), full.names = TRUE)

ssp <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
ssp_labs <- c('SSP1-26', 'SSP2-45', 'SSP3-70', 'SSP5-85')
names(ssp_labs) <- ssp
scenario <- c('ctl', 'restor', 'restor_co2_2014', 'restor_co2_1850', 'ctl_co2_2014', 'ctl_co2_1850')
coln <- paste(rep(scenario, each = length(ssp)), ssp, sep = '_')

tx <- 2014:2100

sim.names <- c('SSP landuse (2014)', 
               'Restoration',
               expression('SSP landuse (2014) + fixed'~CO[2]~'(2014)'),
               expression('Restoration + fixed'~CO[2]~'(2014)'),
               expression('SSP landuse (2014) + fixed'~CO[2]~'(1850)'), 
               expression('Restoration + fixed'~CO[2]~'(1850)'))

NC2Df <- function(varname){
  co2_1850_restor.a <- lapply(co2_1850_restor.file, nc_read, varname)
  co2_2014_restor.a <- lapply(co2_2014_restor.file, nc_read, varname)
  restor.a <- lapply(restor.file, nc_read, varname)
  ctl.a <- lapply(ctl.file, nc_read, varname)
  co2_1850_ctl.a <- lapply(co2_1850_ctl.file, nc_read, varname)
  co2_2014_ctl.a <- lapply(co2_2014_ctl.file, nc_read, varname)
  
  df <- data.frame(ctl=do.call('cbind', ctl.a), 
                   restor=do.call('cbind', restor.a),
                   restor_co2_2014=do.call('cbind', co2_2014_restor.a), 
                   restor_co2_1850=do.call('cbind', co2_1850_restor.a),
                   ctl_co2_2014=do.call('cbind', co2_2014_ctl.a), 
                   ctl_co2_1850=do.call('cbind', co2_1850_ctl.a)) 
  names(df) <- coln
  # if (carbon=='totc'){
  #   df[-1,c(5:ncol(df))] <- df[-1,c(5:ncol(df))] + 8.5e15
  # } else {
  #   df[-1,c(5:ncol(df))] <- df[-1,c(5:ncol(df))] - 3e15
  # }
  df$date <- tx
  df <- melt.data.frame(df, id.vars = 'date')
  df$variable <- as.character(df$variable)
  df$value <- df$value * 1e-15
  df$ssp <- substring(df$variable, nchar(df$variable)-5, nchar(df$variable))
  df$scenario <- substring(df$variable, 1, nchar(df$variable)-7)
  return(df)
}

mean.df <- NC2Df('mean')
conf1.df <- NC2Df('conf1')
conf2.df <- NC2Df('conf2')

mean.df$lower <- conf1.df$value
mean.df$upper <- conf2.df$value

mean.df$scenario <- factor(mean.df$scenario, levels = c('ctl', 'restor', 
                                                        'ctl_co2_2014', 'restor_co2_2014',  
                                                        'ctl_co2_1850', 'restor_co2_1850'))

if (carbon=='totc'){
  agb_1970 <- 138.4146 / 100 # to Pg C percent
} else {
  agb_1970 <- 152.2093 / 100 # to Pg C percent
}

mean.df <- mean.df[!mean.df$scenario %in% c('ctl_co2_1850', 'restor_co2_1850'),]


## plots #######################################################################
clr <- c('#8c2d04','#084594', '#f16913', '#4292c6', '#fdae6b', '#9ecae1')
p <- ggplot(mean.df, aes(date, value,  color = scenario)) + geom_line(stat='identity')
p <- p + geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), colour = NA, alpha = 0.2) + guides(color = guide_legend(ncol=2), fill=guide_legend(ncol=2))
p <- p + scale_fill_discrete(type = clr, name = '', labels = sim.names) + scale_color_discrete(type = clr, name = '', labels = sim.names)
# p <- p + scale_fill_manual(values = clr) + scale_color_manual(values = clr)
p <- p + scale_x_continuous(breaks = seq(2015, 2100, 15))
#p <- p + scale_y_continuous(sec.axis = sec_axis(trans=~./agb_1970, name = paste(carbon_name, 'change w.r.t. 1971-2000 (%)'), breaks = seq(0, 150, 25)))
p <- p + labs(title = 'Total carbon change', x = 'Year', y = paste(carbon_name, 'change w.r.t. 2014 (Pg C)')) + theme_bw(base_size = 12) + theme(legend.position = 'top')
p <- p + facet_wrap(ssp~., labeller = labeller(ssp=ssp_labs))
ggsave(paste0(wdir, 'd',carbon, '_2014-2100_ts.pdf'), 
       plot = p, device = 'pdf', width = 210, height = 170, units = 'mm', dpi = 300)






## 
diffmean.df <- rbind(
  data.frame(date=mean.df$date[mean.df$scenario=='restor'], 
           value=mean.df$value[mean.df$scenario=='restor'] - mean.df$value[mean.df$scenario=='ctl'], 
           lower=mean.df$lower[mean.df$scenario=='restor'] - mean.df$lower[mean.df$scenario=='ctl'],
           upper=mean.df$upper[mean.df$scenario=='restor'] - mean.df$upper[mean.df$scenario=='ctl'],
           ssp=mean.df$ssp[mean.df$scenario=='restor'], scenario='transient CO2'),
  data.frame(date=mean.df$date[mean.df$scenario=='restor_co2_2014'], 
             value=mean.df$value[mean.df$scenario=='restor_co2_2014'] - mean.df$value[mean.df$scenario=='ctl_co2_2014'], 
             lower=mean.df$lower[mean.df$scenario=='restor_co2_2014'] - mean.df$lower[mean.df$scenario=='ctl_co2_2014'],
             upper=mean.df$upper[mean.df$scenario=='restor_co2_2014'] - mean.df$upper[mean.df$scenario=='ctl_co2_2014'],
             ssp=mean.df$ssp[mean.df$scenario=='restor_co2_2014'], scenario='fixed CO2 (2014)')
  # data.frame(date=mean.df$date[mean.df$scenario=='restor_co2_1850'], 
  #            value=mean.df$value[mean.df$scenario=='restor_co2_1850'] - mean.df$value[mean.df$scenario=='ctl_co2_1850'], 
  #            lower=mean.df$lower[mean.df$scenario=='restor_co2_1850'] - mean.df$lower[mean.df$scenario=='ctl_co2_1850'],
  #            upper=mean.df$upper[mean.df$scenario=='restor_co2_1850'] - mean.df$upper[mean.df$scenario=='ctl_co2_1850'],
  #            ssp=mean.df$ssp[mean.df$scenario=='restor_co2_1850'], scenario='fixed CO2 (1850)')
  )

ssp_colours <- c('#66A36C', '#3DACC6', '#D28E54', '#C3474E')
diffmean.df$scenario <- factor(diffmean.df$scenario, levels = c('transient CO2', 'fixed CO2 (2014)', 'fixed CO2 (1850)'), 
                               labels = c(bquote(transient~CO[2]), bquote(fixed~CO[2]~'(2014)'), bquote(fixed~CO[2]~'(1850)')))

## expression in labels

p2 <- ggplot(diffmean.df, aes(date, value,  color = ssp)) + geom_line(stat='identity')
p2 <- p2 + geom_ribbon(aes(ymin = lower, ymax = upper, fill = ssp), colour = NA, alpha = 0.2)
p2 <- p2 + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
p2 <- p2 + scale_x_continuous(breaks = seq(2015, 2100, 25))
p2 <- p2 + labs(title = 'Total carbon gain from restoration', x = 'Year', y = paste('Total biomass carbon (Pg C)')) 
p2 <- p2 + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank())
p2 <- p2 + facet_wrap(scenario~., labeller = label_parsed)
p2 <- p2 + coord_trans(y = squash_axis(0, 15, 10))
ggsave(paste0(wdir, 'diff',carbon, '_2014-2100_ts.pdf'), 
       plot = p2, device = 'pdf', width = 210, height = 120, units = 'mm', dpi = 300)

## FIRST DERIVATIVE
for (s in ssp){
  for (sc in unique(diffmean.df$scenario)){
    diffmean.df$diff[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- c(0, diff(diffmean.df$value[diffmean.df$ssp==s & diffmean.df$scenario==sc]))
    diffmean.df$lower[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- c(0, diff(diffmean.df$lower[diffmean.df$ssp==s & diffmean.df$scenario==sc]))
    diffmean.df$upper[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- c(0, diff(diffmean.df$upper[diffmean.df$ssp==s & diffmean.df$scenario==sc]))
  }
}

## uptake rates and trends ###################################################
# uptake in 2030

diffmean.df[diffmean.df$date==2031 & diffmean.df$scenario=='transient ~ CO[2]',]
diffmean.df$diff[diffmean.df$date==2031 & diffmean.df$scenario=='transient ~ CO[2]'] - 
  diffmean.df$lower[diffmean.df$date==2031 & diffmean.df$scenario=='transient ~ CO[2]']

diffmean.df[diffmean.df$date==2032 & diffmean.df$scenario=='transient ~ CO[2]',]
diffmean.df$diff[diffmean.df$date==2032 & diffmean.df$scenario=='transient ~ CO[2]'] - 
  diffmean.df$lower[diffmean.df$date==2032 & diffmean.df$scenario=='transient ~ CO[2]']

diffmean.df[diffmean.df$date==2050 & diffmean.df$scenario=='transient ~ CO[2]',]
diffmean.df$diff[diffmean.df$date==2050 & diffmean.df$scenario=='transient ~ CO[2]'] - 
  diffmean.df$lower[diffmean.df$date==2050 & diffmean.df$scenario=='transient ~ CO[2]']

diffmean.df[diffmean.df$date==2100 & diffmean.df$scenario=='transient ~ CO[2]',]
diffmean.df$diff[diffmean.df$date==2100 & diffmean.df$scenario=='transient ~ CO[2]'] - 
  diffmean.df$lower[diffmean.df$date==2100 & diffmean.df$scenario=='transient ~ CO[2]']

# trend
for (SSP in unique(diffmean.df$ssp)){
  tnd.df <- diffmean.df[diffmean.df$date >= 2032 & diffmean.df$ssp==SSP & diffmean.df$scenario=='transient ~ CO[2]',]
  tnd <- lm(diff ~ date, tnd.df)
  tnd <- summary(tnd)
  print(paste0('Trend ', tnd$coefficients[2], ' p ', tnd$coefficients[8]))
}

diffmean.df[diffmean.df$date==2031 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"',]
diffmean.df$diff[diffmean.df$date==2031 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"'] - 
  diffmean.df$lower[diffmean.df$date==2031 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"']

diffmean.df[diffmean.df$date==2032 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"',]
diffmean.df$diff[diffmean.df$date==2032 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"'] - 
  diffmean.df$lower[diffmean.df$date==2032 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"']

diffmean.df[diffmean.df$date==2050 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"',]
diffmean.df$diff[diffmean.df$date==2050 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"'] - 
  diffmean.df$lower[diffmean.df$date==2050 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"']

diffmean.df[diffmean.df$date==2100 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"',]
diffmean.df$diff[diffmean.df$date==2100 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"'] - 
  diffmean.df$lower[diffmean.df$date==2100 & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"']

for (SSP in unique(diffmean.df$ssp)){
  tnd.df <- diffmean.df[diffmean.df$date >= 2032 & diffmean.df$ssp==SSP & diffmean.df$scenario=='fixed ~ CO[2] ~ "(2014)"',]
  tnd <- lm(diff ~ date, tnd.df)
  tnd <- summary(tnd)
  print(paste0('Trend ', tnd$coefficients[2], ' p ', tnd$coefficients[8]))
}

#############################################################################
p3 <- ggplot(diffmean.df, aes(date, diff,  color = ssp)) + geom_line(stat='identity')
p3 <- p3 + geom_ribbon(aes(ymin = lower, ymax = upper, fill = ssp), colour = NA, alpha = 0.2)
p3 <- p3 + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
p3 <- p3 + scale_x_continuous(breaks = seq(2015, 2100, 25))# + scale_y_continuous(limits=c(-0.25, 0.75))
p3 <- p3 + labs(title = 'Annual change in total carbon in restration area w.r.t. control', x = 'Year', 
                y = expression('Total carbon gain from restoration (Pg C year'^{-1}*')')) 
p3 <- p3 + theme_bw(base_size = 12) + theme(legend.position = 'top')
p3 <- p3 + facet_wrap(scenario~., labeller = label_parsed) + coord_trans(y = squash_axis(0.3, 1.4, 10))
ggsave(paste0(wdir, 'derivative_',carbon, '_2014-2100_ts.pdf'), 
       plot = p3, device = 'pdf', width = 210, height = 120, units = 'mm', dpi = 300)

library(zoo)
for (s in ssp){
  for (sc in unique(diffmean.df$scenario)){
    diffmean.df$diff[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- rollmean(diffmean.df$diff[diffmean.df$ssp==s & diffmean.df$scenario==sc], 10, align = 'right', na.pad = TRUE)
    diffmean.df$upper[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- rollmean(diffmean.df$upper[diffmean.df$ssp==s & diffmean.df$scenario==sc], 10, align = 'right', na.pad = TRUE)
    diffmean.df$lower[diffmean.df$ssp==s & diffmean.df$scenario==sc] <- rollmean(diffmean.df$lower[diffmean.df$ssp==s & diffmean.df$scenario==sc], 10, align = 'right', na.pad = TRUE)
  }
}

p4 <- ggplot(diffmean.df, aes(date, diff,  color = ssp)) + geom_line(stat='identity')
p4 <- p4 + geom_ribbon(aes(ymin = lower, ymax = upper, fill = ssp), colour = NA, alpha = 0.2)
p4 <- p4 + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
p4 <- p4 + scale_x_continuous(breaks = seq(2015, 2100, 25))# + scale_y_continuous(limits=c(-0.25, 0.75))
p4 <- p4 + labs(title = 'Annual change in total carbon in restration area w.r.t. control', x = 'Year', 
                y = expression('Total carbon gain from restoration (Pg C year'^{-1}*')')) 
p4 <- p4 + theme_bw(base_size = 12) + theme(legend.position = 'top')
p4 <- p4 + facet_wrap(scenario~., labeller = label_parsed) + coord_trans(y = squash_axis(0.3, 1.4, 10))
ggsave(paste0(wdir, 'derivative_',carbon, '_2014-2100_ts_smooth.pdf'), 
       plot = p4, device = 'pdf', width = 190, height = 120, units = 'mm', dpi = 300)
ggarrange(list(p2, p3), ncol = 1, labels = 'AUTO', common.legend = TRUE)