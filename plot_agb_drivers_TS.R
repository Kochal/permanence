library(ncdf4); library(ggplot2); library(reshape)
source('/home/akoch/scripts/R_code/fun_Utility.R')
# source('~/Documents/R_code/fun_Utility.R')

wdir <- '/home/terraces/projects/LPJ_futuretropics/agb/'
# wdir <- '/home/ucfaako/Documents/future_forests/data/'
co2_restor.file <- list.files(wdir, pattern = glob2rx('agb_fixco2_ssp*_ensmean.nc'), full.names = TRUE)
lu.file <- list.files(wdir, pattern = glob2rx('agb_fixlu_ssp*_ensmean.nc'), full.names = TRUE)
clim.file <- list.files(wdir, pattern = glob2rx('agb_climate_ssp*_ensmean.nc'), full.names = TRUE)
all.file <- list.files(wdir, pattern = glob2rx('agb_all_ssp*_ensmean.nc'), full.names = TRUE)
co2_s2014.file <- list.files(wdir, pattern = glob2rx('agb_fixco2_2014_ssp*_ensmean.nc'), full.names = TRUE)

co2_2014_restor.file

co2.a <- lapply(co2.file, nc_read, 'mean')
lu.a <- lapply(lu.file, nc_read, 'mean')
clim.a <- lapply(clim.file, nc_read, 'mean')
all.a <- lapply(all.file, nc_read, 'mean')
co2_2014.a <- lapply(co2_s2014.file, nc_read, 'mean')


ssp <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
scenario <- c('co2', 'climate', 'lu', 'all', 'co2_2014')
coln <- paste(rep(scenario, each = length(ssp)), ssp, sep = '_')


# tx.nc <- nc_open(co2.file)
# tx <- ncvar_get(tx.nc, 'time')
# nc_close(tx.nc)
# tx <- (1850 + tx) 
tx <- 1860:2100

sim.names <- c(expression('fixed'~CO[2]~'(1850)'), 
               'fixed land use (1850)', 
               'fixed climate (1850)', 
               'all transient', 
               expression('fixed'~CO[2]~'(2014)'))

df <- data.frame(co2=do.call('cbind', co2.a), 
                 climate=do.call('cbind', clim.a), 
                 land_use=do.call('cbind', lu.a), 
                 all=do.call('cbind', all.a), 
                 co2_2014=do.call('cbind', co2_2014.a))
names(df) <- coln
df$date <- tx

df <- df[df$date >=1970, ]

df <- melt.data.frame(df, id.vars = 'date')
df$variable <- as.character(df$variable)
df$ssp <- substring(df$variable, nchar(df$variable)-5, nchar(df$variable))
df$scenario <- substring(df$variable, 1, nchar(df$variable)-7)
df$scenario <- factor(df$scenario, levels = scenario)
# 1971-2000
p <- ggplot(df, aes(date, value  * 1e-15,  color = scenario)) + geom_line(stat='identity')
p <- p + scale_fill_discrete(name = '', labels = sim.names) + scale_color_discrete(name = '', labels = sim.names)
p <- p + scale_y_continuous(sec.axis = sec_axis(trans=~./4.0927, name = 'AGB change w.r.t. 1970 (%)', breaks = c(0, 25, 50)))
p <- p + labs(x = 'Year', y = 'AGB change w.r.t. 1970 (Pg C)')+ theme_bw(base_size = 12)
p <- p + facet_wrap(ssp~.)
ggsave(paste0(wdir, 'dAGB_1970-2100_ts.pdf'), plot = p, device = 'pdf', width = 190, height = 170, units = 'mm', dpi = 300)