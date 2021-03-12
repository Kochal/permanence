library(ncdf4); library(ggplot2); library(reshape)
source('/home/akoch/scripts/R_code/fun_Utility.R')

wdir <- paste0('/home/terraces/projects/LPJ_futuretropics/fire/')

restor.file <- list.files(wdir, pattern = glob2rx('t3_burneda*_ensmean.nc'), full.names = TRUE)

ssp <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
ssp_labs <- c('SSP1-26', 'SSP2-45', 'SSP3-70', 'SSP5-85')
names(ssp_labs) <- ssp

tx <- 2014:2100

Nc2Df <- function(x, varname){
  x.a <- lapply(x, nc_read, varname)
  x.df <- do.call('cbind', x.a)
  x.df <- as.data.frame(x.df)
  names(x.df) <- ssp
  x.df$date <- tx
  x.dfm <- reshape::melt.data.frame(x.df, id.vars = 'date')
  return(x.dfm)
}

out.df <- Nc2Df(restor.file, 'mean')
ci_lower <- Nc2Df(restor.file, 'conf1')
ci_upper <- Nc2Df(restor.file, 'conf2')

out.df$value <- out.df$value * 1e-10
out.df$lower <- ci_lower$value * 1e-10
out.df$upper <- ci_upper$value * 1e-10

# out.df$value <- out.df$value - out.df$value[out.df$date==2014]
# out.df$lower <- out.df$lower - out.df$lower[out.df$date==2014]
# out.df$upper <- out.df$upper - out.df$upper[out.df$date==2014]
restoration_area <- 126.38
ssp_colours <- c('#66A36C', '#3DACC6', '#D28E54', '#C3474E')
p <- ggplot(out.df, aes(date, value, color = variable)) + geom_line(stat='identity')
p <- p + geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), colour = NA, alpha = 0.2)
p <- p + scale_y_continuous(sec.axis = sec_axis(trans = ~./(restoration_area/100), name = '(%) of total restoration area'))
p <- p + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
p <- p + scale_x_continuous(breaks = seq(2015, 2100, 15))
p <- p + labs(title = 'Burned restoration area change w.r.t. 2014', x = 'Year', y = paste('Burned area (Mha)')) 
p <- p + theme_bw(base_size = 12) + theme(legend.position = 'top')
ggsave(paste0(wdir, 'burneda_2014-2100_ts.pdf'), 
       plot = p, device = 'pdf', width = 210, height = 150, units = 'mm', dpi = 300)
