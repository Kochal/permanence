library(ggplot2); library(ncdf4)
source('/home/akoch/scripts/R_code/fun_Utility.R')
wdir <- '/home/terraces/projects/LPJ_futuretropics/totc_restor/'
files <- list.files(wdir, pattern = 'csv', full.names = TRUE)

ReadCSV <- function(x, region){
  # get scenario
  fname <- gsub(wdir, '', x)
  fname <- unlist(strsplit(fname, '_'))
  ssp_ind <- grep('ssp', fname)
  scenario <- paste0(fname[(ssp_ind+1):(length(fname)-2)], collapse = '_')
  # get ssp
  ssp <- fname[ssp_ind]
  # get year
  year <- fname[length(fname)-1]
  
  # read in data
  df <- read.csv(x)
  
  continents <- c('Africa', 'South America', 'North America', 
                  'Indian subcontinent', 'Europe', 'Asia', 'Oceania')
  
  if (region=='country'){
    df <- df[!df$Region %in% continents,]
  }
  if (region=='continent'){
    df <- df[df$Region %in% continents,]
    df$Region[df$Region=='Seven seas (open ocean)'] <- 'Africa'
    df <- aggregate.data.frame(df[,paste0('model',1:(ncol(df)-1))], by = list(Region = df$Region), FUN = sum)
  }
  if (region=='world'){
    df <- df[df$Region %in% continents,]
    df$Region <- 'world'
    df <- aggregate.data.frame(df[,paste0('model',1:(ncol(df)-1))], by = list(Region = df$Region), FUN = sum)
  }
  mmm <- apply(df[, 2:ncol(df)], 1, FUN=mean)
  ci95 <- apply(df[, 2:ncol(df)], 1, FUN = function(x) {1.96 * (sd(x) / sqrt(13))})
  df$mmm <- mmm
  df$ci95 <- ci95
  df$scenario <- scenario
  df$ssp <- ssp
  df$year <- year
  return(df)
}

DiffScenarios <- function(df, sc1, sc2, scname){
  df$ci95 <- NULL
  dfm <- reshape::melt.data.frame(df, id.vars = c('Region', 'scenario', 'ssp', 'year'))
  template.df <- dfm[dfm$scenario==sc1, ]
  template.df$scenario <- scname
  template.df$value <- dfm$value[dfm$scenario==sc1] - dfm$value[dfm$scenario==sc2]
  
  ci_template.df <- template.df[template.df$variable=='mmm',]
  ci_template.df$variable <- 'ci95'


  ## calculate ci
  for (REG in unique(df$Region)){
    for (SSP in unique(df$ssp)){
      for (YEAR in c(2020, 2030, 2050, 2090)){
        tmp.df <- template.df$value[template.df$ssp==SSP & 
                                      template.df$year==YEAR & 
                                      template.df$variable!='mmm' & 
                                      template.df$Region==REG]
        ci_tmp.df <- ci_template.df$value[ci_template.df$ssp==SSP & 
                                            ci_template.df$year==YEAR & 
                                            ci_template.df$variable!='mmm' & 
                                            ci_template.df$Region==REG]
        ci_tmp.df <- 1.96 * (sd(tmp.df) / sqrt(13))
        ci_template.df$value[ci_template.df$ssp==SSP & 
                               ci_template.df$year==YEAR & 
                               ci_template.df$variable!='mmm' & 
                               ci_template.df$Region==REG] <- ci_tmp.df
      }
    }
  }

  template.df <- rbind(template.df, ci_template.df)
  return(template.df)
}

PrettyTable <- function(df, multiplier, variable_name, pretty_vals=FALSE, outname){
  df$scenario <- NULL
  df$value <- round(df$value * multiplier, 2)
  vals <- paste0(df$value[df$variable=='mmm'], ' \u00B1',df$value[df$variable=='ci95'])
  if (pretty_vals){
    df <- data.frame(ssp=df$ssp[df$variable=='mmm'],
                     year=df$year[df$variable=='mmm'],
                     value=vals,
                     region=df$Region[df$variable=='mmm'])
  } else {
    df <- data.frame(ssp=df$ssp[df$variable=='mmm'],
                     year=df$year[df$variable=='mmm'],
                     value=df$value[df$variable=='mmm'],
                     ci95=df$value[df$variable=='ci95'],
                     region=df$Region[df$variable=='mmm'])
  }
  
  names(df)[3] <- variable_name
  write.csv(df, paste0(wdir, outname), row.names = FALSE)
}

BoxPlot <- function(df, title){
  ssp_colours <- c('#66A36C', '#3DACC6', '#D28E54', '#C3474E')
  df <- df[df$variable %in% paste0('model', 1:13),]
  bp <- ggplot(df, aes(x = ssp, y = value * 1e-15, color = ssp)) + geom_boxplot() 
  bp <- bp + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
  bp <- bp + labs(title = title, x = '', y = 'Biomass change (Pg C)')
  bp <- bp + facet_wrap(. ~ year, ncol = 3) + theme_bw(base_size = 13) + theme(legend.position = 'none')
  return(bp)
}

ssp <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
ssp_labs <- c('SSP1-26', 'SSP2-45', 'SSP3-70', 'SSP5-85')
names(ssp_labs) <- ssp

## burned area ###############################################################
burneda.files <- files[grep('t3_burneda', files)]
burneda.df <- lapply(burneda.files, ReadCSV, region = 'world')
burneda.df <- do.call('rbind', burneda.df)
burneda_mmm.df <- burneda.df[, grep('model', names(burneda.df), invert = TRUE)]
burneda_mmm.df$mmm <- burneda_mmm.df$mmm * 1e-10
burneda_mmm.df$ci95 <- burneda_mmm.df$ci95 * 1e-10
burneda_mmm.df[order(burneda_mmm.df$year),]
burneda_mmm.df <- reshape::melt.data.frame(burneda_mmm.df, measure.vars = c('mmm', 'ci95'))
PrettyTable(burneda_mmm.df, multiplier = 1, variable_name = 'Mha', outname = 'burneda_mmm.csv')

## boxplot
library(ggplot2)
burneda.df$mmm <- NULL
burneda.df$ci95 <- NULL
burneda.df$year <- paste0(burneda.df$year, 's')

burneda.dfm <- reshape::melt.data.frame(burneda.df, id.vars = c('Region', 'scenario', 'ssp', 'year'))
burneda.dfm$ssp[burneda.dfm$ssp=='ssp126'] <- 'SSP1-26'
burneda.dfm$ssp[burneda.dfm$ssp=='ssp245'] <- 'SSP2-45'
burneda.dfm$ssp[burneda.dfm$ssp=='ssp370'] <- 'SSP3-70'
burneda.dfm$ssp[burneda.dfm$ssp=='ssp585'] <- 'SSP5-58'
restoration_area <- 126.38

burneda.dfm <- burneda.dfm[burneda.dfm$year!='2020s',]
ssp_colours <- c('#66A36C', '#3DACC6', '#D28E54', '#C3474E')
bp <- ggplot(burneda.dfm, aes(x = ssp, y = value * 1e-10, color = ssp)) + geom_boxplot() 
bp <- bp + scale_fill_manual(values = ssp_colours, name = '', labels = ssp_labs) + scale_color_manual(values = ssp_colours, name = '', labels = ssp_labs)
bp <- bp + scale_y_continuous(sec.axis = sec_axis(trans = ~./(restoration_area/100), name = '(%) restoration area'))
bp <- bp + labs(title = 'Burned restoration area', x = '', y = 'Burned area (Mha)')
bp <- bp + facet_wrap(. ~ year, ncol = 3) + theme_bw(base_size = 13) + theme(legend.position = 'none')
ggsave(paste0(wdir, 'burneda_2050_2090_bp.pdf'), 
       plot = bp, device = 'pdf', width = 170, height = 120, units = 'mm', dpi = 300)

## Continents
burneda.df <- lapply(burneda.files, ReadCSV, region = 'continent')
burneda.df <- do.call('rbind', burneda.df)
burneda_mmm.df <- burneda.df[, grep('model', names(burneda.df), invert = TRUE)]
burneda_mmm.df$mmm <- burneda_mmm.df$mmm * 1e-10
burneda_mmm.df$ci95 <- burneda_mmm.df$ci95 * 1e-10

burneda_mmm.df$restorA[burneda_mmm.df$Region=='Africa'] <- 19.75
burneda_mmm.df$restorA[burneda_mmm.df$Region=='Asia'] <- 34.43
burneda_mmm.df$restorA[burneda_mmm.df$Region=='Europe'] <- 0
burneda_mmm.df$restorA[burneda_mmm.df$Region=='North America'] <- 17.67
burneda_mmm.df$restorA[burneda_mmm.df$Region=='Oceania'] <- 0.31
burneda_mmm.df$restorA[burneda_mmm.df$Region=='South America'] <- 54.20
burneda_mmm.df$restorA[burneda_mmm.df$Region=='Indian subcontinent'] <- 4.22

burneda_mmm.df$prop <- burneda_mmm.df$mmm / burneda_mmm.df$restorA
burneda_mmm.df$prop_ci <- burneda_mmm.df$ci95 / burneda_mmm.df$restorA

write.csv(burneda_mmm.df, paste0(wdir, 'burneda_mmm_continents.csv'), row.names = FALSE)

burneda_mmm.df <- reshape::melt.data.frame(burneda_mmm.df, measure.vars = c('mmm', 'ci95'))
burneda_mmm.df <- burneda_mmm.df[order(burneda_mmm.df$value),]




## barcharts
burneda_mmm.df$year <- paste0(burneda_mmm.df$year, 's')
bc <- ggplot(burneda_mmm.df[burneda_mmm.df$Region!='Europe',], aes(x = year, y = prop*100, fill = Region)) 
bc <- bc + geom_col(position = 'dodge') + facet_wrap(. ~ ssp, ncol = 2) 
bc <- bc + geom_errorbar(aes(x = year, ymin = (prop-prop_ci)*100, ymax=(prop+prop_ci)*100), alpha = 0.9, size = 0.5, position = 'dodge')
bc <- bc + scale_fill_brewer(type = 'qual', palette = 'Set2')
bc <- bc + theme_bw(base_size = 13)
bc <- bc + labs(title = 'Burned restoration area', x = 'Decade', y = '(%) restoration area')
ggsave(paste0(wdir, 'perc_restA_burned_continent.pdf'), 
       plot = bc, device = 'pdf', width = 190, height = 170, units = 'mm', dpi = 300)

##############################################################################
##tile carbon ################################################################
##############################################################################
t3C.files <- files[grep('t3C', files)]
t3C.df <- lapply(t3C.files, ReadCSV, region = 'world')
t3C.df <- do.call('rbind', t3C.df)

## restoration vs no restoration
rest.df <- DiffScenarios(t3C.df, sc1 = 'restor_fire', sc2 = 'ctl_fire', scname = 'restor-ctl')
rest.df$year <- paste0(rest.df$year, 's')
rest_mmm.df <- rest.df[!rest.df$variable %in% paste0('model', 1:13),]

BoxPlot(df = rest.df, title = 'Total carbon change from restoration')

PrettyTable(rest_mmm.df, variable_name = 'dCarbon_PgC', multiplier = 1e-15, outname = 'dtotc_restor.csv')

## fixed 2014 co2 ###########################################################
rest_2014.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_2014_fire', 
                              sc2 = 'ctl_fixco2_2014_fire', 
                              scname = 'restor-ctl_fixco2_2014')

rest_2014.df$year <- paste0(rest_2014.df$year, 's')
rest_2014_mmm.df <- rest_2014.df[!rest_2014.df$variable %in% paste0('model', 1:13),]

## boxplot
BoxPlot(df = rest_2014.df, title = 'Total carbon change from restoration under fixed CO2 (2014)')

# output table
PrettyTable(rest_2014_mmm.df, variable_name = 'dCarbon_PgC', multiplier = 1e-15, outname = 'dtotc_restor_fixco2_2014.csv')

## fixed 1850 co2 ###########################################################
rest_1850.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_1850_fire', 
                              sc2 = 'ctl_fixco2_1850_fire', 
                              scname = 'restor-ctl_fixco2_1850')

rest_1850.df$year <- paste0(rest_1850.df$year, 's')
rest_1850_mmm.df <- rest_1850.df[!rest_1850.df$variable %in% paste0('model', 1:13),]

## boxplot
BoxPlot(df = rest_1850.df, title = 'Total carbon change from restoration under fixed CO2 (1850)')

# output table
PrettyTable(rest_1850_mmm.df, variable_name = 'dCarbon_PgC', multiplier = 1e-15, outname = 'dtotc_restor_fixco2_1850.csv')

## Get 2014-2019 mean #########################################################
nc.files <- list.files(wdir, pattern = '2014-2019.nc', full.names = TRUE)

sapply(nc.files, function(x) nc_read(x, 'mean'))


## fire vs no fire ############################################################
fire.df <- DiffScenarios(t3C.df, sc1 = 'restor', sc2 = 'restor_fire', scname = 'fire-nofire')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]

## boxplot
fire.df$ssp[fire.df$ssp=='ssp126'] <- 'SSP1-26'
fire.df$ssp[fire.df$ssp=='ssp245'] <- 'SSP2-45'
fire.df$ssp[fire.df$ssp=='ssp370'] <- 'SSP3-70'
fire.df$ssp[fire.df$ssp=='ssp585'] <- 'SSP5-58'

bp <- BoxPlot(df = fire.df[fire.df$year!='2020s',], title = 'Impact of fire supression')
bp <- bp + scale_y_continuous(limits = c(1.5,8))
ggsave(paste0(wdir, 'nofire_2050_2090_bp.pdf'), 
       plot = bp, device = 'pdf', width = 170, height = 120, units = 'mm', dpi = 300)
# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'PgC', outname = 'dtotc_nofire.csv')

##

fire.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_2014', sc2 = 'restor_fixco2_2014_fire', scname = 'fire-nofire_fixco2')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]

## boxplot
fire.df$ssp[fire.df$ssp=='ssp126'] <- 'SSP1-26'
fire.df$ssp[fire.df$ssp=='ssp245'] <- 'SSP2-45'
fire.df$ssp[fire.df$ssp=='ssp370'] <- 'SSP3-70'
fire.df$ssp[fire.df$ssp=='ssp585'] <- 'SSP5-58'
# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'PgC', outname = 'dtotc_nofire_fixco2.csv')

## fixed co2 ##################################################################
fire.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_2014', sc2 = 'restor_fixco2_2014_fire', scname = 'fire-nofire_fixco2')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]

## boxplot
fire.df$ssp[fire.df$ssp=='ssp126'] <- 'SSP1-26'
fire.df$ssp[fire.df$ssp=='ssp245'] <- 'SSP2-45'
fire.df$ssp[fire.df$ssp=='ssp370'] <- 'SSP3-70'
fire.df$ssp[fire.df$ssp=='ssp585'] <- 'SSP5-58'

bp <- BoxPlot(df = fire.df[fire.df$year!='2020s',], title = 'Impact of fire supression (fixed CO2 2014)')
bp <- bp + scale_y_continuous(limits = c(1.5,8))
ggsave(paste0(wdir, 'nofire_fixco2_2050_2090_bp.pdf'), 
       plot = bp, device = 'pdf', width = 170, height = 120, units = 'mm', dpi = 300)
# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'PgC', outname = 'dtotc_nofire_fixco2.csv')

## For continents ##############################################################
t3C.files <- files[grep('t3C', files)]
t3C.df <- lapply(t3C.files, ReadCSV, region = 'continent')
t3C.df <- do.call('rbind', t3C.df)

rest.df <- DiffScenarios(t3C.df, sc1 = 'restor_fire', sc2 = 'ctl_fire', scname = 'restor-ctl')
rest.df$year <- paste0(rest.df$year, 's')
rest_mmm.df <- rest.df[!rest.df$variable %in% paste0('model', 1:13),]
rest_mmm.df$value[rest_mmm.df$Region=='South America'] <- rest_mmm.df$value[rest_mmm.df$Region=='South America'] + 
  rest_mmm.df$value[rest_mmm.df$Region=='North America']

# output table
PrettyTable(rest_mmm.df, multiplier = 1e-15, variable_name = 'dCarbon', outname = 'dtotc_restor_continent.csv')

rest_2014.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_2014_fire', 
                              sc2 = 'ctl_fixco2_2014_fire', 
                              scname = 'restor-ctl_fixco2_2014')

rest_2014.df$year <- paste0(rest_2014.df$year, 's')
rest_2014_mmm.df <- rest_2014.df[!rest_2014.df$variable %in% paste0('model', 1:13),]


fire.df <- DiffScenarios(t3C.df, sc1 = 'restor', sc2 = 'restor_fire', scname = 'fire-nofire')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]

# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'dCarbon', outname = 'dtotc_nofire_continent.csv')

## fire suppression
fire.df <- DiffScenarios(t3C.df, sc1 = 'restor', sc2 = 'restor_fire', scname = 'fire-nofire')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]
fire_mmm.df$value[fire_mmm.df$Region=='South America'] <- fire_mmm.df$value[fire_mmm.df$Region=='South America'] + 
  fire_mmm.df$value[fire_mmm.df$Region=='North America']

# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'PgC', outname = 'dtotc_nofire_continents.csv')

fire.df <- DiffScenarios(t3C.df, sc1 = 'restor_fixco2_2014', sc2 = 'restor_fixco2_2014_fire', scname = 'fire-nofire_fixco2')
fire.df$year <- paste0(fire.df$year, 's')
fire_mmm.df <- fire.df[!fire.df$variable %in% paste0('model', 1:13),]
fire_mmm.df$value[fire_mmm.df$Region=='South America'] <- fire_mmm.df$value[fire_mmm.df$Region=='South America'] + 
  fire_mmm.df$value[fire_mmm.df$Region=='North America']
# output table
PrettyTable(fire_mmm.df, multiplier = 1e-15, variable_name = 'PgC', outname = 'dtotc_nofire_fixco2_continents.csv')
