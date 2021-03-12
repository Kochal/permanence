library(ggplot2); library(ggpubr)

# wdir <- '/home/ucfaako/Documents/future_forests/data/'
wdir <- '/home/akoch/terraces/projects/LPJ_futuretropics/agb_restor/'

agb.files <- list.files(wdir, pattern = 'totalbiomass', full.names = TRUE)
agb.files <- agb.files[grep('.csv', agb.files)]
scenarios <- paste0(c('ctl', 'ctl_fire', 'restor', 'restor_fire', 'restor_cl_fire'), collapse = '|')
agb.files <- agb.files[grep(scenario, agb.files)]
agb.files <- agb.files[grep('fixco2', agb.files, invert = TRUE)]

AddSSP <- function(x){
  fname <- gsub(wdir, '', x)
  stringn <- unlist(strsplit(fname, split = '_'))
  ssp <- stringn[2]
  scenario <- paste(stringn[3:(length(stringn)-3)], collapse = '_')
  yearrange <- stringn[(length(stringn)-1)]
  df <- read.csv(x)
  df$ssp <- ssp
  df$scenario <- scenario
  df$yearrange <- yearrange
  return(df)
}

agb.df <- lapply(agb.files, AddSSP)
agb.df <- do.call('rbind', agb.df)

colind <- which(names(agb.df) %in% c('CONTINENT', 'ssp', 'scenario', 'yearrange'))

names(agb.df)[1:13] <- paste0('model', 1:13)

agb.dfm <- reshape::melt.data.frame(agb.df, measure.vars = names(agb.df)[1:13])
agb.dfm <- aggregate.data.frame(agb.dfm[,'value'], 
                                by = list(ssp=agb.dfm$ssp, 
                                          scenario=agb.dfm$scenario,
                                          yearrange=agb.dfm$yearrange,
                                          variable=agb.dfm$variable), FUN = sum)

agb.dfm$ssp <- toupper(agb.dfm$ssp)
agb.dfm$scenario[agb.dfm$scenario=='ctl'] <- 'SSP landuse'
agb.dfm$scenario[agb.dfm$scenario=='ctl_fire'] <- 'SSP landuse + Fire'
agb.dfm$scenario[agb.dfm$scenario=='restor'] <- 'Restoration'
agb.dfm$scenario[agb.dfm$scenario=='restor_fire'] <- 'Restoration + Fire'
agb.dfm$scenario[agb.dfm$scenario=='restor_cl_fire'] <- 'Restoration + Fire \n+ Climate adjusted'

agb.dfm$scenario <- factor(agb.dfm$scenario, levels = c('SSP landuse', 'SSP landuse + Fire', 
                                     'Restoration', 'Restoration + Fire', 
                                     'Restoration + Fire \n+ Climate adjusted'))

# summary.df <- data.frame(mmm=apply(agb.df[,-colind], 1, mean),
#                          sd=apply(agb.df[,-colind], 1, sd),
#                          ci=apply(agb.df[,-colind], 1, 
#                                   function(x) 1.96 * (sd(x) / sqrt(13))))
# summary.df$CONTINENT <- agb.df$CONTINENT
# summary.df$ssp <- agb.df$ssp
# summary.df$scenario <- agb.df$scenario
# summary.df$yearrange <- agb.df$yearrange
# 
# aggregate.data.frame(summary.df[, -c(4:6)], 
#                      by = list(ssp=summary.df$ssp, 
#                                scenario=summary.df$scenario,
#                                yearrange=summary.df$yearrange), FUN = sum)
# boxplot
bp.l <- list()
# for (yr in unique(agb.dfm$yearrange)){
for (yr in c("2040-2020", "2090-2020")){
  bp <- ggplot(data = agb.dfm[agb.dfm$yearrange %in% yr, ], aes(x = ssp, y = x * 1e-15)) 
  bp <- bp + geom_boxplot() + facet_wrap(. ~ scenario, ncol = 5)
  bp <- bp + labs(x = '', y = 'AGB change (Pg C)', title = yr) + theme_bw(base_size = 11)
  bp <- bp + theme(strip.text.x = element_text(size = 12, face = 'bold'))
  bp.l[[yr]] <- bp
}
bp.l[[2]] <- bp.l[[2]] + scale_y_continuous(breaks = seq(0, 60, 15))
bp.ar <- ggarrange(bp.l[[1]], bp.l[[2]], nrow = 2, labels = 'auto')
ggsave(paste0(wdir, 'agb_bp_all.pdf'), 
       plot = bp.ar, device = 'pdf', width = 300, height = 180, units = 'mm', dpi = 300)

