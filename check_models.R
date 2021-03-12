wdir <- '/home/terraces/datasets/CMIP6/'
output <- list.files(path = wdir, pattern = '.nc', recursive = TRUE, full.names = TRUE)

filename <- strsplit(output, '/')
filename <- sapply(filename, FUN = function(x) x[length(x)])
filename <- strsplit(filename, '_')
ssp <- sapply(filename, FUN = function(x) x[4])
model <- sapply(filename, FUN = function(x) x[3])
variable <- sapply(filename, FUN = function(x) x[1])

out.df <- data.frame(unique=paste(ssp, model, variable, sep = '_'))
out.df <- unique(out.df$unique)
out.df <- strsplit(out.df, '_')
out.df <- data.frame(ssp=sapply(out.df, FUN = function(x) x[1]), 
                     model=sapply(out.df, FUN = function(x) x[2]), 
                     variable=sapply(out.df, FUN = function(x) x[3]))
out.df <- out.df[out.df$ssp %in% c('historical', 
                                   'ssp126-ssp370Lu', 
                                   'ssp370-ssp126Lu', 
                                   'ssp126',
                                   'ssp245',
                                   'ssp370',
                                   'ssp585'),]
write.csv(out.df, '/home/akoch/data/future_forests/ssp_model_var.csv', 
          row.names = FALSE)

out.df <- read.csv('/home/ucfaako/Documents/future_forests/data/ssp_model_var.csv')
head(out.df)

## check manually for lumip ###################################################
ssp1_ssp3 <- out.df[out.df$ssp=='ssp126-ssp370Lu',]
ssp3_ssp1 <- out.df[out.df$ssp=='ssp370-ssp126Lu',]
merge.data.frame(ssp3_ssp1, ssp1_ssp3, by=c('model', 'variable'), all = TRUE)
# CESM2 not available in ssp126-ssp370Lu

ssp1 <- out.df[out.df$ssp=='ssp126',]
ssp1.ovr <- merge.data.frame(ssp1, ssp1_ssp3, by=c('model', 'variable'), all = TRUE)
ssp1.ovr[!is.na(ssp1.ovr$ssp.y),]

ssp3 <- out.df[out.df$ssp=='ssp370',]
ssp3.ovr <- merge.data.frame(ssp3, ssp1_ssp3, by=c('model', 'variable'), all = TRUE)
ssp3.ovr[!is.na(ssp3.ovr$ssp.y),]
# need to check data for most ssp1/3 

# BCC-CSM2-MR GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-LR
# NB strange that NorESM2-LM is not available!
## contingency tables for ssp-hist ############################################
hist <- out.df[out.df$ssp=='historical',]
hist <- hist[hist$variable!='sftlf',]

ssp2 <- out.df[out.df$ssp=='ssp245',]
ssp5 <- out.df[out.df$ssp=='ssp585',]

exclude_vars <- c('cLeaf', 'cRoot', 'cVeg', 'hfls', 'hfss', 'ts')
`%notin%` <- Negate(`%in%`)

merge.data.frame(ssp1[ssp1$variable %notin% exclude_vars,], hist,
                 by=c('model', 'variable'), all = TRUE)
merge.data.frame(ssp2[ssp2$variable %notin% exclude_vars,], hist,
                 by=c('model', 'variable'), all = TRUE)
merge.data.frame(ssp3[ssp3$variable %notin% exclude_vars,], hist,
                 by=c('model', 'variable'), all = TRUE)
merge.data.frame(ssp5[ssp5$variable %notin% exclude_vars,], hist,
                 by=c('model', 'variable'), all = TRUE)

