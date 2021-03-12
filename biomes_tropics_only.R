#!/home/akoch/.conda/envs/R_env/bin/R
# Biome script modified to tropics only
library(ncdf4)
source('~/Documents/R_code/fun_Utility.R')
## Input ######################################################
input_args <- commandArgs(trailingOnly = TRUE)

#wdir <- '/home/ucfaako/Documents/LGM/data/'

dgvm.name <- input_args[1] #'AWI-ESM_lgm_150yrs.nc'
#gdd5.name <- 'gdd5.nc'
outname <- input_args[2] #'AWI_lgm_biome.nc'

# LPJ output
dgvm.file <- paste0(wdir, dgvm.name)
dgvm.file <- '/home/ucfaako/Documents/future_forests/data/cover_2020.nc'
dgvm.nc <- nc_open(dgvm.file)
cover <- ncvar_get(dgvm.nc, 'cover') 
height <- ncvar_get(dgvm.nc, 'height')
nc_close(dgvm.nc)

# Growing degree days > 5degC
# gdd5.file <- paste0(wdir, gdd5.name)
# gdd5.nc <- nc_open(gdd5.file)
# gdd5 <- ncvar_get(gdd5.nc, 'gdd5')
# nc_close(gdd5.nc)
## LPJ PFTs ###################################################
# TrBE = tropical broadleaf evergreen, 
# TrBR = tropical broadleaf raingreen, 
# TeNE = temperate needle-leaf evergreen, 
# TeBE = temperate broadleaf evergreen, 
# TeBS = temperate broadleaf summergreen, 
# BoNE = boreal needleleaf evergreen,
# BoS = boreal summergreen, 
# C3gr = C 3 perennial grass, 
# C4gr = C 4 perennial grass

## Biomes ######################################################
# PFT abundance to distinguish desert, dry grass- and shrubland,
# forests, and savannahs

# 1 Tundra #F9B8C3
# 2 Dry grass-/shrubland #F68E24
# 3 Desert #EAED28
# 4 Tropical forest #163417
# 5 Savannah #C55E1F
# 6 Warm temperate forest #2F6468
# 7 Sclerophyll woodland #839836
# 8 Temperate forest #3CB73D
# 9 Temperate parkland #96D780
# 10 Boreal forest #2A3A83
# 11 Boreal parkland #83D5DC


#C6C0E5 - Shrub-tundra


## Distinguish desert, dry grass- and shrubland, from forests/savannahs #####
landmask <- apply(cover, 1:2, sum, na.rm=TRUE)
trees <- apply(cover[,,1:7], 1:2, sum, na.rm=TRUE)
forest <- trees
forest[trees > 0.6] <- 1

savanna <- trees
savanna[trees > 0.6] <- 0
savanna[trees < 0.3] <- 0
savanna[trees > 0] <- 1

notrees <- forest * savanna
notrees[notrees > 0] <- 1
notrees <- abs(notrees - 1)

# grassland
gl <- cover[,,8] + cover[,,9]
gl[is.na(gl)] <- 0
gl_mask <- gl
gl_mask[gl_mask > 0.3] <- 1
gl_mask[gl_mask <= 0.3] <- 0

# desert
ds <- abs(gl_mask - 1)

gl <- gl * notrees
ds <- ds * notrees

## Forests ######################################################
# height conversion
forest <- list()

# Tropical PFTs
# height > 10 = tropcial forest
tropf <- cover[,,1] * forest[[1]] + cover[,,2] * forest[[2]]
# height < 10 = tropical savannah
sv <- cover[,,1] * nonforest[[1]] + cover[,,2] * nonforest[[2]]

# temperate broadleaf evergreen
# height > 10 = warm temperate forest
wtf <- cover[,,4] * forest[[4]] 
# height < 10 = Sclerophyll woodland
sw <- cover[,,4] * nonforest[[4]]


# temperate needle-leaf evergreen OR temperate broadleaf summergreen
# height > 10 = temperate forest
tempf <- cover[,,3] * forest[[3]] + cover[,,5] * forest[[5]]
# height < 10 = temperate parkland
tempp <- cover[,,3] * nonforest[[3]] + cover[,,5] * nonforest[[5]]


# boreal needleleaf evergreen OR boreal summergreen
# height > 10 = boreal forest
bf <- cover[,,6] * forest[[6]] + cover[,,7] * forest[[7]]
# height < 10 = boreal parkland
bp <- cover[,,6] * nonforest[[6]] + cover[,,7] * nonforest[[7]]

# shrub-tundra
stu <- 0#cold * trees * nonforest

# assign each grid cell to biome with greatest proportion
total <- tu + stu + gl + ds + tropf + sv + wtf + sw + tempf + tempp + bf + bp
total.list <- list(tu, stu, gl, ds, tropf, sv, wtf, sw, tempf, tempp, bf, bp)
biomes_prop <- lapply(total.list, FUN = function(x) x / total)
biomes_prop <- simplify2array(biomes_prop)
top_prop <- apply(biomes_prop, 1:2, max)

biomes <- matrix(data = NA, nrow = nrow(top_prop), ncol = ncol(top_prop))
for (x in 1:ncol(top_prop)){
  for (y in 1:nrow(top_prop)){
    z <- which(biomes_prop[y,x,]==top_prop[y,x])
    if(length(z)>0){
      biomes[y,x] <- z
    }
  }
}
# TODO fix lon lat rotations
## write Netcdf ##############################################################
nlon <- dim(biomes)[1]
dlon <- 360 / nlon
lon <- as.array(seq(0, 360 - dlon, dlon))
nlat <- dim(biomes)[2]
dlat <- 180 / nlat
lat <- as.array(seq(-90, 90 - dlat, dlat))

londim <- ncdim_def('lon','degrees_east',as.double(lon)) 
latdim <- ncdim_def('lat','degrees_north',as.double(lat))

# define variables
fillvalue <- 1e32
dlname <- 'Biomes after Prentice et al. (2011)'
var_def <- ncvar_def('biomes','biome',
                     list(londim, latdim),
                     fillvalue, dlname, prec='float')

## create nc with variables on disk
ncout <- nc_create(filename = outname, 
                   vars = list(biomes), force_v4=TRUE)

## put variables
ncvar_put(ncout, var_def, biomes)

ncatt_put(ncout,0, 'title', 'Biomes')
ncatt_put(out.nc, 0, 'legend', '
# 1 Tundra 
# 2 Shrub-tundra
# 3 Dry grass-/shrubland
# 4 Desert
# 5 Tropical forest
# 6 Savannah
# 7 Warm temperate forest
# 8 Sclerophyll woodland
# 9 Temperate forest
# 10 Temperate parkland 
# 11 Boreal forest
# 12 Boreal parkland')

nc_close(out.nc)
