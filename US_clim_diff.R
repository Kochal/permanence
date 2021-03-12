# for M in *.asc; do gdalwarp -t_srs EPSG:4326 -s_srs "+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77 +datum=WGS84" -tr 0.008333333300000 -0.008333333300000 -r bilinear -co "TILED=YES" ${M} wgs84/${M}.tif; done
# for M in {1..12}; do N=`printf "%02d\n" $M`; gdal_calc.py --co="COMPRESS=LZW" -A ./wgs84/Tave${N}.asc.tif -B ../CHELSEA/US/CHELSA_temp10_${N}_1979-2013_V1.2_land.tif --outfile=diff_CHELSEA/Tave${N}.tif --calc="A - (B/10)"; echo diff_CHELSEA/Tave${N}.tif; done
# for M in {1..12}; do N=`printf "%02d\n" $M`; gdal_calc.py --co="COMPRESS=LZW" -A ./wgs84/PPT${N}.asc.tif -B ../CHELSEA/US/CHELSA_prec_${N}_V1.2_land.tif --outfile=diff_CHELSEA/PPT${N}.tif --calc="A - (B/10)"; echo diff_CHELSEA/PPT${N}.tif; done

# for M in {1..12}; do N=`printf "%02d\n" $M`; gdalwarp -co "COMPRESS=LZW"  -te -180, 8, -50, 86 Tave${N}.tif Tave${N}_ext.tif; done
# for M in {1..12}; do N=`printf "%02d\n" $M`; gdalwarp -co "COMPRESS=LZW"  -te -180, 8, -50, 86 PPT${N}.tif PPT${N}_ext.tif; done
library(ncdf4); library(raster)
Tave.l <- list.files('~/terraces/datasets/climate/climateNA/diff_CHELSEA/', pattern = 'Tave', full.names = TRUE)
Tave.l <- Tave.l[grep('ext', Tave.l)]
Tave.s <- stack(Tave.l)
Tave.arr <- as.array(Tave.s)
Tave.arr <- aperm(Tave.arr, c(2,1,3))

Pre.l <- list.files('~/terraces/datasets/climate/climateNA/diff_CHELSEA/', pattern = 'PPT', full.names = TRUE)
Pre.l <- Pre.l[grep('ext', Pre.l)]
Pre.s <- stack(Pre.l)
Pre.arr <- as.array(Pre.s)
Pre.arr <- aperm(Pre.arr, c(2,1,3))

nc.file <- '~/terraces/datasets/climate/climateNA/diff_CHELSEA/diffs.nc'
diff.nc <- nc_open(nc.file, write = TRUE)
ncvar_put(diff.nc, varid = 'tmp', vals = Tave.arr)
ncvar_put(diff.nc, varid = 'pre', vals = Pre.arr)
nc_close(diff.nc)

library(terra)
plot(rast('/home/ucfaako/Downloads/PPT12_ext.tif'))
