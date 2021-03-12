#!/bin/bash

prefix=$1
suffix=$2

# interpolate anomalies to 0.5 degree

for var in tas dtr pr mc_lvl clt sfcWind 
do

    infile=$var$suffix

    original=$prefix/$infile
    outfile=$prefix/144x90/$infile
          
    echo $var

    cdo -s -f nc4 -P 8 remapbic,/home/akoch/LPJ/climategenerator/tools/grid144x90 $original $outfile

done
