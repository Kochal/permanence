#!/home/public/easybuild/software/Python/3.8.2-GCCcore-9.3.0/bin/python
## create ensembles
#cd /home/terraces/projects/LPJ_futuretropics/for_Katie/
#for VAR in clt dtr lightning preacc sfcWind tas wetdays;
#do
#for MOD in BCC-CSM2-MR IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 FGOALS-g3 MIROC6 ACCESS-CM2 ACCESS-ESM1-5 AWI-CM-1-1-MR EC-Earth3-Veg INM-CM4-8 INM-CM5-0 CanESM5;
#do
#cdo -L -yearmean -selyear,1971/2000 /home/terraces/datasets/climategenerator_data/CMIP6_permanence/transient_1850-2100/control/ssp126/${MOD}/anomalies*/30m/${VAR}*anomaly.nc ${VAR}_${MOD}_hist.nc;
#ncrename -v .prec,pr ${VAR}_${MOD}_hist.nc;
#done;

#cdo ensmean ${VAR}_*_hist.nc ${VAR}_hist.nc;

#for SSP in ssp126 ssp245 ssp370 ssp585;
#do

#for MOD in BCC-CSM2-MR IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 FGOALS-g3 MIROC6 ACCESS-CM2 ACCESS-ESM1-5 AWI-CM-1-1-MR EC-Earth3-Veg INM-CM4-8 INM-CM5-0 CanESM5;
#do
#cdo -L yearmean -selyear,2040/2049 /home/terraces/datasets/climategenerator_data/CMIP6_permanence/transient_1850-2100/control/${SSP}/${MOD}/anomalies*/30m/${VAR}*anomaly.nc ${VAR}_${MOD}_2040s_X.nc;
#ncrename -v .prec,pr ${VAR}_${MOD}_2040s_X.nc;

#cdo -L yearmean -selyear,2090/2099 /home/terraces/datasets/climategenerator_data/CMIP6_permanence/transient_1850-2100/control/${SSP}/${MOD}/anomalies*/30m/${VAR}*anomaly.nc ${VAR}_${MOD}_2090s_X.nc;
#ncrename -v .prec,pr ${VAR}_${MOD}_2090s_X.nc;
#done;

#cdo merge ${VAR}_*_2040s_X.nc ${VAR}_${SSP}_ens_2040s.nc;
#cdo merge ${VAR}_*_2090s_X.nc ${VAR}_${SSP}_ens_2090s.nc;

#rm ${VAR}_*_2040s_X.nc; rm ${VAR}_*_2090s_X.nc;
#done;
#done

#for VAR in clt dtr lightning preacc sfcWind tas wetdays;
#do
#for SSP in ssp126 ssp245 ssp370 ssp585;
#do
#/home/akoch/scripts/future_forests/Knutti_R.py ${VAR}_${SSP}_ens_2040s.nc ${VAR} ${VAR}_hist.nc R_${VAR}_${SSP}_ens_2040s.nc;
#/home/akoch/scripts/future_forests/Knutti_R.py ${VAR}_${SSP}_ens_2090s.nc ${VAR} ${VAR}_hist.nc R_${VAR}_${SSP}_ens_2090s.nc;

#ncrename -v $VAR,R R_${VAR}_${SSP}_ens_2040s.nc;
#ncrename -v $VAR,R R_${VAR}_${SSP}_ens_2090s.nc;

#cdo expr,'R=R>=0.8' R_${VAR}_${SSP}_ens_2040s.nc R_${VAR}_${SSP}_ens_2040s_0.nc;
#cdo expr,'R=R>=0.8' R_${VAR}_${SSP}_ens_2090s.nc R_${VAR}_${SSP}_ens_2090s_0.nc;

#mv R_${VAR}_${SSP}_ens_2040s_0.nc R_${VAR}_${SSP}_ens_2040s.nc
#mv R_${VAR}_${SSP}_ens_2090s_0.nc R_${VAR}_${SSP}_ens_2090s.nc

#done;
#done

import xarray as xr
import numpy as np
import sys

##############################################################################
def _knutti_sedlacek(ref, fut):
    def diff_cdf_sq_area_int(x1, x2):
        """Exact integral of the squared area between the non-parametric CDFs of 2 vectors."""
        # Non-parametric CDF on points x1 and x2
        # i.e. y1(x) is the proportion of x1 <= x
        y1 = (np.arange(x1.size) + 1) / x1.size
        y2 = (np.arange(x2.size) + 1) / x2.size
        
        x2_in_1 = np.searchsorted(x1, x2, side="right")  # Where to insert x2 in x1
        x1_in_2 = np.searchsorted(x2, x1, side="right")  # Where to insert x1 in x2
        
        # Merge to get all "discontinuities" of the CDF difference
        # y1 with repeated value (to the right) where x2 is inserted
        # Same for y2. 0s are prepended where needed.
        x = np.insert(x1, x2_in_1, x2)
        y1_f = np.insert(y1, x2_in_1, np.r_[0, y1][x2_in_1])
        y2_f = np.insert(y2, x1_in_2, np.r_[0, y2][x1_in_2])
        
        # Discrete integral of the squared difference (distance) between the two CDFs.
        return np.sum(np.diff(x) * (y1_f - y2_f)[:-1] ** 2)
        
    # Get sorted vectors
    v_fut = np.sort(fut.flatten())  # "cumulative" models distribution
    v_favg = np.sort(fut.mean(axis=-1))  # Multi-model mean
    v_ref = np.sort(ref)  # Historical values
    
    A1 = diff_cdf_sq_area_int(v_fut, v_favg)
    A2 = diff_cdf_sq_area_int(v_ref, v_favg)
    
    return 1 - A1 / A2

###############################################################################

files=sys.argv[1]
varname=sys.argv[2]
histfiles=sys.argv[3]
outfile=sys.argv[4]


ens = xr.open_dataset(files)
hist = xr.open_dataset(histfiles)

ens=ens.drop('time_bnds')
hist=hist.drop('time_bnds')

ensdim = [var for var in ens.dims 
             if 'height' in var or 'sfc' in var]

R = xr.apply_ufunc(
        _knutti_sedlacek,
        hist,
        ens,
        input_core_dims=[["time"], [ensdim[0], "time"]],
        exclude_dims={"time"},
        vectorize=True,
        dask="parallelized",
        output_dtypes=[float],)
        
R.attrs.update(
        name="R",
        long_name="Ensemble robustness coefficient",
        description="Ensemble robustness coefficient as defined by Knutti and Sedláček (2013).",
        reference="Knutti, R. and Sedláček, J. (2013) Robustness and uncertainties in the new CMIP5 climate model projections. Nat. Clim. Change.",
        units="",
        #xclim_history=update_history("knutti_sedlacek(fut, ref)", ref=ref, fut=fut),
        )
        
R.to_netcdf(outfile)

