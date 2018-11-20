library(??ncdf4)


ncin <- nc_open('~/Downloads/S5P_NRTI_L2__NO2____20181017T045013_20181017T045513_05233_01_010100_20181017T053427.nc')
ncin$var %>% str()
ncvar_get(ncin, 'vals')
