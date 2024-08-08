# -*- coding: utf-8 -*-
"""
Created on Wed Jul  3 14:54:12 2024

@author: davytorres
"""

import numpy as np
import netCDF4 as nc
print("Reading npy file")
data = np.load('X_test5.npy')
ncfile = nc.Dataset('aerosol_input.nc',mode='w',format='NETCDF4_CLASSIC')
input_dim = ncfile.createDimension('inputd', 80) 
instances_dim = ncfile.createDimension('instancesd',1214705)
input_vars = ncfile.createVariable('input_vars',np.float64,('instancesd','inputd'))
input_vars[:,:] = data
print(ncfile)
ncfile.close()
#ncfile.title='ICAR training data for neural network'



print(data.shape)
print(data)
