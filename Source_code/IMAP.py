import math
import numpy as np
import numpy.linalg as lin
import pandas as pd
from matplotlib import cm
from matplotlib import ticker
#from UMAT import *
import matplotlib.pyplot as plt
import csv
import sys
#--------------------------------------------------------------------------#
# CHOOSE DATA TYPE
# DATA_TYPE=2: ERROR(1): UNIAXIAL TENSION
# DATA_TYPE=3: ERROR(2): BALANCED BIAXIAL TENSION
# DATA_TYPE=4: ERROR(3): PURE SHEAR
data_type=10
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
# OPEN DATA FILES #
data = np.loadtxt('OUT\IMAP.csv', delimiter=',', dtype=np.float64)

max_indx=np.max(data[:,0])
min_indx=np.min(data[:,0])
print (max_indx)
ndata=int(np.sqrt(np.size(data[:,0])))
delta=data[1,1]-data[0,1]
#--------------------------------------------------------------------------#
plt.figure(1, figsize=(10,8))
plt.rc('axes', linewidth=2.0)
plt.grid(which='major', alpha=0.5)
plt.axvline(x=0, color='k', linewidth=1.5)
plt.axhline(y=0, color='k', linewidth=1.5)
plt.axis([min_indx,max_indx,min_indx,max_indx])
r=1.0
plt.axis([min_indx*r,max_indx*r,min_indx*r,max_indx*r])
plt.tick_params(axis='both', direction='in', length=10, width=2, pad=6, labelsize=20)
plt.locator_params(axis='x', nbins=7)
plt.locator_params(axis='y', nbins=7)
plt.xlabel("\u0394\u03B5$_{11}$/\u03B5$_{11y}$", fontsize=20)
plt.ylabel("\u0394\u03B5$_{22}$/\u03B5$_{22y}$", fontsize=20)
#--------------------------------------------------------------------------#
val=data[:,data_type]
min_val=np.round(np.min(val),0)
max_val=np.round(np.max(val),0)
ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(data[:,0], data[:,1], c=val, cmap=cmap, s=30, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
plt.grid(visible='none')
#--------------------------------------------------------------------------#
plt.show()