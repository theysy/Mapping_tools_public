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
# DATA_TYPE=2: ERROR1: ANGLE BETWEEN STRESS DIRECTIONS
# DATA_TYPE=3: ERROR2: RELATIVE ERROR OF EFFECTIVE STRESS
# DATA_TYPE=4: MODE3: PURE SHEAR
data_type=7
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
# OPEN DATA FILES #
yl_sig=np.loadtxt('OUT\yld_locus.csv', delimiter=',', dtype=np.float64)
data = np.loadtxt('OUT\IMAP.csv', delimiter=',', dtype=np.float64)

max_indx=np.max(data[:,0])
min_indx=np.min(data[:,0])
ndata=int(np.sqrt(np.size(data[:,0])))
delta=data[1,1]-data[0,1]
#--------------------------------------------------------------------------#
r=1.5
plt.figure(1, figsize=(9,8))
plt.rc('axes', linewidth=2.0)
plt.grid(which='major', alpha=0.5)
plt.axvline(x=0, color='k', linewidth=1.5)
plt.axhline(y=0, color='k', linewidth=1.5)
#plt.axis([min_indx,max_indx,min_indx,max_indx])
plt.axis([-r,r,-r,r])
plt.tick_params(axis='both', direction='in', length=10, width=2, pad=6, labelsize=20)
plt.locator_params(axis='x', nbins=7)
plt.locator_params(axis='y', nbins=7)
#--------------------------------------------------------------------------#
# Yield locus
plt.plot(yl_sig[:,0], yl_sig[:,1], 'k--', linewidth=2.5)# ORIGINAL YIELD LOCUS
plt.plot(yl_sig[:,2], yl_sig[:,3], 'k', linewidth=2.5)  # CURRENT YIELD LOCUS
#--------------------------------------------------------------------------#
val=data[:,data_type]
#min_val=np.round(np.min(val),1)
#max_val=np.round(np.max(val),1)
min_val=np.min(val)
max_val=np.max(val)
ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(data[:,0], data[:,1], c=val, cmap=cmap, s=15, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
plt.grid(visible='none')
#--------------------------------------------------------------------------#
plt.show()