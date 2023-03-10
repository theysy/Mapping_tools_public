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
# DATA_TYPE=2: MODE1: UNIAXIAL TENSION
# DATA_TYPE=3: MODE2: BALANCED BIAXIAL TENSION
# DATA_TYPE=4: MODE3: PURE SHEAR
data_type=2
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
# OPEN DATA FILES #
data = np.loadtxt('OUT\IMAP.csv', delimiter=',', dtype=np.float64)

max_indx=np.max(data[:,0])
min_indx=np.min(data[:,0])
ndata=int(np.sqrt(np.size(data[:,0])))
delta=data[1,1]-data[0,1]
#--------------------------------------------------------------------------#
plt.figure(1, figsize=(9,8))
plt.rc('axes', linewidth=2.0)
plt.grid(which='major', alpha=0.5)
plt.axvline(x=0, color='k', linewidth=1.5)
plt.axhline(y=0, color='k', linewidth=1.5)
plt.axis([min_indx,max_indx,min_indx,max_indx])
plt.tick_params(axis='both', direction='in', length=10, width=2, pad=6, labelsize=20)
plt.xlabel("\u0394\u03B5$_{1}$/\u03B5$_{1y}$", fontsize=20)
plt.ylabel("\u0394\u03B5$_{2}$/\u03B5$_{2y}$", fontsize=20)
#--------------------------------------------------------------------------#
val=data[:,data_type]
min_val=np.round(np.min(val),1)
max_val=np.round(np.max(val),1)
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