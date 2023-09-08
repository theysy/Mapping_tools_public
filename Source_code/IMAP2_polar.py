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
# DATA_TYPE=2: ERROR(1): STRESS DIRECTION (ANGLE)
# DATA_TYPE=3: ERROR(2): EFFECTIVE STRESS
# DATA_TYPE=4: ERROR(3): STRESS TENSOR
# DATA_TYPE=5: ERROR(4): PRECISION PARAMETER
# DATA_TYPE=6: ERROR(5): ALGORITHMIC ITERATIONS
data_type=2
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
# OPEN DATA FILES #
data = np.loadtxt('OUT\IMAP_POLAR.csv', delimiter=',', dtype=np.float64)

max_indx=np.max(data[:,1])
min_indx=np.min(data[:,1])
print (max_indx)
ndata=int(np.sqrt(np.size(data[:,0])))
delta=data[1,1]-data[0,1]
#--------------------------------------------------------------------------#
fig=plt.figure(1, figsize=(10,8))
plt.rc('axes', linewidth=2.0)
#plt.axvline(x=0, color='k', linewidth=2.5)
#plt.axhline(y=0, color='k', linewidth=2.5)
r=1.0
plt.axis([min_indx*r,max_indx*r,min_indx*r,max_indx*r])
ax=plt.axes(projection='polar')
ax.set_rlabel_position(-15)
ax.set_rlim(0,10)
ax.tick_params(size=0, width=0)
#--------------------------------------------------------------------------#
val=data[:,data_type]
#min_val=np.round(np.min(val),3)
#max_val=np.round(np.max(val),3)
min_val=np.min(val)
max_val=np.max(val)
ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(data[:,0], data[:,1], c=val, cmap=cmap, s=30, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#plt.grid(visible='none')
#--------------------------------------------------------------------------#
plt.show()