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
# OPEN DATA FILES #
yl_sig=np.loadtxt('OUT\yld_locus.csv', delimiter=',', dtype=np.float64)
data = np.loadtxt('OUT\CMAP.csv', delimiter=',', dtype=np.float64)
#--------------------------------------------------------------------------#
ang1=int(max(data[:,0])+1)
ang2=int(max(data[:,1])+1)
cmap_data=np.zeros([ang1*ang2,3])
cmap_fail=np.zeros([ang1*ang2,3])
for indx in range(ang1*ang2):
      if data[indx,6] == 200:
            cmap_fail[indx,0]=data[indx,2];
            cmap_fail[indx,1]=data[indx,3];
            cmap_fail[indx,2]=data[indx,6];
      else:
            cmap_data[indx,0]=data[indx,2];
            cmap_data[indx,1]=data[indx,3];
            cmap_data[indx,2]=data[indx,6];
#--------------------------------------------------------------------------#
range1=max(data[:,0])-min(data[:,0])
range2=max(data[:,1])-min(data[:,1])
interval1=data[int(range2+1),0]-data[0,0]
interval2=data[1,1]-data[0,1]
ndata=(interval1*range1+1)*(interval1*range2+1)
#--------------------------------------------------------------------------#
# [1] Yield surface template
r=1.5
th=0*np.arccos(-1)/180
yl_axis=np.zeros([2,2])
x=r/np.sqrt(1+np.tan(th)**2)
yl_axis[0,0]=r/np.sqrt(1+np.tan(th)**2)
yl_axis[0,1]=np.tan(th)*yl_axis[0,0]
th=90*np.arccos(-1)/180
yl_axis[1,0]=r/np.sqrt(1+np.tan(th)**2)
yl_axis[1,1]=np.tan(th)*yl_axis[1,0]
r2=r
plt.figure(1, figsize=(9,8))
plt.rc('axes', linewidth=2.0)
plt.grid(which='major', alpha=0.5)
plt.axis([-r2,r2,-r2,r2])
plt.axvline(x=0, color='k', linewidth=2.0)
plt.axhline(y=0, color='k', linewidth=2.0)
plt.tick_params(axis='both', direction='in', length=5, pad=6, labelsize=20)
#--------------------------------------------------------------------------#
# [2] Yield locus
plt.plot(yl_sig[:,0], yl_sig[:,1], 'k--', linewidth=2.5)# ORIGINAL YIELD LOCUS
plt.plot(yl_sig[:,2], yl_sig[:,3], 'k', linewidth=2.5)  # CURRENT YIELD LOCUS
#--------------------------------------------------------------------------#
# [3] CONVERGENCE MAPPING
val=cmap_data[:,2] # Convergence iteration
min_val=0
max_val=np.round(np.max(val),0)
if max_val > 10:
      max_val=np.round(np.max(val),-1)
      ncmap=10
elif max_val< 8:
      ncmap=max_val*2
else:
      ncmap=max_val
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(cmap_fail[:,0], cmap_fail[:,1], c='lightcoral', s=10)
plt.scatter(cmap_data[:,0], cmap_data[:,1], c=val, cmap=cmap, s=10, vmin=min_val, vmax=max_val)
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
textstr='Mean: '+str(np.round(np.mean(val)))
plt.text(-1.3, 1.2, textstr, fontsize=25, bbox=props)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#


