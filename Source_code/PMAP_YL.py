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
# DATA_TYPE=6: REFERENCE PRECISION PARAMETER
# DATA_TYPE=7: PRECISION PARAMETER
# DATA_TYPE=8: RELATIVE ERROR BETWEEN REFERENCE DATA AND GENERATED DATA
data_type=7
#--------------------------------------------------------------------------#
# OPEN DATA FILES #
yl_sig=np.loadtxt('OUT\yld_locus.csv', delimiter=',', dtype=np.float64)
data = np.loadtxt('OUT\PMAP.csv', delimiter=',', dtype=np.float64)
range1=max(data[:,0])-min(data[:,0])
range2=max(data[:,1])-min(data[:,1])
interval1=data[int(range2),0]-data[0,0]
interval2=data[1,1]-data[0,1]
print(interval1,interval2)
ndata=(interval1*range1+1)*(interval1*range2+1)
#--------------------------------------------------------------------------#
# Yield surface template
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
# Yield locus
plt.plot(yl_sig[:,0], yl_sig[:,1], 'k--', linewidth=2.5)# ORIGINAL YIELD LOCUS
plt.plot(yl_sig[:,2], yl_sig[:,3], 'k', linewidth=2.5)  # CURRENT YIELD LOCUS
#--------------------------------------------------------------------------#
# PRECISION MAPPING
val=data[:,data_type]
min_val=np.round(np.min(val),1)
max_val=np.round(np.max(val),1)
#if data_type<=7:
#      min_val=1.2
#      max_val=1.3

ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(data[:,2], data[:,3], c=val, cmap=cmap, s=10, vmin=min_val, vmax=max_val)
if data_type==8:
      props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
      textstr='Max: '+str(np.round(np.max(val),1))+'%'
      plt.text(-1.3, 1.2, textstr, fontsize=25, bbox=props)

cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#


