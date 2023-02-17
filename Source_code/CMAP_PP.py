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
# SET PLOTTING PARAMETERS #
r=1.2             # RADIUS OF AXIS
# OPEN DATA FILES #
pi_sig=np.loadtxt('OUT\pi_plane.csv', delimiter=',', dtype=np.float64)
data = np.loadtxt('OUT\CMAP.csv', delimiter=',', dtype=np.float64)
#--------------------------------------------------------------------------#
# SPLIT CONVERGENCE DATA
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
print(interval1,interval2)
ndata=(interval1*range1+1)*(interval1*range2+1)
#--------------------------------------------------------------------------#
indx=0
ndiv=0
nconv=0
#--------------------------------------------------------------------------#
# [1] PI-PLANE AXES
pp_axis=np.zeros([3,2])
th=0*np.arccos(-1)/180
pp_axis[0,0]=r/np.sqrt(1+np.tan(th)**2)
pp_axis[0,1]=np.tan(th)*pp_axis[0,0]
th=120*np.arccos(-1)/180
pp_axis[1,0]=-r/np.sqrt(1+np.tan(th)**2)
pp_axis[1,1]=np.tan(th)*pp_axis[1,0]
th=240*np.arccos(-1)/180
pp_axis[2,0]=-r/np.sqrt(1+np.tan(th)**2)
pp_axis[2,1]=np.tan(th)*pp_axis[2,0]
plt.figure(1,figsize=(10,8))
r0=0.9/r
plt.plot([0,pp_axis[0,0]],[0,pp_axis[0,1]], 'k', linewidth=3)
plt.plot([0,-pp_axis[0,0]*r0],[0,-pp_axis[0,1]*r0], '--k', linewidth=3)  # Opposite axis
plt.annotate("$s_{xx}$", xy=(pp_axis[0,0],pp_axis[0,1]), size=30, xytext=(pp_axis[0,0]-0.1,pp_axis[0,1]-0.15))
plt.plot([0,pp_axis[1,0]],[0,pp_axis[1,1]], 'k', linewidth=3)
plt.plot([0,-pp_axis[1,0]*r0],[0,-pp_axis[1,1]*r0], '--k', linewidth=3)  # Opposite axis
plt.annotate("$s_{yy}$", xy=(pp_axis[1,0],pp_axis[1,1]), size=30, xytext=(pp_axis[1,0]+0.05,pp_axis[1,1]-0.05))
plt.plot([0,pp_axis[2,0]],[0,pp_axis[2,1]], 'k', linewidth=3)
plt.plot([0,-pp_axis[2,0]*r0],[0,-pp_axis[2,1]*r0], '--k', linewidth=3) # Opposite axis
plt.annotate("$s_{zz}$", xy=(pp_axis[2,0],pp_axis[2,1]), size=30, xytext=(pp_axis[2,0]+0.05,pp_axis[2,1]-0.05))
r2=r*1.05
plt.rc('axes', linewidth=2.0)
plt.axis([-r2,r2,-r2,r2])
plt.axis('off')
# [2] PLOT PI-PLANE
plt.plot(pi_sig[:,0], pi_sig[:,1], 'k', linewidth=2.5)
# [3] CONVERGENCE MAPPING
val=cmap_data[:,2] # Convergence iteration
min_val=0
max_val=np.round(np.max(val),0)
max_val=8
if max_val > 10:
      max_val=np.round(np.max(val),-1)
      ncmap=10
elif max_val< 6:
      max_val=max_val
      ncmap=max_val*2
else:
      ncmap=max_val
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(cmap_fail[:,0], cmap_fail[:,1], c='lightcoral', s=15)
plt.scatter(cmap_data[:,0], cmap_data[:,1], c=val, cmap=cmap, s=15, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#



