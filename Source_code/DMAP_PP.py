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
# DATA_TYPE=6: EFFECTIVE PLASTIC STRAIN INCREMENT
# DATA_TYPE=7: EFFECTIVE PLASTIC STRAIN
# DATA_TYPE=8: EFFECTIVE STRESS
data_type=6
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
print_par=0       # Print the result on csv file. (0: off / 1: on)
r=1.2             # RADIUS OF AXIS
# OPEN DATA FILES #
pi_sig=np.loadtxt('OUT\pi_plane.csv', delimiter=',', dtype=np.float64)
data = np.loadtxt('OUT\DMAP.csv', delimiter=',', dtype=np.float64)
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
# [3] DATA MAPPING
val=data[:,data_type]
min_val=np.min(val)
max_val=np.max(val)
ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
plt.scatter(data[:,2], data[:,3], c=val, cmap=cmap, s=15, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps, format='%.0e')
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#



