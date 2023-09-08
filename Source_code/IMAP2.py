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
# DATA_TYPE=3: ERROR(2): EFFECTIVE STRESS ERROR
# DATA_TYPE=4: ERROR(3): STRESS TENSOR
# DATA_TYPE=5: ERROR(4): PRECISION PARAMETER
# DATA_TYPE=6: ERROR(5): ALGORITHMIC ITERATIONS
# DATA_TYPE=7: ERROR(6): EFFECTIVE STRAIN (VALUE)
data_type=2
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
#plt.grid(which='major', alpha=0.5)
#plt.axvline(x=0, color='k', linewidth=2.5)
#plt.axhline(y=0, color='k', linewidth=2.5)
#plt.axis([min_indx,max_indx,min_indx,max_indx])
r=1.1
plt.axis([min_indx*r,max_indx*r,min_indx*r,max_indx*r])
plt.tick_params(axis='both', direction='in', length=10, width=2, pad=6, labelsize=20)
plt.locator_params(axis='x', nbins=5)
plt.locator_params(axis='y', nbins=5)
#plt.xlabel("\u0394t", fontsize=20)
#plt.ylabel("\u0394t", fontsize=20)
#--------------------------------------------------------------------------#
radian=np.arctan(1.0)/45
frame_coord=np.zeros([360,2])
for i in range(360):
      frame_coord[i,0]=np.cos(i*radian)*max_indx
      frame_coord[i,1]=np.sin(i*radian)*max_indx
frame_coord_x=np.linspace(-1,1)*frame_coord[0,0]
frame_coord_y=np.linspace(-1,1)*frame_coord[0,1]
plt.plot(frame_coord_x,frame_coord_y, 'k--', linewidth=1.0)
frame_coord_x=np.linspace(-1,1)*frame_coord[90,0]
frame_coord_y=np.linspace(-1,1)*frame_coord[90,1]
plt.plot(frame_coord_x,frame_coord_y, 'k--', linewidth=1.0)
frame_coord_x=np.linspace(-1,1)*frame_coord[45,0]
frame_coord_y=np.linspace(-1,1)*frame_coord[45,1]
plt.plot(frame_coord_x,frame_coord_y, 'k--', linewidth=1.0)
#plt.plot(frame_coord_x,frame_coord_y, color='lightgrey', linewidth=1.0)
frame_coord_x=np.linspace(-1,1)*frame_coord[-45,0]
frame_coord_y=np.linspace(-1,1)*frame_coord[-45,1]
plt.plot(frame_coord_x,frame_coord_y, 'k--', linewidth=1.0)
#plt.plot(frame_coord_x,frame_coord_y, color='lightgrey', linewidth=1.0)
for i in range(5):
      r2=i/5
      plt.plot(frame_coord[:,0]*r2,frame_coord[:,1]*r2, color='grey', linewidth=1.0)
plt.plot(frame_coord[:,0],frame_coord[:,1], color='k', linewidth=3)
#--------------------------------------------------------------------------#
#r3=1.05
#r4=1.15
#plt.text(frame_coord[0,0]*r3,frame_coord[0,1]*r3, '0'+'\xb0', fontsize=20)
#plt.text(frame_coord[90,0]*r3,frame_coord[90,1]*r3, '90'+'\xb0', fontsize=20)
#plt.text(frame_coord[180,0]*r4,frame_coord[180,1]*r4, '180'+'\xb0', fontsize=20)
#plt.text(frame_coord[270,0]*r4,frame_coord[270,1]*r4, '270'+'\xb0', fontsize=20)
#--------------------------------------------------------------------------#
val=data[:,data_type]
#min_val=np.round(np.min(val),0)
#max_val=np.round(np.max(val),0)
min_val=np.min(val)
max_val=np.max(val)
ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
#cmap = plt.get_cmap('cool',ncmap)
plt.scatter(data[:,0], data[:,1], c=val, cmap=cmap, s=10, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=25, width=2, direction='in', length=11)
cb.update_ticks()
#plt.grid(True)
#--------------------------------------------------------------------------#
plt.show()