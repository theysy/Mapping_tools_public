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
# DATA_TYPE=7: ERROR(6): EFFECTIVE STRAIN (%)
data_type=4
#--------------------------------------------------------------------------#
# SET PLOTTING PARAMETERS #
# OPEN DATA FILES #
data = np.loadtxt('OUT\DIMAP.csv', delimiter=',', dtype=np.float64)

max_indx=np.max(data[:,0])
min_indx=np.min(data[:,0])
delta=data[1,1]-data[0,1]
ndata=np.shape(data)
#--------------------------------------------------------------------------#
# SPLIT DATA
good_data=np.zeros(ndata)
fail_data=np.zeros(ndata)
for indx in range(ndata[0]):
      if data[indx,6] == 100:
            fail_data[indx,:]=data[indx,:];
      else:
            good_data[indx,:]=data[indx,:];
#--------------------------------------------------------------------------#
plt.figure(1, figsize=(10.75,8))
plt.rc('axes', linewidth=2.0)
r=1.1
plt.axis([min_indx*r,max_indx*r,min_indx*r,max_indx*r])
plt.tick_params(axis='both', direction='in', length=10, width=2, pad=6, labelsize=30)
plt.locator_params(axis='x', nbins=5)
plt.locator_params(axis='y', nbins=5)
plt.tight_layout(pad=2.0)
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
val=good_data[:,data_type]
#min_val=np.round(np.min(val),0)
#max_val=np.round(np.max(val),0)
min_val=np.min(val)
max_val=np.max(val)
if data_type==6:
      min_val=0
      #max_val=20
      if max_val<=5:
            max_val=5
      textstr='Avg:'+str(np.round(np.mean(val)))
      props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
      plt.text(0.06, 0.095, textstr, fontsize=23, bbox=props)
elif data_type==4:
      min_val=0.0
      max_val=100
      textstr='Avg:'+str(np.round(np.mean(val)))+'%'
      props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
      coord1=max_indx*0.5; coord2=max_indx*0.95
      plt.text(coord1, coord2, textstr, fontsize=23, bbox=props)

ncmap=10
nstep=int(ncmap/2+1)
steps=np.linspace(min_val,max_val,nstep)
cmap = plt.get_cmap('Blues',ncmap)
#cmap = plt.get_cmap('cool',ncmap)
#if data_type==6:
#      plt.scatter(fail_data[:,0], fail_data[:,1], c='lightcoral', s=10)
plt.scatter(fail_data[:,0], fail_data[:,1], c='lightcoral', s=10)
plt.scatter(good_data[:,0], good_data[:,1], c=val, cmap=cmap, s=10, vmin=min_val, vmax=max_val)
cb=plt.colorbar(ticks=steps)
cb.ax.tick_params(labelsize=30, width=2, direction='in', length=11)
cb.update_ticks()
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
plt.show()