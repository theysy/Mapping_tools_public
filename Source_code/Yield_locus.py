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
data = np.loadtxt('OUT\PMAP.csv', delimiter=',', dtype=np.float64)
range1=max(data[:,0])-min(data[:,0])
range2=max(data[:,1])-min(data[:,1])
interval1=data[int(range2+1),0]-data[0,0]
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
plt.figure(1, figsize=(9,9))
plt.rc('axes', linewidth=2.0)
plt.grid(which='major', alpha=0.5)
plt.axis([-r2,r2,-r2,r2])
plt.axvline(x=0, color='k', linewidth=2.0)
plt.axhline(y=0, color='k', linewidth=2.0)
plt.tick_params(axis='both', direction='in', length=5, pad=6, labelsize=20)
plt.xlabel("$\\sigma_{11}/\\sigma_{y}$", fontsize=20)
plt.ylabel("$\\sigma_{22}/\\sigma_{y}$", fontsize=20)
#--------------------------------------------------------------------------#
# Yield locus
plt.plot(yl_sig[:,0], yl_sig[:,1], 'k--', linewidth=2.5)
plt.plot(yl_sig[:,2], yl_sig[:,3], 'k', linewidth=2.5)
x=np.linspace(yl_sig[44,0],yl_sig[224,0])
y=np.linspace(yl_sig[44,1],yl_sig[224,1])
plt.plot(x, y, 'k--', linewidth=2.5)
x=np.linspace(yl_sig[134,0],yl_sig[314,0])
y=np.linspace(yl_sig[134,1],yl_sig[314,1])
plt.plot(-x, x, 'k--', linewidth=2.5)
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#


