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
plt.figure(1,figsize=(8,8))
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
#--------------------------------------------------------------------------#
# [2] PLOT PI-PLANE
plt.plot(pi_sig[:,2], pi_sig[:,3], 'k--', linewidth=2.5)
plt.plot(pi_sig[:,0], pi_sig[:,1], 'k', linewidth=2.5)
#--------------------------------------------------------------------------#
plt.show()
#--------------------------------------------------------------------------#



