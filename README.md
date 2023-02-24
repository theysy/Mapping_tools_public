# Information of developer
    1. Name: Seongyong Yoon
    2. Affiliation: Max-Planck-Institut für Eisenforschung GmbH (MPIE)
    3. E-mail: s.yoon@mpie.de

# Introduction
    - This bundle is designed for evaluating numerical performance parameters of stress integration methods.
    - The source code (MML_U2SA.for) can be directly used for UMAT subroutines because the input/output arguments are shared. 
    - For the current repository, an updated source code is used. This will be merged with the original UMAT subroutine in future.
    - For one's convenience, every mapping fucntion is separated.

# Main functions
    - CMAP (Convergence map) records the interation numbers for the convergence of stress update algorithms.
    - PMAP (Precision map) records the precision parameter to estimate the fidelity of the coded stress integration methods.
    - DMAP (Data map) records any state variable such as (1) effective strain increment / (2) effective plastic strain / (3) effective stress and others.

# Data generation guide
    - The stress and other variables are incrementally calculated based on the prescribed (1) Time increment and (2) Boundary condition. (Those are easily found in MAPS.for)
    - For state variable dependent plasticity models, one may make a decision on the way to read state variables by running simulation (Sub. pre_strain).
    - The pi-plane described in this bundle is not a real pi-plane but a deviatoric plane. For convenience, lets call it pi-plane.

# Equation for the precision parameter
$$ \alpha = \sqrt{\frac{\partial \overline{\sigma}}{\partial \mathbf{\sigma}} \mathbin{:} \frac{\partial \overline{\sigma}}{\partial \mathbf{\sigma}}} $$

# Relative error
$$ Error = \frac{\lvert \alpha_{ref} - \alpha \lvert}{\alpha_{ref}} \times 100 \% $$

# How to use the code
    1. Fill out the user material property file in 'UMAT_PROPS' folder. Ex) PROPS_AA2090_YLD2K.CSV
    2. Compile and Run 'MAPS.for' using intel fortran or gfortran. (Intel fortran is faster than GNU fortran.)
    3. '(X)MAP.csv' contrains the data for mapping and will be written in the 'OUT' folder.
    4. Run '(X)MAP.py' using python. 
    5. PP: Pi-plane and YL: Yield locus. Ex) CMAP_PP.py means the convergence mapping on the pi-plane.

# Prerequisites
    1. Intel or GNU fortran
    2. Python3
    3. Python packages: numpy, matplotlib

# How to generate maps
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/MAPPING.png"></p>

# Screenshots
    1. Convergence maps
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/CMAP_PP_CPPM_0.01.png"></p>  
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/CMAP_YL_CPPM_0.01.png"></p>  

    2. Prevision maps: Precision parameters
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/PMAP_PP_CPPM_0.01.png"></p>   
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/PMAP_YL_CPPM_0.01.png"></p>

    3. Precision maps: Relative error (%)
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/EMAP_PP_CPPM_0.01.png"></p>   
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/EMAP_YL_CPPM_0.01.png"></p>   

    4. Data maps: Effective plastic strain increment
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP1_PP_CPPM_0.01.png"></p>   
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP1_YL_CPPM_0.01.png"></p>   

    5. Data maps: Effective plastic strain
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP2_PP_CPPM_0.01.png"></p>   
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP2_YL_CPPM_0.01.png"></p>   

    6. Data maps: Effective stress
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP3_PP_CPPM_0.01.png"></p>   
<p align="center"><img src="https://github.com/theysy/Mapping_tools_public/blob/main/Screenshots/DMAP3_YL_CPPM_0.01.png"></p>   
