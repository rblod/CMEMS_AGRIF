#!/bin/sh 
#
# AUTHOR - date
# ===========
# Sebastien Masson - 04/2011 - LOCEAN
#
# DESCRIPTION
# ===========
# define all varibles needed by std_plot.sh and std_plot.pro
#
# EXAMPLES
# ========
# $ . ./std_plot_vardef.sh
#
#
#===================== User PATHS =====================
#
#idl_command=/Applications/itt/idl64/bin/idl
idl_command=/Applications/itt/idl71/bin/idl
#idl_command=/usr/local_linux/idl/idl_6.4/idl64/bin/idl
#idl_command=idl71
#
PS_DIR=$( pwd )/p4H25a50-testht_ps_plot
PDF_DIR=$( pwd )/p4H25a50-testht_pdf_plot
HTML_DIR=$( pwd )/html_plot
SAXO_DIR=/Users/sflod/SAXO_DIR
#
DIR_DATA=/Users/sflod/idl_PLOTS/DATA_STORE/RUN_CLIMATO/lim3_ada     # path of data in NetCDF format
DIR_CLIMATO=/Users/sflod/idl_PLOTS/CLIMATOLOGIES    # path of climatological data
DIR_MASK=/Users/sflod/idl_PLOTS/MASK  # path of mask files (ex: subbasins)
#
#===================== Model GRID =====================
#
FILE_MESH_MASK=/Users/sflod/idl_PLOTS/MASK/ORL2PISV35_mesh_mask.nc # meshmask
FILE_MASK_SUBDOMAIN=subbasins_orca21_nored.nc              # sub-bassin masks
#
#===================== DATA =====================
#
VAR_TEMP_3D=votemper     ;   FILE_TEMP_3D=potT_annual_mean.nc                         # PHC3
VAR_SAL_3D=vosaline      ;   FILE_SAL_3D=Salt_1y_corr_PHC3WOA09.nc                    # PHC3
VAR_SST=sst              ;   FILE_SST=NewREY_ORCA2_1991_2000_1y.nc                   # Reynolds
VAR_FLUX=qnet            ;   FILE_FLUX=OAFlux_1my_01_12_1984_2004_orca2_qnet.nc      # flux
VAR_MLD=mld              ;   FILE_MLD=mld_DR003_c1m_ORCA2_1y.nc                      # Mixed layer depth
VAR_ICE_EXT_NH=extt_NH   ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Extent North Emisphere
VAR_ICE_EXT_SH=extt_SH   ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Extent South Emisphere
VAR_ICE_area_NH=area_NH  ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Area North Emisphere
VAR_ICE_area_SH=area_SH  ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Area South Emisphere
VAR_SNOW_NH=SnowDepth    ;   FILE_SNOW_ARC=Warren99_1954_1991_arctic_snowdepth.nc    # Snow arctic 
VAR_SNOW_SH=sno_dept_mm  ;   FILE_SNOW_ANT=SMMR_SSMI_antarc_sno_dep_mm1979_2004.nc   # Snow antartic
#
# Geothermal heating -> define FILE_GEOHEAT to 'NO' if there is not such forcing
#                    -> define VAR_GEOHEAT to a constant if geothermal heating is constant over the domain
VAR_GEOHEAT=heatflow   ;   FILE_GEOHEAT=geothermal_heating.nc 
#
#===================== EXP1 =====================
#
FILE1_T=p4H25a50_20410101_20501231_1Y_grid_T.nc   # exp1 grid T input file
FILE1_U=p4H25a50_20410101_20501231_1Y_grid_U.nc   # exp1 grid U input file
FILE1_V=p4H25a50_20410101_20501231_1Y_grid_V.nc   # exp1 grid V input file
FILE1_I=p4H25a50_20410101_20501231_1M_icemod.nc   # exp1 ice    input file
VAR1_T=thetao
VAR1_S=so        
VAR1_QNET=qt
VAR1_ERP=wfcorr
VAR1_EMP=wfo
VAR1_MLD=mldr10_1
VAR1_U=uocetr_eff
VAR1_V=vocetr_eff
VAR1_Ithick=sithic
VAR1_Ifrac=siconc
VAR1_Isnow=snvolu
VAR1_Isal=sisali
VAR1_Iage=siages
VAR1_IvelU=sivelu
VAR1_IvelV=sivelv
VAR1_Ivelo=sivelo
#
#===================== EXP2 =====================
#
FILE2_T=testht_20410101_20501231_1Y_grid_T.nc   # exp1 grid T input file
FILE2_U=testht_20410101_20501231_1Y_grid_U.nc   # exp1 grid U input file
FILE2_V=testht_20410101_20501231_1Y_grid_V.nc   # exp1 grid V input file
FILE2_I=testht_20410101_20501231_1M_icemod.nc   # exp1 ice    input file
VAR2_T=thetao
VAR2_S=so
VAR2_QNET=qt
VAR2_ERP=wfcorr
VAR2_EMP=wfo
VAR2_MLD=mldr10_1
VAR2_U=uocetr_eff
VAR2_V=vocetr_eff
VAR2_Ithick=sithic
VAR2_Ifrac=siconc
VAR2_Isnow=snvolu
VAR2_Isal=sisali
VAR2_Iage=siages
VAR2_IvelU=sivelu
VAR2_IvelV=sivelv
VAR2_Ivelo=sivelo
#
######################### Export Variables ###############################
#
#===================== User PATHS =====================
export PS_DIR PDF_DIR HTML_DIR SAXO_DIR
export DIR_DATA DIR_CLIMATO DIR_MASK
#===================== Model GRID =====================
export FILE_MESH_MASK FILE_MASK_SUBDOMAIN
#===================== DATA =====================
export FILE_TEMP_3D   VAR_TEMP_3D
export FILE_SAL_3D    VAR_SAL_3D
export FILE_SST       VAR_SST
export FILE_GEOHEAT   VAR_GEOHEAT
export FILE_FLUX      VAR_FLUX
export FILE_MLD       VAR_MLD
export FILE_ICE       VAR_ICE_EXT_NH  VAR_ICE_EXT_SH VAR_ICE_area_NH VAR_ICE_area_SH
export FILE_SNOW_ARC  VAR_SNOW_NH
export FILE_SNOW_ANT  VAR_SNOW_SH
#===================== EXP1 =====================
export READ_ONLY_FIRST_RECORD
export FILE1_T     FILE1_U  FILE1_V   FILE1_I
export VAR1_T      VAR1_S   VAR1_QNET VAR1_ERP VAR1_EMP VAR1_MLD 
export VAR1_U      VAR1_V
export VAR1_Ithick VAR1_Ifrac VAR1_Isnow VAR1_Isal VAR1_Iage VAR1_IvelU VAR1_IvelV VAR1_Ivelo
#===================== EXP2 =====================
export FILE2_T     FILE2_U    FILE2_V   FILE2_I
export VAR2_T      VAR2_S     VAR2_QNET VAR2_ERP VAR2_EMP VAR2_MLD 
export VAR2_U      VAR2_V
export VAR2_Ithick VAR2_Ifrac VAR2_Isnow VAR2_Isal VAR2_Iage VAR2_IvelU VAR2_IvelV VAR2_Ivelo
#
