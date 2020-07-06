#!/bin/bash

################################################################
#
# Script to launch a set of STATION_ASF simulations
#
# L. Brodeau, 2020
#
################################################################

# What directory inside "tests" actually contains the compiled "nemo.exe" for STATION_ASF ?
TC_DIR="STATION_ASF2"

# DATA_IN_DIR => Directory containing sea-surface + atmospheric forcings
#             (get it there https://drive.google.com/file/d/1MxNvjhRHmMrL54y6RX7WIaM9-LGl--ZP/):
if [ `hostname` = "merlat"        ]; then
    DATA_IN_DIR="/MEDIA/data/STATION_ASF/input_data_STATION_ASF_2016-2018"
elif [ `hostname` = "luitel"        ]; then
    DATA_IN_DIR="/data/gcm_setup/STATION_ASF/input_data_STATION_ASF_2016-2018"
elif [ `hostname` = "ige-meom-cal1" ]; then
    DATA_IN_DIR="/mnt/meom/workdir/brodeau/STATION_ASF/input_data_STATION_ASF_2016-2018"
elif [ `hostname` = "salvelinus" ]; then
    DATA_IN_DIR="/opt/data/STATION_ASF/input_data_STATION_ASF_2016-2018"
else
    echo "Oops! We don't know `hostname` yet! Define 'DATA_IN_DIR' in the script!"; exit 
fi

expdir=`basename ${PWD}`; # we expect "EXPREF" or "EXP00" normally...

# NEMOGCM root directory where to fetch compiled STATION_ASF nemo.exe + setup:
NEMO_WRK_DIR=`pwd | sed -e "s|/tests/STATION_ASF/${expdir}||g"`

# Directory where to run the simulation:
PROD_DIR="${HOME}/tmp/STATION_ASF"


####### End of normal user configurable section #######

#================================================================================

# NEMO executable to use is:
NEMO_EXE="${NEMO_WRK_DIR}/tests/${TC_DIR}/BLD/bin/nemo.exe"


echo "###########################################################"
echo "#        S T A T I O N   A i r  -  S e a   F l u x        #"
echo "###########################################################"
echo
echo " We shall work in here: ${STATION_ASF_DIR}/"
echo " NEMOGCM   work    depository is: ${NEMO_WRK_DIR}/"
echo "   ==> NEMO EXE to use: ${NEMO_EXE}"
echo " Input forcing data into: ${DATA_IN_DIR}/"
echo " Production will be done into: ${PROD_DIR}/"
echo

mkdir -p ${PROD_DIR}

if [ ! -f ${NEMO_EXE} ]; then echo " Mhhh, no compiled 'nemo.exe' found into `dirname ${NEMO_EXE}` !"; exit; fi

echo
echo " *** Using the following NEMO executable:"
echo "  ${NEMO_EXE} "
echo

NEMO_EXPREF="${NEMO_WRK_DIR}/tests/STATION_ASF/EXPREF"
if [ ! -d ${NEMO_EXPREF} ]; then echo " Mhhh, no EXPREF directory ${NEMO_EXPREF} !"; exit; fi

rsync -avP ${NEMO_EXE}          ${PROD_DIR}/

for ff in "context_nemo.xml" "domain_def_nemo.xml" "field_def_nemo-oce.xml" "file_def_nemo-oce.xml" "grid_def_nemo.xml" "iodef.xml" "namelist_ref"; do
    if [ ! -f ${NEMO_EXPREF}/${ff} ]; then echo " Mhhh, ${ff} not found into ${NEMO_EXPREF} !"; exit; fi
    rsync -avPL ${NEMO_EXPREF}/${ff} ${PROD_DIR}/
done

# Copy forcing to work directory:
rsync -avP ${DATA_IN_DIR}/Station_PAPA_50N-145W*.nc ${PROD_DIR}/

for CASE in "ECMWF" "COARE3p6" "NCAR" "ECMWF-noskin" "COARE3p6-noskin"; do

    echo ; echo
    echo "============================="
    echo " Going for ${CASE} experiment"
    echo "============================="
    echo

    scase=`echo "${CASE}" | tr '[:upper:]' '[:lower:]'`

    rm -f ${PROD_DIR}/namelist_cfg
    rsync -avPL ${NEMO_EXPREF}/namelist_${scase}_cfg ${PROD_DIR}/namelist_cfg

    cd ${PROD_DIR}/
    echo
    echo "Launching NEMO !"
    ./nemo.exe 1>out_nemo.out 2>err_nemo.err
    echo "Done!"
    echo

    # Moving output files:
    mkdir -p output
    mv -f STATION_ASF-${CASE}_*_grid*.nc output/
    
    # Saving logs:
    mkdir -p ${CASE}_log
    mv -f *.out *.err ocean.output output.namelist.dyn ${CASE}_log/

done
