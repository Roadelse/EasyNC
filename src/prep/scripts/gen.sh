#!/bin/bash


# need : real4 and logical codes
fPath=`dirname $(readlink -f $0)`
cd ${fPath}

echo "jj2 -> f90"
python render-jj2.py

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> generate real8/int4 based on real4
# echo "real4 -> int4 & real8"

# cp materials/easync.struct-defs-template.F90 ../easync.struct-defs.F90
# cp materials/easync.struct-io-template.F90 ../easync.struct-io.F90

cd ..

echo "cpp, expand"


# Please note the ',', it matters! Take care if you add some other "allocatable" in source code
sed  -e 's/allocatable,/pointer,/' \
     -e 's/easyOA/easyOP/' \
     -e 's/easyIA/easyIP/' \
     -e 's/allocated/associated/' \
     easync.allocatable.inc > easync.pointer.inc

gfortran -cpp -E EasyNC.main.F90 | grep '^[^#]' > ../EasyNC.F90
rm -f easync.main.F90 easync.numeric.inc easync.string.inc easync.allocatable.inc easync.pointer.inc

# echo "split fortran lines"
cd scripts
python split-fortran-lines.py
