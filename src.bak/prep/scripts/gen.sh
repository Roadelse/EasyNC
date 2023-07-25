#!/bin/bash


# need : real4 and logical codes
fPath=`dirname $(readlink -f $0)`
cd ${fPath}

echo "jj2 -> f90"
python render-jj2.py

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> generate real8/int4 based on real4
echo "real4 -> int4 & real8"

cd ..

sed  -e 's/real(kind=4)/integer(kind=4)/' \
     -e 's/NF90_FLOAT/NF90_INT/' \
     -e 's/_real4/_int4/' \
     easync.real4.inc > easync.int4.inc

sed  -e 's/real(kind=4)/real(kind=8)/' \
     -e 's/NF90_FLOAT/NF90_DOUBLE/' \
     -e 's/_real4/_real8/' \
     easync.real4.inc > easync.real8.inc

echo "cpp, expand"

gfortran -cpp -E EasyNC.main.F90 | grep '^[^#]' > ../EasyNC.F90

echo "split fortran lines"
cd scripts
python split-fortran-lines.py
