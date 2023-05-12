#!/bin/bash


# need : real4 and logical codes

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> generate real8/int4 based on real4
sed  -e 's/real\*4/integer/' \
     -e 's/NF90_FLOAT/NF90_INT/' \
     -e 's/_real4/_int4/' \
     easync.real4.inc > easync.int4.inc

sed  -e 's/real\*4/real*8/' \
     -e 's/NF90_FLOAT/NF90_DOUBLE/' \
     -e 's/_real4/_real8/' \
     easync.real4.inc > easync.real8.inc

gfortran -cpp -E EasyNC.main.f90 | grep '^[^#]' > ../EasyNC.f90
