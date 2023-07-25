#!/bin/bash

gfortran -c -g -ffree-line-length-132 -I /usr/include ../src/EasyNC.F90
gfortran -c -g main.F90
gfortran -g *.o -L /usr/lib/x86_64-linux-gnu/ -lnetcdff

rm -f *nc
