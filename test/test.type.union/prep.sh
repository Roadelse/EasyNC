#!/bin/bash

python generate.struct_io_incs.py
python split-fortran-lines.py

#gfortran -c -g -fcheck=all -cpp -ffixed-line-length-132 m1.F -I ../../cbuild.gnu/include
#gfortran -c -g -fcheck=all main.F90 -I.
#gfortran -fcheck=all m1.o main.o -L../../cbuild.gnu/lib -L../../../rdee_fortran/cbuild.gnu/lib  -lEasyNC -lrdee_fortran -lnetcdff

