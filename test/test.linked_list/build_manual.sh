#!/bin/bash

python run.py
python render_struct_io.py test.run.pk /mnt/d/recRoot/GitRepos/froed
python split-fortran-lines.py

gfortran -c -g test.linked_list.F90 -I ../../cbuild.gnu/include -std=f2008 -fcheck=all
gfortran test.linked_list.o -L ../../cbuild.gnu/lib -L ../../../rdee_fortran/cbuild.gnu/lib  -lEasyNC -lrdee_fortran -lnetcdff -lnetcdf

rm -f test.linked_list.nc
