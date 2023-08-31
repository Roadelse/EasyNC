#!/bin/bash

python run.py
python render_struct_io.py test.run.pk /mnt/d/recRoot/GitRepos/froed
python split-fortran-lines.py

gfortran -c -g -fcheck=all -cpp -ffixed-line-length-132 m1.F -I ../../cbuild.gnu/include
gfortran -c -g -fcheck=all main.F90 -I.
gfortran -fcheck=all m1.o main.o -L../../cbuild.gnu/lib -L../../../rdee_fortran/cbuild.gnu/lib  -lEasyNC -lrdee_fortran -lnetcdff

