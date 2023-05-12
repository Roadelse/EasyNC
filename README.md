# EasyNC

Encapsulation for Fortran netcdf I/O with different data type, dimension, struct data, etc.


## content

+ ./EasyNC.f90 : target file, contains a Fortran module which provide two interfaces : easyI & easyO
+ ./src/       : contains source code to generate the `EasyNC.f90`, run the `gen.sh`
+ ./test/      : test the availability
