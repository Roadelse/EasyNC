# EasyNC

Encapsulation for Fortran netcdf I/O with different data type, dimension, struct data, etc.

## Interfaces

### easyO & easyI

supports:

+ overload for different generic data type and ranks, including int4, int8, real4, real8, charater(\*), logical, complex, and max to 7-dimensional array
+ I/O with sub-array in netcdf variable, via optional arguments to specify shape_total, position and count_lens

### easyOA & easyIA

a wrapper for allocatable array (doesn't support allocatable scalar by now)

### easyOP & easyIP

a wrapper for pointer array (doesn't support pointer scalar as well), will not keep the `=>` relationship after dump & load


## binary & scripts

I provide a python script "render-struct-io.py" to generate I/O code for specific derived data type, which is used to be inserted into target fortran code file, making target struct easy to be I/O


## source

+ ./src/       		: source code
   * EasyNC.F90  	: final target source code
   * prep/ 			: jinja2 templates and scripts to generate the final `EasyNC.F90`
+ ./test/      		: test F90 code


## How-to-use

built with cmake:

```bash
mkdir cbuild && cd $_
cmake ..
make
make test
# then
ls include
ls lib
ls bin
```
