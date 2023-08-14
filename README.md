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


## build

usually just build it in `./cbuild.gnu`, or `intel`

### structure

+ lib/ : will possess the `libEasyNC.a` library
+ include/ : will possess the `easync.mod`
+ bin/ : 
   * render-struct-io/ : possess the scripts and templates for rendering a struct I/O Fortran code


## source

+ ./src/       		: source code
   * EasyNC.F90  	: final target source code
   * prep/ 			: jinja2 templates and scripts to generate the final `EasyNC.F90`
+ ./test/      		: test F90 code


## How-to-use

built with cmake directly:

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

For specific machines, there are several environment variables which may need to be set, since it doesn't support getting compilation link & include information via `nf-config` by now.
```bash
export NETCDFF_INC_DIRECTORIES=
export NETCDFF_LINK_DIRECTORIES=
export NETCDFF_LINK_LIBS="netcdff;netcdf;hdf5_hl;hdf5;z"

export inSW=TRUE
export ENCT_NC_VIEW_CMD=ncl_filedump

CC=... FC=... cmake ..

```