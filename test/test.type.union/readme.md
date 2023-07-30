
# readme

## Preface

used to test `easyIO_*` for derived type

fortran module and froed results come from : `/mnt/d/recRoot/GitRepos/froed/test/demo1`


1. copy m1.F and `demo1.run.pk` to here
2. modify m1.F, i.e., insert the `include` directives
3. run `generate.struct_io_incs.py` to get the include files
4. run `split-fortran-lines.py` to split the long fortran lines

`-------` (below steps had been done already)

5. just run the make test, or run this test particularly
