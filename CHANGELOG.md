

# v2.0  @ 2023-07-25

re-constructed in general, now support:

+ interfaces : `easyO, easyI, easyOA, easyIA, easyOP, easyIP`
+ overload for data type & rank
+ **I/O for sub-array in netcdf variables**
+ **provide a python script "render-struct-io.py" to generate easyIO_struct\* code for specific derived data type**

Besides, it is built based on `cmake`, along with fully test cases for different data types and functions.




***
***
***

# v1.2.1

+ add support for complex
+ bug fix for easyIA_logical_...


# v1.2  @ 2023-06-26

+ add support to 1-7 dimensional array
+ add interfaces : `easyIA` & `easyOA`, for allocatable I/O, with corresponding implementations
+ turn into `CMake` compilation process

# v1.1  @ 2023-06-02

+ add support for 5-6 and 6-d array
+ add support for store struct element by position
