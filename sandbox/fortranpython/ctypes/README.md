Examples of interfaces between python/C/Fortran (Julien Heremans)
-----------------------------------------------

1) **Example 1:** C/Fortran Interface with iso_c_binding
   Using Fortran library iso_C_binding to use C-like structures as fortran derived types. The example creates a grid of nodes in 'main.c', and the data are read from a Fortran subroutine 'show_coordinate.f90'. The "node" structure is defined in 'structure.h', and re-defined using the C-binding in the fortran subroutine.

2) **Example 2A:** C/Python Interface with c_types
   Calculate the cumsum of a integer numpy 1d-array (created in python) using a C function.

3) **Example 2B:** Fortran/Python Interface with c_types and iso_c_binding.
   Idem as previous example but using Fortran instead of C.

4) **Example 3:** C/Python Interface with c_types.
   Use a C function to interpolate data on an 1d- real valued array.

- - - - - 

**Remarks**

* The library iso_C_binding is a intrinsic module in Fortran, so no installation is required.

* Ctypes is a python library and must be previously installed. For all the presented examples using ctypes, the C or Fortran code must be compilated as a share library as follow: 
  ```
  gcc -shared -o sumlib.so -fPIC sum.c
  ```
  or 
  ```
  gfortran -shared -o sumlib.so -fPIC sum.f90
  ```
  For all the examples presented, a shell file is provided in the subfolder containing the necessary compilation commands to be executed. 

