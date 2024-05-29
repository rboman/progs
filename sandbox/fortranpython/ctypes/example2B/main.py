import ctypes
import numpy as np

# it is sometimes required to set full path to library
lib = ctypes.CDLL('./sumlib.so')
# types of input arguments. Array are passed as pointers to avoid making a copy
lib.cumsum.argtypes = (np.ctypeslib.ndpointer(dtype='int32',ndim=1,flags='F_CONTIGUOUS'), ctypes.c_int ) 
lib.cumsum.restypes = ctypes.POINTER(ctypes.c_int) # types of output arguments

array = np.array( [1,2,3,4,5,6], dtype='int32' )
print( 'Vector: ',  array )
print('Numpy: ', np.cumsum( array ) )
lib.cumsum( np.ctypeslib.as_array(array), len( array ) )
print( 'Fortran interface: ', array )
