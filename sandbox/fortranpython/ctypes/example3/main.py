import numpy as np
import ctypes
from matplotlib import pyplot as plt

xx = np.linspace( 0, 2*np.pi, 100 )
yy = np.sin( xx )

# Defines a ctypes structure to hold the data
class Data( ctypes.Structure ):
    _fields_ = [
        ( 'n', ctypes.c_int ),
        ( 'x', ctypes.POINTER( ctypes.c_double ) ),
        ( 'y', ctypes.POINTER( ctypes.c_double ) ), 
        # We don't know the type where spline pointer points to (it is defined in a module used by interpolate.c), so we use void pointer as a ghost pointer 
        ( 'spline', ctypes.POINTER( ctypes.c_void_p ) ), # void pointer to the spline object
        ( 'acc', ctypes.c_void_p) # void pointer to the acc object
    ]

# Create and fill the data structure
data = Data()
data.n = len( xx )
data.x = xx.ctypes.data_as( ctypes.POINTER( ctypes.c_double ) )
data.y = yy.ctypes.data_as( ctypes.POINTER( ctypes.c_double ) )


# Load the shared library
lib = ctypes.CDLL( './interpolate.so' )
lib.interp1d.argtypes = [ctypes.POINTER(Data), ctypes.POINTER(ctypes.c_double)]
lib.interp1d.restype = ctypes.c_double

npts = 10
xnew = np.random.rand(10) * 2*np.pi
ynew = np.zeros( npts )
# Pass data and xnew as pointers to the C function
for i in range(npts):
    ynew[i] = lib.interp1d( ctypes.byref( data ), ctypes.byref(ctypes.c_double( xnew[i] ) ) )

plt.figure()
plt.plot( xx, yy )
plt.plot( xnew, ynew, 'ro' )
plt.grid()
plt.xlabel( 'Time t [s]' )
plt.ylabel( 'Amplitude' )
plt.show()