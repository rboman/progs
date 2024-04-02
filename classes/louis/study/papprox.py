#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt


def func(x):
    return np.sin(x)
    # return x*x


L = 3*np.pi/2 # length of the domain

npart = 10 # nb of particles

xpart = np.linspace(0, L, npart) # particle positions
fpart = func(xpart)

xview = np.linspace(0, L, (npart-1)*5+1) # 5 points between each pair of particles
fview = func(xview)

s = xpart[1] - xpart[0]     # mesh spacing
h = 1.2*s                   # smoothing length

# V = s       # volume d'une particule
# rho = 1/V   # densité


def cubic_spline_kernel(r, h):
    alpha_d = 1.0/h
    r_h = np.abs(r)/h
    if r_h < 1.0:
        return alpha_d*(2/3-(r_h**2)+(r_h**3)/2)
    elif r_h < 2.0:
        return alpha_d*(2-r_h)**3/6
    else:
        return 0.0

def cubic_spline_kernel_d(r, h): # derivative
    alpha_d = 1.0/h
    r_h = np.abs(r)/h
    if r_h < 1.0:
        return alpha_d*(-2*r_h/h+3*r_h**2/2/h)*np.sign(r)
    elif r_h < 2.0:
        return -alpha_d*3*(2-r_h)**2/6/h*np.sign(r)
    else:
        return 0.0


# mass
mpart = np.ones(npart)  # *V*rho
mpart[0] = 0.5
mpart[-1] = 0.5


# summation density
rhopart = []
for x in xpart:
    r = 0.0
    for i in range(npart):
        r += cubic_spline_kernel(x-xpart[i], h)*mpart[i]
    rhopart.append(r)

# print(f'rhopart={rhopart}')


# print(x)
print(f's={s}')
print(f'h={h}')
# print(f'rho={rho}')

# kernel approximation
xapprox = np.linspace(0, L, (npart-1)*5+1)

fapprox = []
for x in xapprox:
    fa = 0.0
    for i in range(npart):
        # fa += fpart[i]*cubic_spline_kernel(x-xpart[i], h)*1/rho
        fa += fpart[i]*cubic_spline_kernel(x-xpart[i], h)*mpart[i]/rhopart[i]
    fapprox.append(fa)

# dfapprox = []
# for x in xapprox:
#     dfa = 0.0
#     for i in range(npart):
#         dfa += fpart[i]*cubic_spline_kernel_d(x-xpart[i], h)*mpart[i]/rhopart[i]
#     dfapprox.append(fa)


# Plot the cubic spline kernel
fig = plt.figure()
xspline = np.linspace(-3*h, 3*h, 101)
yspline = np.array([cubic_spline_kernel(x, h) for x in xspline])
yspline_d = [cubic_spline_kernel_d(x, h) for x in xspline]
yspline_d2 = (yspline[2:]-yspline[:-2])/2/(xspline[1]-xspline[0]) 
plt.plot(xspline, yspline, '-', label='kernel')
plt.plot(xspline, yspline_d, '-', label='derivative')
plt.plot(xspline[1:-1], yspline_d2, '--', label='finite difference')
plt.legend()
plt.grid(True)
fig.show()




# Plot the function
fig = plt.figure()
plt.plot(xpart, fpart, 'bo', xview, fview, 'b-')
plt.plot(xapprox, fapprox, 'r-')
plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('smooth approximation of a function')
plt.grid(True)
fig.show()

input('press <ENTER> to continue')
