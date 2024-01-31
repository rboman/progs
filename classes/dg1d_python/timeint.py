#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# time integration

import numpy as np

class RHS(object):
    """ base class for right-hand sides u'=rhs(u,x)
    """
    def __init__(self):
        pass
    def __call__(self, u, t):
        raise Exception('not implemented')

class TimeInt(object):
    def __init__(self, rhs):
        self.rhs = rhs

class BwEuler(TimeInt):
    """ Backward Euler scheme
    """
    def __init__(self, rhs):
        super(BwEuler, self).__init__(rhs)

    def onestep(self, u, t, dt):
        return u + dt*self.rhs(u, t)

class RK2(TimeInt):
    """ Runge-Kutta 2 (Zanotti slides p20)
    """
    def __init__(self, rhs):
        super(RK2, self).__init__(rhs)

    def onestep(self, u, t, dt):
        k1 = self.rhs(u, t)
        v1 = u + dt*k1
        k2 = self.rhs(v1, t+dt)
        u2 = 0.5*(u+v1+dt*k2)
        return u2

class RK4(TimeInt):
    """ Runge-Kutta 44 (4 steps - 4th order)
    """
    def __init__(self, rhs):
        super(RK4, self).__init__(rhs)

    def onestep(self, u, t, dt):
        k1 = self.rhs(u, t)
        v1 = u + dt/2*k1
        k2 = self.rhs(v1, t+dt/2)
        v2 = u + dt/2*k2
        k3 = self.rhs(v2, t+dt/2)
        v3 = u + dt*k3
        k4 = self.rhs(v3, t+dt)
        u2 = u + dt/6* ( k1+2*k2+2*k3+k4 )
        return u2

class Cos(RHS):
    def __call__(self, u, t):
        return np.cos(t)   


# -- simple test

def loop(t0, u0, int1, tmax, dt):
    u1=u0
    ts=[]
    t=t0
    ts.append(t)
    us=[]
    us.append(u1)
    while t+dt<=tmax+dt/1000:
        u2 = int1.onestep(u1, t, dt)
        t=t+dt
        ts.append(t)
        us.append(u2)
        u1=u2 
    return ts, us   

def test():
    t0, u0 = 0., 0.
    tmax = 2*np.pi
    dt = tmax/30
    rhs = Cos()
    t1, u1 = loop(t0, u0, BwEuler(rhs), tmax, dt)
    t2, u2 = loop(t0, u0, RK4(rhs), tmax, dt)
    ta = np.linspace(t0, tmax, 1000)
    ua = np.sin(ta)

    # display
    import matplotlib.pyplot as plt
    plt.figure(1)
    #plt.clf()
    plt.plot(t1, u1, 'ro-', markersize=4, label='bw-Euler')
    plt.plot(t2, u2, 'bo-', markersize=4, label='RK4')
    plt.plot(ta, ua, 'k-', markersize=4, label='exact')
    plt.xlabel('t')
    plt.ylabel('u')
    plt.ylim(-1.2, 1.2)
    plt.grid(True)
    plt.title('integration test du/dt=sin(t)')
    plt.legend()
    plt.draw()
    plt.pause(.001)    
    plt.show()


if __name__=="__main__":
    test()
