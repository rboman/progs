#!/usr/bin/env python

import matplotlib
import csv
import numpy as np
import matplotlib.pyplot as plt


def readExps(fname):
    exps = []
    with open(fname, 'rb') as csvfile:
        f = csv.reader(csvfile, delimiter='\t')  # , quotechar='""')
        for row in f:
            try:
                exps.append(
                    {
                        "host": row[0],
                        "gpu": row[1],
                        "size": int(row[2]),
                        "prog": row[3].strip(),
                        "nit": int(row[4]),
                        "time": float(row[5])
                    })
            except:
                pass
    return exps

def plotfig(labels, values, title, ylabel, ymax=None):
    x = np.arange(len(labels))  # the label locations
    nbars = len(times)
    width = 0.6 # the width of a series of bars
    inter = (0.1*width)/(nbars-1) # empty space between 2 bars
    wbar = (width-inter*(nbars-1))/nbars # the width of 1 bar

    fig, ax = plt.subplots()
    k=0
    for key, val in values.items():
        #print (key, x, val)
        rects1 = ax.bar(x - width/2 + (2*k+1)*wbar/2 + k*inter, values[key], wbar, label=key)
        k+=1

    ax.set_xlabel('Matrix size')
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.set_xticks(x)
    ax.set_xticklabels(labels)
    ax.set_ylim(top=ymax)
    ax.legend()

    fig.tight_layout()

    fig.show()
    return fig


# read experiments
exps = readExps('treebeard.csv')
exps.extend( readExps('garfield.csv') )

# extract hosts / gpus
gpus = list(set([ e["gpu"] for e in exps ]))
hosts = list(set([ e["host"] for e in exps ]))

# comparison of solvers on a given machine
# ----------------------------------------
for host, gpu in zip(hosts, gpus):
    print 'processing results for', host, gpu
    sizes = np.unique([ e["size"] for e in exps ])
    progs = set([ e["prog"] for e in exps ])

    # extract relevant data from exps
    times={}
    iters={}
    times_per_it={}
    speedups={}
    for p in progs:
        times[p] = np.array([ e["time"] for e in exps if e["prog"]==p and e["host"]==host])
        iters[p] = np.array([ e["nit"] for e in exps if e["prog"]==p and e["host"]==host])
        times_per_it[p] = times[p] / iters[p]
        cpu_serial = np.array([ e["time"] for e in exps if e["prog"]=="serial" and e["host"]==host])
        #cpu_serial = np.array([ e["time"] for e in exps if e["prog"]=="serial" and e["host"]=="garfield"])
        speedups[p] = cpu_serial/times[p]

    #print np.array([ e["time"] for e in exps if e["prog"]=="serial"])
    #print np.array([ e["time"] for e in exps ])
    speedup_max = 0.
    for p in progs:
        tmax = np.array([ e["time"] for e in exps if e["prog"]==p ])
        #print 'tmax=', tmax
        tserial = np.array([ e["time"] for e in exps if e["prog"]=="serial"])
        #print 'tserial=', tserial
        sp = tserial / tmax
        #print 'sp=', sp
        spmax = np.max( tserial / tmax)
        #print 'spmax=', spmax
        speedup_max = max( speedup_max, spmax )
    speedup_max *=1.1
    times_max = np.max([ e["time"] for e in exps ])*1.1
    print (times_max)
    times_per_it_max = np.max( np.array([ e["time"] for e in exps ])/np.array([ e["nit"] for e in exps ]) )*1.1
    print (times_per_it_max)

    fig = plotfig(labels=sizes, values=times_per_it, title='Jacobi solver - {} ({})'.format(host, gpu), ylabel='Wall-clock time per iteration [s]', ymax=times_per_it_max)
    plt.savefig('cpu_{}.png'.format(host))
    fig = plotfig(labels=sizes, values=times, title='Jacobi solver - {} ({})'.format(host, gpu), ylabel='Total wall-clock time [s]',ymax=times_max)
    fig = plotfig(labels=sizes, values=speedups, title='Jacobi solver - {} ({})'.format(host, gpu), ylabel='Speedup []', ymax=speedup_max)
    plt.savefig('speedup_{}.png'.format(host))

# speedups on a given machine
# ---------------------------






raw_input('<PRESS ENTER>')