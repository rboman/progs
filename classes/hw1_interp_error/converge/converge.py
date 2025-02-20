#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# plot the error as a function of the number of elements and their interpolation order
# the code is run several times in parallel using all the available cores

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import os
import re
import subprocess
import multiprocessing
import sys
import math

def runHW1(geofile, order, factor):
    # print(f"hello order={order} factor={factor}")
    # return factor, order
    cwd = os.path.dirname(__file__)
    # run program
    exe = os.path.join(cwd, '..', 'build', 'hw1')
    cmd = [exe, geofile, "-setnumber", "factor",
           f"{factor}", "-order", f"{order}", "-v", "0", "-nopopup"]
    out = subprocess.check_output(cmd).decode()
    # parse output
    m = re.search(r'Nelems = (.+)', out)
    Nelems = int(m.group(1))
    m = re.search(r'Etot_i = (.+)', out)
    Etot_i = float(m.group(1))
    m = re.search(r'Etot_a = (.+)', out)
    Etot_a = float(m.group(1))
    m = re.search(r'Amean = (.+)', out)
    Amean = float(m.group(1))
    return order, Nelems, Amean, Etot_i, Etot_a


def params(geofile):
    for factor in range(10, 0, -1):
        for order in range(7, 0, -1):
            yield (geofile, order, factor)


def main(geofile):

    curves = {}

    pool = multiprocessing.Pool()
    for result in pool.starmap(runHW1, params(geofile)):
        order, Nelems, Amean, Etot_i, Etot_a = result
        curves.setdefault(order, [[], [], [], []])
        curves[order][0].append(Nelems)
        curves[order][1].append(math.sqrt(Amean))
        curves[order][2].append(Etot_i)
        curves[order][3].append(Etot_a)

    # Errors wrt Nelem
    fig1 = plt.figure(1)
    for order, lists in sorted(curves.items()):
        nelems, etoti, etota = zip(*sorted(zip(lists[0], lists[2], lists[3])))
        plt.loglog(nelems, etoti, 'o-', label=f'order={order}', color=f"C{order-1}")
        plt.loglog(nelems, etota, 'o--', color=f"C{order-1}")

    plt.grid(which='both')
    plt.legend()
    plt.ylabel('L2 norm of the error')
    plt.xlabel('Number of elements')
    plt.title(f'{os.path.basename(geofile)}\n(solid lines: interpolation -- dashed lines: approximation)')
    # fig1.savefig(geofile.replace('.geo', '_nelems.png'))
    fig1.show()

    # Errors wrt hmean
    fig2 = plt.figure(2)
    for order, lists in curves.items():
        hmean, etoti, etota = zip(*sorted(zip(lists[1], lists[2], lists[3])))
        plt.loglog(hmean, etoti, 'o-', label=f'order={order}', color=f"C{order-1}")
        plt.loglog(hmean, etota, 'o--', color=f"C{order-1}")
        try:
            slopei = (math.log(etoti[-1])-math.log(etoti[-3]))/(math.log(hmean[-1])-math.log(hmean[-3]))
            slopea = (math.log(etota[-1])-math.log(etota[-3]))/(math.log(hmean[-1])-math.log(hmean[-3]))
        except:
            slopei, slopea = 0,0
        print(f'order={order}, slope_i={slopei:.1f}, slope_a={slopea:.1f}')

    plt.grid(which='both')
    plt.legend()
    plt.ylabel('L2 norm of the error')
    plt.xlabel('Mean edge length')
    plt.title(f'{os.path.basename(geofile)}\n(solid lines: interpolation -- dashed lines: approximation)')
    fig2.savefig(geofile.replace('.geo', '.png'))
    fig2.show()

    plt.show()



if __name__ == "__main__":
    cwd = os.path.dirname(__file__)
    geofile = os.path.join(cwd, 'louvain.geo')

    if len(sys.argv) > 1:
        geofile = sys.argv[1]

    if os.path.isfile(geofile):
        print(f'running convergence study for {geofile}...')
        main(geofile)
    else:
        print(f'usage: {sys.argv[0]} <geofile.geo>')
