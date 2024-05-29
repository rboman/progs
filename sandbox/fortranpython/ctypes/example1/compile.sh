#!/bin/bash
gcc -c main.c -o main.o
gfortran -g main.o show_coordinates.f90 -o a.out