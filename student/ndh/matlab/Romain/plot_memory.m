clear all; close all;

istep = 50;
N = 0:10:100;
mem = 2*N.*N+4*N+2*(N+1)+2*(istep+1);
plot(N, mem)
grid
