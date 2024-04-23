#!/usr/bin/env python3

# quelques essais pour essayer de déduire les nombres de particules
# des cas-tests de Louis.


# nombre de particules d'un cube avec N particules sur 1 coté
def cubeN(N, M, P):
    return N*M*P - (N-2)*(M-2)*(P-2)



if __name__=="__main__":
    N = 34+1
    M = 34+1
    P = 31+1
    print(f'cube complet, 1 couche de particules: {cubeN(N,M,P) = }')
    print(f'cube 1 couche de particules, sans couvercle: {cubeN(N,M,P)-N*M = }')

    print(f'cube complet, 2 couches de particules: {cubeN(N,M,P) + cubeN(N+1,M+1,P+1) = }')