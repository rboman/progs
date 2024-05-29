// by Julien Heremans

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "structure.h"

int main() {

    node *nodes;
    int nnodes=10; 
    nodes = (node *) malloc(nnodes * sizeof(node));

    srand( time( NULL ) );
    for (int i = 0; i < nnodes; i++) {
        nodes[i].tag = i;
        nodes[i].x = rand() / 1e6;
        nodes[i].y = rand() / 1e6;
    }

    // Print data from C.
    printf("Print data from C\n\n");
    for (int i = 0; i < nnodes; i++) {
        printf("Point %d: %f %f\n", nodes[i].tag, nodes[i].x, nodes[i].y);
    }
    printf("\n\n");

    // Print data from fortran
    show_coordinates( nodes, nnodes);

    return 1;


}