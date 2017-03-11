// xls2funct - convert a xls file to a Funct object of Oofelie
// RoBo - june 2003

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    FILE *f_in;
    int nbno=0;
    int state=0;

    if(argc<2)
    {
        fprintf(stderr, "%s [fichier noeuds]\n", argv[0]);
        exit(1);
    }

    fprintf(stderr,"opening file %s...", argv[1]);
    fflush(stdin);

    f_in = fopen(argv[1], "r+t");
    if(!f_in)
    {
        fprintf(stderr, "error!\n");
        exit(1);
    }

    // header
    fprintf(stdout,"%% Imported by xls2funct - RoBo - 2003;\n\n");
    fprintf(stdout,"Funct myf;\n\n");

    // lecture noeuds
    do
    {
        double x,y;
        state = fscanf(f_in,"%Lf,%Lf\n", &x, &y);
        if(state!=EOF)
        {
            fprintf(stdout,"myf.setdata(%20.15E, %20.15E);\n", y, x);
        }
    }
    while(state!=EOF);

    // footer
    fprintf(stdout,"\n%% EOF");

    fprintf(stderr,"done.\n");
    fclose(f_in);

    return 0;
}

