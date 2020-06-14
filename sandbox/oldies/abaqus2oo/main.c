/*
   $Id$
*/

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[])
{
    FILE *f_in;
    int nbno = 0;
    int state = 0;

    if (argc < 2)
    {
        fprintf(stderr, "%s [fichier noeuds]\n", argv[0]);
        exit(1);
    }

    fprintf(stderr, "opening file %s...", argv[1]);
    fflush(stdin);

    f_in = fopen(argv[1], "r+t");
    if (!f_in)
    {
        fprintf(stderr, "error!\n");
        exit(1);
    }

    // header
    fprintf(stdout, "%% Imported by Abaqus2oo - RoBo - 2003;\n\n");
    fprintf(stdout, "Refer dom(domain);\n");
    fprintf(stdout, "Refer nset(dom[NODESET_PO]); nset.build_hash();\n");

    // lecture noeuds
    do
    {
        int no;
        double x, y;
        state = fscanf(f_in, "%d,%Lf,%Lf\n", &no, &x, &y);
        if (state != EOF)
        {
            fprintf(stdout, "nset.define(%d, %20.15E, %20.15E, 0);\n", no, x,
                    y);
        }
    } while (state != EOF);

    // footer
    fprintf(stdout, "\n%% EOF");

    fprintf(stderr, "done.\n");
    fclose(f_in);

    return 0;
}
