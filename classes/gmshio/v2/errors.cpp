// $Id$

#include "gmshio.h"

void errorAndQuit(const char *msg, FILE *fp)
{
    printf("Error: %s\n", msg);
    if(fp) fclose(fp);
    exit(1);
}
