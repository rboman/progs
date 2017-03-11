/*
 * A stupid but useful crypt program
 * Two methods : 
 *    - swap byte : swap each pair of bytes.
 *    - xor       : xor all bytes with a passwd
 *
 * RoBo - May 2003
 */

#include <string.h>
#ifdef WIN32
#include "getopt.h"
#else
#include <unistd.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

void usage(char *name)
{
    printf("\nStupide util de cryptage by RoBo -  May 2003\n\n");
    //printf("usage: %s [-x|-s] [-v] -i infile [-o outfile] [-p passwd]\n", name);
    printf("usage: %s [-x|-s] [-v] infile(s) [-p passwd]\n", name);
    printf("       -s: swap bytes method\n");
    printf("       -x: xor method\n");
    printf("       -v: verbose\n");
    printf("       -k: keep input\n");
    printf("Compiled on : "__DATE__" " __TIME__"\n\n");
}

size_t readbyte(FILE *file, char *byte)
{
    return fread(byte, sizeof(char), 1, file);
}

size_t writebyte(FILE *file, char byte)
{
    return fwrite(&byte, sizeof(char), 1, file);
}

int passwd(char *key)
{
    key[0]='\0';
    while(key[0]=='\0')
    {
        printf("Password:");
        fscanf(stdin,"%s", key);
    }
    return 0;
}

int crypt01(FILE *ifile, FILE *ofile, char *key, int v)
{
    char b1, b2;
    int n1=1,n2=1;
    int count=0;
    if(v)
        printf("Swap byte method :");
    while(n1 && n2)
    {
        n1 = readbyte(ifile, &b1);
        n2 = readbyte(ifile, &b2);
        if(n2) {
            count++;
            writebyte(ofile, b2);
        }
        if(n1) {
            count++;
            writebyte(ofile, b1);
        }
    }
    if(v)
        printf(" %d bytes swapped!\n", count);
    
    return 0;
}

int crypt02(FILE *ifile, FILE *ofile, char *key, int v)
{
    char b1;
    int n1=1;
    int idx=0;
    int count=0;

    if(key[0]=='\0')
        passwd(key);
    if(v)
        printf("Xor method :");

    while(n1)
    {
        if(key[idx]=='\0') idx=0;

        n1 = readbyte(ifile, &b1);
        if(n1) {
            count++;
            writebyte(ofile, b1^(key[idx]));
        }
        idx++;
    }
    if(v)
        printf(" %d bytes xor'ed!\n", count);

    return 0;
}

int main(int argc, char *argv[])
{
    int i;
    FILE *ifile=NULL, *ofile=NULL;
    char key[255];
    char *in=NULL, *out=NULL, *pass=NULL;
    int c;
    int verbose=0;
    int (*cfun)(FILE *, FILE *, char *, int v)=crypt01;
    char myout[255];
    int keep=0;

    opterr = 0;
    
    while ((c = getopt (argc, argv, "xksvp:")) != -1)
    {
        switch (c)
        {
        case 'x':
            cfun = crypt02;
            break;
        case 's':
            cfun = crypt01;
            break;
        case 'k':
            keep = 1;
            break;
/*                        "xsvp:i:o:"
        case 'i':
            in = optarg;
           break;
        case 'o':
            out = optarg;
            break;
*/
        case 'p':
            pass = optarg;
            break;
        case 'v':
            verbose = 1;
            break;
        case '?':
            if (isprint (optopt))
                fprintf (stderr, "Unknown option `-%c'.\n", optopt);
            else
                fprintf (stderr,
                         "Unknown option character `\\x%x'.\n",
                         optopt);
            return 1;
        default:
            exit(1);
        }
    }

    //if(!in)
    if(optind==argc)
    {
        printf("no input file(s)!\n");
        usage(argv[0]);
        exit(1);
    }

    for (i = optind; i < argc; i++)
    {
        in  = argv[i];
        out = NULL;

        if(!out)
        {
            char *ext=".X";
            int n1=strlen(in);
            int n2=strlen(ext);
            int j;
            int match=0;
            out=myout;
            for(j=n1-1;j>=n1-n2 && j>=0;--j)
            {
                if(in[j]==ext[j-n1+n2]) match++;
            }
            if(match==n2) { /* removes an extension */
                strncpy(out, in, n1-n2); out[n1-n2]='\0';
            } else        { /* adds an extension */
                strcpy(out, in); out[n1]='\0';
                strcat(out, ext);
            }
        }

        if(verbose)
            printf("%s -> %s\n", in, out);
        
        if( !(ifile = fopen(in, "r+b")))
        {
            fprintf (stderr,"cannot open file %s for reading!\n", in);  
        }

        if( !(ofile = fopen(out, "w+b")))
        {
            fprintf (stderr,"cannot open file %sfor writing!\n", out);  
        }
        
        if(ifile && ofile)
        {
            if(pass)
                strcpy(key, pass);
            else
                key[0]='\0';
            (*cfun)(ifile,ofile,key,verbose);
        }
        
        if(ofile) fclose(ofile);
        if(ifile) fclose(ifile);
        
        if(!keep) {
            if(verbose)
                printf("removing file %s\n", in);
            remove(in);
        }
    }

    return 0;
}
