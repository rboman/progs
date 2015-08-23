//
// $Id$
//
// convert a *.m vector/matrix metafor file to a *.txt file
// use load -ascii pipo.txt to load it inside matlab (faster)
//

#include <stdio.h>
#include <stdlib.h>
#include <vector>

typedef std::vector<std::vector<double> > MyArray;

bool
manageErr(int err)
{
    switch(err)
    {
    case 0:
        fprintf(stderr, "** error : no field assigned!\n");
        return false;
    case EOF:
        fprintf(stderr, "** error : end of file reached!\n");
        return false;
    default:
        return true;
    }
}

bool
skipname(FILE *file)
{
    for(;;)
    {
        char c;
        c=fgetc(file);
        if(c==EOF)
            return false;
        if(c=='(')
            return true;
    }
}

bool
getidx(FILE *file, int *ni, int *nj)
{
    *ni=1; *nj=1;
    int lost=0;
    int err = fscanf(file,"%d", ni);
    if(!manageErr(err)) return false;
    for(;;)
    {
        char c;
        c=fgetc(file);
        if(c==EOF)
            return false;
        if(c==')')
            return true;
        if(c==',')
            return getidx(file, nj, &lost);
    }
}


bool
getvalue(FILE *file, double *val)
{
    int err = fscanf(file," =%lf", val);
    return manageErr(err);
}

bool
manageFileErr(FILE *file)
{
    if(file) return true;
    fprintf(stderr, "cannot open file!\n");
    exit(1);
}

bool
openFileR(FILE **file, char *name)
{
    *file = fopen(name, "r+t"); 
    return manageFileErr(*file);
}

bool
openFileW(FILE **file, char *name)
{
    *file = fopen(name, "w+t"); 
    return manageFileErr(*file);
}

void 
getMaxIdx(FILE *file, int *nimax, int *njmax)
{
    *nimax=1;
    *njmax=1;
    for(;;)
    {
        int ni,nj;
        double val;
        if(!skipname(file)) break;
        if(!getidx(file, &ni, &nj)) break;
        if(!getvalue(file, &val)) break;
        *nimax = (ni>*nimax)? ni:*nimax;
        *njmax = (nj>*njmax)? nj:*njmax;
    }
}

void 
fillArray(FILE *file, MyArray &array)
{
    for(;;)
    {
        int ni,nj;
        double val;
        if(!skipname(file)) break;
        if(!getidx(file, &ni, &nj)) break;
        if(!getvalue(file, &val)) break;
        array[ni-1][nj-1]=val;
    }
}

void
getMaxIdx(char *name, int *nimax, int *njmax)
{
    FILE *file;
    openFileR(&file, name);
    getMaxIdx(file, nimax, njmax);
    fclose(file);
}

void
writeArray(char *name, MyArray &array)
{
    FILE *file;
    openFileW(&file, name);

    int i;
    for(i=0; i<array.size(); ++i)
    {
        int j;
        for(j=0;j<array[i].size(); ++j)
            fprintf(file, "%15.10E ", array[i][j]);
        fprintf(file, "\n");
    }
    fclose(file);
}

void 
allocArray(MyArray &array, int nimax, int njmax)
{
    array.resize(nimax);
    int i;
    for(i=0; i<nimax; ++i)
    {
        array[i].resize(njmax);
        int j;
        for(j=0;j<njmax; ++j)
            array[i][j]=0.0;
    }
}

void
fillArray(char *name, MyArray &array)
{
    FILE *file;
    openFileR(&file, name);
    fillArray(file, array);
    fclose(file);
}

void
convertFileName(const char *fileM, char *fileT)
{
    int i=0;
    while(fileM[i]!='.' && i<100)
    {
        fileT[i]=fileM[i];
        i++;
    }
    strcpy(&(fileT[i]),".txt");
}

bool
checkFileExist(char *name)
{
    FILE *file = fopen(name, "r+t");
    if(file) 
    {
        fclose(file);
        return true;
    }
    return false;
}

bool 
convertOneFile(char *name)
{
    char fileT[100];
    convertFileName(name, fileT);
    printf("converting %s to %s:", name, fileT);
    if(checkFileExist(name))
    {
        int nimax, njmax;
        getMaxIdx(name, &nimax, &njmax);
        printf(" size %d x %d\n",nimax, njmax);
        MyArray array;
        allocArray(array, nimax, njmax);
        fillArray(name, array);
        writeArray(fileT, array);
    }
    else
    {
        printf(" bad file - skipped!\n");
        return false;
    }
    return true;
}

int 
main(int argc, char *argv[])
{
    int n;
    for(n=1; n<argc; ++n)
    {
        convertOneFile(argv[n]);
    }

    return 0;
}

