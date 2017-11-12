#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    int status;
    int pid;
    char *prog_argv[4];
    
    /* 
     * Build argument list
     */
    
    prog_argv[0] = "ls";
    prog_argv[1] = "-l";
    prog_argv[2] = ".";
    prog_argv[3] = NULL;
    
    /*
     * Create a process space for the ls  
     */
    if ((pid=fork()) < 0)
    {
        perror ("Fork failed");
        exit(errno);
    }
    
    printf("child pid = %d (my pid :getpid=%d)\n",pid,getpid());

    if (!pid)
    {
        /* This is the child, so execute the ls */ 
        printf("on entre dans execvp()...\n");
        execvp (prog_argv[0], prog_argv);
        printf("on sort de execvp()...\n"); /* jamais */
    }
    
    if (pid)
    {
        /* 
         * We're in the parent; let's wait for the child to finish
         */
        printf("on attend...\n"); 
        waitpid (pid, NULL, 0);
        printf("c'est fini...\n");
    }

    printf("fin du prog...\n");
}

