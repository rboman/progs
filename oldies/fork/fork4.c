/* fork.c - fork a child process */
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

static int statv = 0;

void main()
{
    int *sharedmem = NULL;

    int fork_return;
    int count = 0;

    sharedmem = (int *)malloc(sizeof(int));
    *sharedmem = 0;
    printf("sharedmem=%x\n", sharedmem);

    /* getpid() returns the process id of this process. */
    printf("Process %d about to fork a child.\n", getpid());
    fork_return = fork();
    if (fork_return < 0)
    {
        printf("Unable to create child process, exiting.\n");
        exit(-1);
    }
    /* BOTH processes will do this! */
    system("ps");
    if (fork_return > 0)
    /* Then fork_return is the pid of the child process and I am
      the parent. Start printing a's. */
    {
        printf("Created child process %d.\n", fork_return);
        while (count++ < 800)
        {
            putchar('a');
            if (count % 80 == 0)
            {
                putchar('\n');
                sleep(1);
                printf("(child: count=%d)", count);
                statv++;
                (*sharedmem) = (*sharedmem) + 1;
                printf("sharedmem=%x\n", sharedmem);
            }
        }
    }
    else
    /* A 0 return tells me that I am the child. Print b's */
    {
        while (count++ < 800)
        {
            putchar('b');
            if (count % 80 == 0)
            {
                putchar('\n');
                sleep(1);
                printf("(parent: count=%d)", count);
                statv++;
                (*sharedmem) = (*sharedmem) + 1;
                printf("sharedmem=%x\n", sharedmem);
            }
        }
    }

    printf("statv=%d\n", statv);
    printf("sharedmem=%d\n", *sharedmem);
}
