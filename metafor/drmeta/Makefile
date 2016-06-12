#
#	$Id: Makefile,v 1.3 1999/12/21 16:14:35 boman Exp $	
#
MAKE 	      = gmake

DEBUG         = -g 

CFLAGS	      = 
#-machine_code -source_listing

F77           = f77

CC	      = gcc

FFLAGS	      = 

CFLAGS	      =
#-assume nounderscore -names as_is -machine_code -V

DEST          = .

LD            = gcc

LDFLAGS       =

LIBOBJ        = lib.a

MAKEFILE      = makefile

SRCS          = $(wildcard *.c)

OBJS          =	$(patsubst %.c,%.o,$(wildcard *.c))

DEPEND        = makedepend $(CFLAGS)

PROGRAM       = drmeta 

LIBS	      = 
#-lm 

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS)
		@echo "Linking $(PROGRAM) ..."
		$(LD) $(LDFLAGS) $(DEBUG) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"
debug:
	$(MAKE) DEBUG=-g

new:;		@touch $(SRCS)
		@make -f $(MAKEFILE)

clean:;		@echo "Cleaning ..."
		@rm -f $(OBJS) core $(PROGRAM) *~ *.lis *.bak

arch:;		@echo "Archivage ..."
		@ar -rv $(LIBOBJ) $(OBJS)
		@make -f $(MAKEFILE) clean

depend:		$(SRCS)
		$(DEPEND) $(SRCS)

.cxx.o:
		cxx -c $(CFLAGS) $(DEBUG) $*.cxx
.f.o:
		$(F77) -c $(FFLAGS) $(DEBUG) $*.f
.cxx:
		@echo "Compile...";cxx $(LDFLAGS) $(CFLAGS) $(DEBUG) -o $@
.c.o:
		$(CC) -c $(CFLAGS) $(DEBUG) $*.c

.SUFFIXES:	.cxx .hxx .f .o .c


# DO NOT DELETE

drc.o: /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h
drc.o: /usr/include/gnu/stubs.h
drc.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
drc.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stdarg.h
drc.o: /usr/include/bits/types.h /usr/include/libio.h
drc.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
drc.o: /usr/include/strings.h dr.h
drf.o: /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h
drf.o: /usr/include/gnu/stubs.h
drf.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
drf.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stdarg.h
drf.o: /usr/include/bits/types.h /usr/include/libio.h
drf.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h dr.h
