#Makefile for SACTOOLS programs
#
#--------------------------------------------------------------------
F90=gfortran
FFLAGS=-O4 
RM=/bin/rm -f
BINDIR=./bin                 


all : modules sac2xy copy

#Compile modules.
modules : mod_sac_io.f90
	$(F90) $(FFLAGS) -c mod_sac_io.f90

#Compile Source-code and link.
sac2xy : sac2xy.f90 modules
	$(F90) $(FFLAGS) sac2xy.f90 -o sac2xy ./mod_sac_io.o


#Copy executable to appropriate directories.
#main : mod_sac_io.o sac2xy
#	cp sac2xy $(BINDIR)

copy :
	cp sac2xy ${BINDIR}

clean :
	  $(RM) sac2xy mod_sac_io.o sac_i_o.mod

