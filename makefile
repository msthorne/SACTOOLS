#Makefile for SACTOOLS programs
#
#--------------------------------------------------------------------
F90=gfortran
FFLAGS=-O4 
RM=/bin/rm -f
BINDIR=./bin                 


all : modules sac2xy xy2sac 

#Compile modules.
modules : mod_sac_io.f90
	$(F90) $(FFLAGS) -c mod_sac_io.f90

#Compile Source-code and link.
sac2xy : sac2xy.f90 modules
	$(F90) $(FFLAGS) sac2xy.f90 -o sac2xy ./mod_sac_io.o
	mv sac2xy $(BINDIR)

xy2sac : xy2sac.f90 modules
	$(F90) $(FFLAGS) xy2sac.f90 -o xy2sac ./mod_sac_io.o
	mv xy2sac $(BINDIR)


#Copy executable to appropriate directories.
#main : mod_sac_io.o sac2xy
#	cp sac2xy $(BINDIR)

clean :
	  $(RM) mod_sac_io.o sac_i_o.mod


