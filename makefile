#Makefile for SACTOOLS programs
#
#--------------------------------------------------------------------
F90=gfortran
FFLAGS=-O4 
RM=/bin/rm -f
BINDIR=./bin                 


all : modules amp2sac mavg sac2xy sac2xyfill sachead sacsnr sacunused stacksac stacksacgc stacksacaz stalta xy2sac 

#Compile modules.
modules : mod_sac_io.f90
	$(F90) $(FFLAGS) -c mod_sac_io.f90

#Compile Source-code and link.
amp2sac : amp2sac.f90 modules
	$(F90) $(FFLAGS) amp2sac.f90 -o amp2sac ./mod_sac_io.o
	mv amp2sac $(BINDIR)

mavg : mavg.f90 modules
	$(F90) $(FFLAGS) mavg.f90 -o mavg ./mod_sac_io.o
	mv mavg $(BINDIR)

sac2xy : sac2xy.f90 modules
	$(F90) $(FFLAGS) sac2xy.f90 -o sac2xy ./mod_sac_io.o
	mv sac2xy $(BINDIR)

sac2xyfill : sac2xyfill.f90 modules
	$(F90) $(FFLAGS) sac2xyfill.f90 -o sac2xyfill ./mod_sac_io.o
	mv sac2xyfill $(BINDIR)

sachead : sachead.f90 modules
	$(F90) $(FFLAGS) sachead.f90 -o sachead ./mod_sac_io.o
	mv sachead $(BINDIR)

sacsnr : sacsnr.f90 modules
	$(F90) $(FFLAGS) sacsnr.f90 -o sacsnr ./mod_sac_io.o
	mv sacsnr $(BINDIR)

sacunused : sacunused.f90 modules
	$(F90) $(FFLAGS) sacunused.f90 -o sacunused ./mod_sac_io.o
	mv sacunused $(BINDIR)

stacksac : stacksac.f90 modules
	$(F90) $(FFLAGS) stacksac.f90 -o stacksac ./mod_sac_io.o
	mv stacksac $(BINDIR)

stacksacgc : stacksacgc.f90 modules
	$(F90) $(FFLAGS) stacksacgc.f90 -o stacksacgc ./mod_sac_io.o
	mv stacksacgc $(BINDIR)

stacksacaz : stacksacaz.f90 modules
	$(F90) $(FFLAGS) stacksacaz.f90 -o stacksacaz ./mod_sac_io.o
	mv stacksacaz $(BINDIR)

stalta : stalta.f90 modules
	$(F90) $(FFLAGS) stalta.f90 -o stalta ./mod_sac_io.o
	mv stalta $(BINDIR)

xy2sac : xy2sac.f90 modules
	$(F90) $(FFLAGS) xy2sac.f90 -o xy2sac ./mod_sac_io.o
	mv xy2sac $(BINDIR)

clean :
	  $(RM) mod_sac_io.o sac_i_o.mod


