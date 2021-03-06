######################################

######## MODIFIED MAKE FILE TO MAKE EDDY CHANNEL WORK IN BOREAS ##############

##ANNPATH = /home/Karu/WORKHERE/CODE_DOC_PAPER/CODE/ann_1.1.2
ANNPATH = /opt/ann_1.1.2

#HDF5_PATH = /home/Karu/WORKHERE/CODE_DOC_PAPER/CODE/hdf5-1.8.15-patch1-installdir
#HDF5_PATH = /opt/hdf5-1.8.16+docs
HDF5_PATH = /opt/hdf5-1.8.15-patch1

##LAPACK_PATH = /usr/local/LAPACK
LAPACK_PATH = /opt/lapack-3.5.0
#LAPACK_PATH = /opt/lapack-3.5.0

##BLAS_PATH = /usr/local/BLAS
BLAS_PATH = /opt/BLAS-3.6.0


#Compiler command

#F_COMP  = ftn
#F_COMP  = h5pfc
#CC_COMP = CC

#F_COMP  = /opt/mpich2-1.5_intel/bin/mpif90
#CC_COMP = /opt/mpich2-1.5_intel/bin/mpicc


F_COMP   = /opt/openmpi/bin/mpif90
CC_COMP  = /opt/openmpi/bin/mpicc
#F_COMP = mpif90
#CC_COMP = g++

#Libraries

ANNLIB = ANN
#ANNLIB = ANN_PGI
#ANNLIB = ANN_INTEL

#INCLUDE DIRECTORIES

#BLASDIRS   = -I/usr/local/BLAS
BLASDIRS    = -I/opt/BLAS-3.6.0
#LAPACKDIRS = -I/usr/local/LAPACK_KARU/SRC
#LAPACKDIRS  = -I/opt/lapack-3.6.1
LAPACKDIRS  = -I/opt/lapack-3.5.0/SRC

#C++ Section
CPPFLAGS = $(CC_COMP) -c $(OPT) -I$(ANNPATH)/include

#DEBUGGING OPTIONS
# DEBUG = -check all -warn all,nodec,interfaces -gen-interfaces -traceback -fpe0 -fp-stack-check
# DEBUG  =-debug -traceback          # use -debug to get debugging info from gdb
# DEBUG  =-p                         # -p lets you use profiling with gprof

# DEBUG = -warn all -warn nointerfaces


#CRAY Compiler
#(comf.f must be compiled with -O1)
#OPT = -O3
#OPT = -e D
#CFLAGS = $(F_COMP) $(OPT) -s real64 -N 132 $(DEBUG) $(EXTRA) $(PROFILE) -c
#LDFLAGS = $(F_COMP) $(EXTRA) -o

#PATHSCALE Compiler
#OPT = -O3
#EXTRA = -intrinsic=PGI

#CFLAGS = $(F_COMP) $(OPT) -r8 -extend-source $(DEBUG) $(EXTRA) $(PROFILE) -c
#LDFLAGS = $(F_COMP) -o

#GNU Compiler
#OPT = -O3
#DEBUG = -g -fbacktrace -fdump-core -fbounds-check -ffpe-trap=invalid,zero,overflow,underflow,denormal
#DEBUG = -g -fbacktrace -fdump-core -fbounds-check
#EXTRA = -fmax-stack-var-size=100000
#PROFILE = -pg
#CFLAGS = $(F_COMP) $(OPT) -fdefault-real-8 -ffixed-line-length-none $(DEBUG) $(EXTRA) $(PROFILE) -c
#LDFLAGS = $(F_COMP) -o

#PGI Compiler
#DEBUG = -g -C
#OPT = -fastsse -O3 -Mvect -Minline=levels:10 -tp amd64
#CFLAGS  = $(F_COMP) -r8 $(OPT) -Mextend $(DEBUG) -c 
#LDFLAGS = $(F_COMP) -r8 $(DEBUG) -o

#Intel
#DEBUG = -check all -debug all -g -heap-arrays 100000 -fpe0 -traceback
EXTRA = -heap-arrays 100000
#OPT   = -O3 HIGH OPTIMIZATION
OPT   = -O3
#OPT  = -O0 NO OPTIMIZATION


#CFLAGS  = $(F_COMP) -c -r8 -132 $(OPT) $(DEBUG)
CFLAGS  = $(F_COMP) -c -r8 -132 $(OPT) $(EXTRA) $(DEBUG) -I$(ANNPATH)/include -I$(HDF5_PATH)/include 
LDFLAGS = $(F_COMP) -r8 -132 $(OPT) $(DEBUG) -o 

#export prefix= /home/Karu/WORKHERE/CODE_DOC_PAPER/CODE/hdf5-1.8.15-patch1-installdir
export prefix=/home/sheel/Work/export

#CLIBS    = -I$(ANNPATH)/include -L$(ANNPATH)/lib -l$(ANNLIB) -pgcpplibs -lm
#CLIBS    = -I$(ANNPATH)/include -L$(ANNPATH)/lib -l$(ANNLIB) -lstdc++ -lm 
CLIBS    =  -L$(ANNPATH)/lib -l$(ANNLIB) -L$(HDF5_PATH)/lib -lhdf5_fortran  -lhdf5 -lstdc++ -lm -lz 

FLIBS    = -L/opt/lapack-3.5.0 -llapack -L/opt/BLAS-3.6.0 -lblas -lstdc++ -lm -lz
#FLIBS    = -L/usr/local/LAPACK_KARU -llapack -lblas -lstdc++ -lm -lz

HDF5LIBS =  -L$(HDF5_PATH)/lib -lhdf5_fortran.so -lstdc++ -lm -lz
#TECLIBS =/opt/tecplot10/lib/tecio64.a -lstdc++
TECLIBS  =/opt/tec360/lib/libtecio.a -lstdc++
#-mmacosx-version-min=10.4
EXEC    = Eddy.x
#WDIR    = $(TG_CLUSTER_SCRATCH)/newcode_2012_suboff/appended_suboff_new


WDIR    = ./run
#WDIR    =  /home/jose/WORK/SOURCE_EDDY_CYL/run

CMD     = $(WDIR)/$(EXEC)
##########################################################################
.SUFFIXES:  .F .f .h
.cpp.o:
	$(CPPFLAGS) $<  

.F.o: 
	$(CFLAGS) $<  

.f.o: 
	$(CFLAGS) $<  


OBJ =\
 modules.o\
 Eddy6.o\
 Planes.o\
 mpi_setup.o\
 boundary.o\
 direct.o\
 grid.o\
 info.o\
 inpall.o\
 Initialize.o\
 io.o\
 rt.o\
 matint.o\
 object.o\
 rhs.o\
 setup.o\
 timeindeg.o\
 timestep.o\
 refreshbc.o\
 numrec.o\
 comf.o\
 blktri.o\
 fftpack.o\
 genbun.o\
 gnbnaux.o\
 flowstat.o\
 vt.o\
 calcforce.o\
 triinter.o\
 momforc.o\
 interp.o\
 rbm.o\
 devel.o\
 stats.o\
 output.o\
 output_cart.o\
 pdc2dn.o\
 rhs_density.o\
 density.o\
 hdf5io.o\
 ANN.o

$(CMD):		$(OBJ)
		@echo Makefile: ... compiling $@
		$(LDFLAGS) $(@) $(OBJ) ${BLASDIRS} ${LAPACKDIRS} $(CLIBS) $(FLIBS) $(TECLIBS)

dep:
	makedepend -fMakefile.par *.F *.f *.f90 *.h

clean:
	rm *.o *.mod

##########################################################################

# DO NOT DELETE
