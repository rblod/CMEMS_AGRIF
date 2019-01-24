SHELL = /bin/bash
MPI = NO

include make.inc

PROGS = agrif_create_bathy agrif_create_coordinates
SFMAKEDEPEND := ./sfmakedepend
DIR_LIB=lib
DIR_FILES=src
DIR_PPSRC=NEMOFILES
DIR_PPSRC_2=ppsrc
DIR_DOC=doc
DIR_OBJS=obj
DIR_LIB=lib

FILENAMES =$(notdir $(basename $(wildcard src/*.F90))) $(notdir $(basename $(wildcard src/*.F))) $(notdir $(basename $(wildcard src/*.c))) $(notdir $(basename $(wildcard src/*.f))) $(notdir $(basename $(wildcard src/*.f90)))
FILES=src/*
FILENAMES_CREATE_COORDINATES = agrif_create_coordinates agrif_profiles agrif_readwrite par_oce io_netcdf variables agrif_user
OBJS_CREATE_COORDINATES=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES_CREATE_COORDINATES)))
FILENAMES_CREATE_BATHY = agrif_create_bathy agrif_profiles agrif_readwrite par_oce io_netcdf variables agrif_user
OBJS_CREATE_BATHY=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES_CREATE_BATHY)))
OBJS_AGRIF=obj/agrif2model.o


all: depend conv prepagrif AGRIF_LIB $(OBJS_CREATE_COORDINATES) $(PROGS)
		@echo
		@echo ===================================================
		@echo   Nesting is ok
		@echo ===================================================
		@echo


agrif_create_coordinates: $(DIR_LIB)/libnesting.a $(DIR_FILES)/agrif_create_coordinates.f90 $(OBJS_CREATE_COORDINATES) $(OBJS_AGRIF)
		$(FC) $(LINKFLAGS) $(FFLAGS) -o agrif_create_coordinates $(OBJS_CREATE_COORDINATES) $(OBJS_AGRIF) $(LIBS) -I$(DIR_OBJS) $(FFLAGS_SPEC) -I../../ext/AGRIF
        
agrif_create_bathy: $(DIR_LIB)/libnesting.a $(DIR_FILES)/agrif_create_bathy.f90 $(OBJS_CREATE_BATHY) $(OBJS_AGRIF)
		$(FC) $(LINKFLAGS) $(FFLAGS) -o agrif_create_bathy $(OBJS_CREATE_BATHY) $(OBJS_AGRIF) $(LIBS) -I$(DIR_OBJS) $(FFLAGS_SPEC) -I../../ext/AGRIF

prepagrif:
		@echo
		@echo ===================================================
		@echo   Create working directories for AGRIF ...
		@echo ===================================================
		mkdir -p obj
		mkdir -p $(DIR_PPSRC_2)
		mkdir -p $(DIR_LIB)
		mkdir -p $(DIR_OBJS)
		mkdir -p $(DIR_PPSRC)
		mkdir -p $(DIR_PPSRC)/AGRIF_MODELFILES
		mkdir -p $(DIR_PPSRC)/AGRIF_INC
		\cp ../../ext/AGRIF/LIB/conv $(DIR_PPSRC)
		\cp ../../ext/AGRIF/agrif_oce.in $(DIR_PPSRC)
		@echo

conv:
		$(MAKE) -C ../../ext/AGRIF/LIB

AGRIF_LIB: 
		$(MAKE) -C ../../ext/AGRIF FC=$(FC) CPP=$(CPP) FFLAGS="$(FFLAGS)"

$(DIR_LIB)/libnesting.a: $(OBJS)
		@echo
		@echo ===================================================
		@echo   Compile Nesting library...
		@echo ===================================================
		$(AR) -r $@ $(OBJS)
		ranlib $@
		@echo



$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F90 Makefile  MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_PPSRC)/$(*F).f90
			@(cd NEMOFILES ; ./conv ./agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90)
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

obj/agrif2model.o:src_agrif/agrif2model.F90 Makefile  MODEL.CPP 
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS) src_agrif/agrif2model.F90 > $(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 Makefile MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).f90 > $(DIR_PPSRC)/$(*F).f90
			@(cd NEMOFILES ; ./conv agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90)
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F Makefile MODEL.CPP
#		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F > $(DIR_PPSRC)/$(*F).f
#		$(FC) $(FFLAGS) $(F77FLG) -c $(DIR_PPSRC)/$(*F).f -o $(DIR_OBJS)/$(*F).o

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 Makefile MODEL.CPP
#		$(FC) $(FFLAGS) -c $(DIR_PPSRC)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o

#$(DIR_OBJS)/par_oce.o: $(DIR_FILES)/par_oce.f90
#		echo $(OBJS) $(DIR_FILES)/

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.c Makefile MODEL.CPP
#	$(CC) -c $(CFLAGS) -c $(DIR_FILES)/$(*F).c -o $(DIR_OBJS)/$(*F).o

#--------------------------------------------------------------------------
#  Target to create dependeanies.
#--------------------------------------------------------------------------

MakeDepend: $(DIR_FILES)
		$(SFMAKEDEPEND) --silent --fext=f90 --fext=f --fext=F90 --fext=F --fext=c --file=- --moddir=$(DIR_OBJS) --objdir=$(DIR_OBJS)  $(FILES) > MakeDepend

depend: $(DIR_FILES)
		@echo
		@echo ===================================================
		@echo   Create dependancies for Nesting ...
		@echo ===================================================
		$(SFMAKEDEPEND) --fext=f90 --fext=f  --silent --fext=F90 --fext=F --fext=c --file=- --moddir=$(DIR_OBJS) --objdir=$(DIR_OBJS)  $(FILES) > MakeDepend

#ifneq "$(MAKECMDGOALS)" "clean"
-include ./MakeDepend
#endif

#--------------------------------------------------------------------------
#  Target to clean
#--------------------------------------------------------------------------

clean:
		@echo
		@echo ===================================================
		@echo   Clean ...
		@echo ===================================================
		$(RM) -rf $(DIR_PPSRC) $(DIR_PPSRC_2) $(DIR_LIB) $(DIR_OBJS) $(PROGS) 
	
# DO NOT DELETE THIS LINE - used by make depend	
