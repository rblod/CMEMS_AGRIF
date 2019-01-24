# Automatic Makefile

FCM_BLD_CFG = /Users/rblod/201809_Agrif/dev_r10077_Agrif/mk/bld_preproagr_tools.cfg
export FCM_VERBOSE ?= 1

export FCM_ROOTDIR := /Users/rblod/201809_Agrif/dev_r10077_Agrif/tools/NESTING_AGRIF/NEMOFILES
export FCM_CACHEDIR := $(FCM_ROOTDIR)/.cache/.bld
export FCM_CFGDIR := $(FCM_ROOTDIR)/cfg
export FCM_SRCDIR := $(FCM_ROOTDIR)/src
export FCM_BINDIR := $(FCM_ROOTDIR)/bin
export FCM_ETCDIR := $(FCM_ROOTDIR)/etc
export FCM_LIBDIR := $(FCM_ROOTDIR)/lib
export FCM_TMPDIR := $(FCM_ROOTDIR)/tmp
export FCM_DONEDIR := $(FCM_ROOTDIR)/done
export FCM_FLAGSDIR := $(FCM_ROOTDIR)/flags
export FCM_INCDIR := $(FCM_ROOTDIR)/inc
export FCM_PPSRCDIR := $(FCM_ROOTDIR)/ppsrc
export FCM_OBJDIR := $(FCM_ROOTDIR)/obj
export FCM_ROOTPATH := $(FCM_ROOTDIR)
export FCM_CACHEPATH := $(FCM_CACHEDIR)
export FCM_CFGPATH := $(FCM_CFGDIR)
export FCM_SRCPATH := $(FCM_SRCDIR)
export FCM_BINPATH := $(FCM_BINDIR)
export FCM_ETCPATH := $(FCM_ETCDIR)
export FCM_LIBPATH := $(FCM_LIBDIR)
export FCM_TMPPATH := $(FCM_TMPDIR)
export FCM_DONEPATH := $(FCM_DONEDIR)
export FCM_FLAGSPATH := $(FCM_FLAGSDIR)
export FCM_INCPATH := $(FCM_INCDIR)
export FCM_PPSRCPATH := $(FCM_PPSRCDIR)
export FCM_OBJPATH := $(FCM_OBJDIR)

export PERL5LIB := /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/FCM/bin/../lib:$(PERL5LIB)

export AR = 
export ARFLAGS = 
export CC = cc
export CC_COMPILE = -c
export CC_DEFINE = -D
export CC_INCLUDE = -I
export CC_OUTPUT = -o
export CFLAGS = 
export CPPKEYS = 
export DIFF3FLAGS = -E -m
export FC = /Users/rblod/201809_Agrif/dev_r10077_Agrif/mk/agrifpp_2.sh
export FC_COMPILE = 
export FC_DEFINE = -D
export FC_INCLUDE = 
export FC_MODSEARCH = 
export FC_OUTPUT = 
export FFLAGS = 
export FPPKEYS = key_mpp_mpi key_nosignedzero key_agrif
export LD = 
export LDFLAGS = 
export LD_LIBLINK = -l
export LD_LIBSEARCH = -L
export LD_OUTPUT = -o
export SHELL = /bin/sh

vpath % $(FCM_BINPATH)
vpath %.done $(FCM_DONEPATH)
vpath %.idone $(FCM_DONEPATH)
vpath %.etc $(FCM_ETCPATH)
vpath %.flags $(FCM_FLAGSPATH)
vpath % $(FCM_INCPATH)
vpath %.a $(FCM_LIBPATH)
vpath %.f90 $(FCM_OBJPATH)

FCM_BLD_TARGETS = agrif_user.f90 agrif2model.f90 create_coordinate.f90

.PHONY : all

all : $(FCM_BLD_TARGETS)

$(FCM_DONEDIR)/FCM_CP.dummy:
	touch $@

CFLAGS__nemo.flags: CFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__nemo.flags: CPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

FFLAGS__nemo.flags: FFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo.flags: FPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo.flags: LDFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo.flags: LD.flags
	touch $(FCM_FLAGSDIR)/$@

OBJECTS__nemo = agrif2model.f90 agrif_user.f90 user_allocate.f90 io_netcdf.f90 par_kind.f90 par_oce.f90 variables.f90

libnemo.a: $(OBJECTS__nemo)
	fcm_internal archive $@ $^

CC.flags:
	touch $(FCM_FLAGSDIR)/$@

CFLAGS.flags: CC.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS.flags:
	touch $(FCM_FLAGSDIR)/$@

FC.flags:
	touch $(FCM_FLAGSDIR)/$@

FFLAGS.flags: FC.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS.flags:
	touch $(FCM_FLAGSDIR)/$@

LD.flags:
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS.flags:
	touch $(FCM_FLAGSDIR)/$@

export OBJECTS = $(OBJECTS__nemo)

libfcm_default.a: $(OBJECTS)
	fcm_internal archive $@ $^

FFLAGS__nemo__agrif2model.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif2model.done: agrif2model.f90
	touch $(FCM_DONEDIR)/$@

agrif2model.f90: /Users/rblod/201809_Agrif/dev_r10077_Agrif/tools/NESTING_AGRIF/src/agrif2model.F90 FFLAGS__nemo__agrif2model.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_coordinates.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_coordinates.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_coordinates.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_coordinates.exe: create_coordinates.f90 LD__nemo__agrif_create_coordinates.flags LDFLAGS__nemo__agrif_create_coordinates.flags io_netcdf.done par_oce.done user_allocate.done variables.done
	fcm_internal load:F nemo $< $@

create_coordinates.f90: $(FCM_PPSRCDIR)/nemo/agrif_create_coordinates.f90 FFLAGS__nemo__agrif_create_coordinates.flags io_netcdf.f90 par_oce.f90 user_allocate.f90 variables.f90
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_user.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_user.done: agrif_user.f90
	touch $(FCM_DONEDIR)/$@

agrif_user.f90: $(FCM_PPSRCDIR)/nemo/agrif_user.f90 FFLAGS__nemo__agrif_user.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__allocate_arrays.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

user_allocate.done: user_allocate.f90 variables.done
	touch $(FCM_DONEDIR)/$@

user_allocate.f90: $(FCM_PPSRCDIR)/nemo/allocate_arrays.f90 FFLAGS__nemo__allocate_arrays.flags variables.f90
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__create_coordinate.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__create_coordinate.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__create_coordinate.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

create_coordinate.exe: create_coordinate.f90 LD__nemo__create_coordinate.flags LDFLAGS__nemo__create_coordinate.flags par_oce.done
	fcm_internal load:F nemo $< $@

create_coordinate.f90: $(FCM_PPSRCDIR)/nemo/create_coordinate.f90 FFLAGS__nemo__create_coordinate.flags par_oce.f90
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__io_netcdf.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

io_netcdf.done: io_netcdf.f90
	touch $(FCM_DONEDIR)/$@

io_netcdf.f90: $(FCM_PPSRCDIR)/nemo/io_netcdf.f90 FFLAGS__nemo__io_netcdf.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__par_kind.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

par_kind.done: par_kind.f90
	touch $(FCM_DONEDIR)/$@

par_kind.f90: $(FCM_PPSRCDIR)/nemo/par_kind.f90 FFLAGS__nemo__par_kind.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__par_oce.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

par_oce.done: par_oce.f90
	touch $(FCM_DONEDIR)/$@

par_oce.f90: $(FCM_PPSRCDIR)/nemo/par_oce.f90 FFLAGS__nemo__par_oce.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__variables.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

variables.done: variables.f90
	touch $(FCM_DONEDIR)/$@

variables.f90: $(FCM_PPSRCDIR)/nemo/variables.f90 FFLAGS__nemo__variables.flags
	fcm_internal compile:F nemo $< $@

# EOF
