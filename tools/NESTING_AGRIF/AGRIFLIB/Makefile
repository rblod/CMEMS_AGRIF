# Automatic Makefile

FCM_BLD_CFG = /Users/rblod/201809_Agrif/dev_r10077_Agrif/mk/conv.cfg
export FCM_VERBOSE ?= 1

export FCM_ROOTDIR := /Users/rblod/201809_Agrif/dev_r10077_Agrif/tools/NESTING_AGRIF/AGRIFLIB
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

export AR = ar
export ARFLAGS = rs
export CC = cc
export CC_COMPILE = -c
export CC_DEFINE = -D
export CC_INCLUDE = -I
export CC_OUTPUT = -o
export CFLAGS = -O0
export CPPKEYS = 
export DIFF3FLAGS = -E -m
export FC = f90
export FC_COMPILE = -c
export FC_DEFINE = -D
export FC_INCLUDE = -I
export FC_MODSEARCH = 
export FC_OUTPUT = -o
export FFLAGS = 
export FPPKEYS = 
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
vpath %.o $(FCM_OBJPATH)

FCM_BLD_TARGETS = libconvsrc.a fortran.o main.o conv

.PHONY : all

all : $(FCM_BLD_TARGETS)

$(FCM_DONEDIR)/FCM_CP.dummy:
	touch $@

CFLAGS__convsrc.flags: CFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc.flags: CPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

FFLAGS__convsrc.flags: FFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__convsrc.flags: FPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__convsrc.flags: LDFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

LD__convsrc.flags: LD.flags
	touch $(FCM_FLAGSDIR)/$@

convsrc.etc: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/run /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/Makefile $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_ETCDIR)/$@

OBJECTS__convsrc = diversliste.o subloopcreation.o utilagrif.o utilcharacter.o utilfile.o utilfortran.o utilliste.o utilnotgriddep.o workwithallocatelist.o workwithparameterlist.o workwithglobliste.o workwithlistdatavariable.o workwithlistmoduleinfile.o workwithlistofcoupled.o workwithlistofmodulebysubroutine.o workwithlistvarindoloop.o workwithvarofsubroutineliste.o writeinfile.o writedeclarations.o dependfile.o fortran.o toamr.o

libconvsrc.a: $(OBJECTS__convsrc)
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

export OBJECTS = $(OBJECTS__convsrc)

libfcm_default.a: $(OBJECTS)
	fcm_internal archive $@ $^

CFLAGS__convsrc__DiversListe.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__DiversListe.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

diversliste.done: diversliste.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

diversliste.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/DiversListe.c CFLAGS__convsrc__DiversListe.flags CPPKEYS__convsrc__DiversListe.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__SubLoopCreation.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__SubLoopCreation.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

subloopcreation.done: subloopcreation.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

subloopcreation.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/SubLoopCreation.c CFLAGS__convsrc__SubLoopCreation.flags CPPKEYS__convsrc__SubLoopCreation.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilAgrif.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilAgrif.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilagrif.done: utilagrif.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilagrif.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilAgrif.c CFLAGS__convsrc__UtilAgrif.flags CPPKEYS__convsrc__UtilAgrif.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilCharacter.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilCharacter.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilcharacter.done: utilcharacter.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilcharacter.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilCharacter.c CFLAGS__convsrc__UtilCharacter.flags CPPKEYS__convsrc__UtilCharacter.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilFile.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilFile.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilfile.done: utilfile.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilfile.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilFile.c CFLAGS__convsrc__UtilFile.flags CPPKEYS__convsrc__UtilFile.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilFortran.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilFortran.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilfortran.done: utilfortran.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilfortran.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilFortran.c CFLAGS__convsrc__UtilFortran.flags CPPKEYS__convsrc__UtilFortran.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilListe.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilListe.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilliste.done: utilliste.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilliste.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilListe.c CFLAGS__convsrc__UtilListe.flags CPPKEYS__convsrc__UtilListe.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__UtilNotGridDep.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__UtilNotGridDep.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

utilnotgriddep.done: utilnotgriddep.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

utilnotgriddep.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/UtilNotGridDep.c CFLAGS__convsrc__UtilNotGridDep.flags CPPKEYS__convsrc__UtilNotGridDep.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithAllocatelist.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithAllocatelist.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithallocatelist.done: workwithallocatelist.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithallocatelist.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithAllocatelist.c CFLAGS__convsrc__WorkWithAllocatelist.flags CPPKEYS__convsrc__WorkWithAllocatelist.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithParameterlist.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithParameterlist.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithparameterlist.done: workwithparameterlist.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithparameterlist.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithParameterlist.c CFLAGS__convsrc__WorkWithParameterlist.flags CPPKEYS__convsrc__WorkWithParameterlist.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithglobliste.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithglobliste.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithglobliste.done: workwithglobliste.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithglobliste.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithglobliste.c CFLAGS__convsrc__WorkWithglobliste.flags CPPKEYS__convsrc__WorkWithglobliste.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithlistdatavariable.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithlistdatavariable.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithlistdatavariable.done: workwithlistdatavariable.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithlistdatavariable.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithlistdatavariable.c CFLAGS__convsrc__WorkWithlistdatavariable.flags CPPKEYS__convsrc__WorkWithlistdatavariable.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithlistmoduleinfile.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithlistmoduleinfile.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithlistmoduleinfile.done: workwithlistmoduleinfile.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithlistmoduleinfile.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithlistmoduleinfile.c CFLAGS__convsrc__WorkWithlistmoduleinfile.flags CPPKEYS__convsrc__WorkWithlistmoduleinfile.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithlistofcoupled.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithlistofcoupled.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithlistofcoupled.done: workwithlistofcoupled.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithlistofcoupled.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithlistofcoupled.c CFLAGS__convsrc__WorkWithlistofcoupled.flags CPPKEYS__convsrc__WorkWithlistofcoupled.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithlistofmodulebysubroutine.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithlistofmodulebysubroutine.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithlistofmodulebysubroutine.done: workwithlistofmodulebysubroutine.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithlistofmodulebysubroutine.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithlistofmodulebysubroutine.c CFLAGS__convsrc__WorkWithlistofmodulebysubroutine.flags CPPKEYS__convsrc__WorkWithlistofmodulebysubroutine.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithlistvarindoloop.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithlistvarindoloop.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithlistvarindoloop.done: workwithlistvarindoloop.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithlistvarindoloop.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithlistvarindoloop.c CFLAGS__convsrc__WorkWithlistvarindoloop.flags CPPKEYS__convsrc__WorkWithlistvarindoloop.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WorkWithvarofsubroutineliste.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WorkWithvarofsubroutineliste.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

workwithvarofsubroutineliste.done: workwithvarofsubroutineliste.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

workwithvarofsubroutineliste.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WorkWithvarofsubroutineliste.c CFLAGS__convsrc__WorkWithvarofsubroutineliste.flags CPPKEYS__convsrc__WorkWithvarofsubroutineliste.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__WriteInFile.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__WriteInFile.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

writeinfile.done: writeinfile.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

writeinfile.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/WriteInFile.c CFLAGS__convsrc__WriteInFile.flags CPPKEYS__convsrc__WriteInFile.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__Writedeclarations.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__Writedeclarations.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

writedeclarations.done: writedeclarations.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

writedeclarations.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/Writedeclarations.c CFLAGS__convsrc__Writedeclarations.flags CPPKEYS__convsrc__Writedeclarations.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

decl.h: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/decl.h
	cp $< $(FCM_INCDIR)
	chmod u+w $(FCM_INCDIR)/$@

decl.h.idone: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/decl.h
	touch $(FCM_DONEDIR)/$@

CFLAGS__convsrc__dependfile.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__dependfile.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

dependfile.done: dependfile.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

dependfile.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/dependfile.c CFLAGS__convsrc__dependfile.flags CPPKEYS__convsrc__dependfile.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__fortran.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__fortran.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

fortran.done: fortran.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

fortran.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/fortran.c CFLAGS__convsrc__fortran.flags CPPKEYS__convsrc__fortran.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__main.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__main.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__convsrc__main.flags: LDFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

LD__convsrc__main.flags: LD__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

conv: main.o LD__convsrc__main.flags LDFLAGS__convsrc__main.flags decl.h.idone
	fcm_internal load:C convsrc $< $@

main.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/main.c CFLAGS__convsrc__main.flags CPPKEYS__convsrc__main.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

CFLAGS__convsrc__toamr.flags: CFLAGS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__convsrc__toamr.flags: CPPKEYS__convsrc.flags
	touch $(FCM_FLAGSDIR)/$@

toamr.done: toamr.o decl.h.idone
	touch $(FCM_DONEDIR)/$@

toamr.o: /Users/rblod/201809_Agrif/dev_r10077_Agrif/ext/AGRIF/LIB/toamr.c CFLAGS__convsrc__toamr.flags CPPKEYS__convsrc__toamr.flags decl.h
	fcm_internal compile:C convsrc $< $@ 1

# EOF
