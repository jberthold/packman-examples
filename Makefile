

# can be set e.g. $>gmake EDEN=/path/to/my/eden/ghc 
ifeq ($(strip $(EDEN)),) 
  EDEN=./eden-exp
endif

# can be set e.g. $>gmake EDEN=/path/to/my/eden/ghc 
ifeq ($(strip $(HADDOCK)),) 
  HADDOCK=./eden-exp-haddock
endif

# can be set e.g. $>gmake WAYFLAGS="-parcp -eventlog -debug"
ifeq ($(strip $(WAYFLAGS)),)
   WAYFLAGS=
endif

OTHERFLAGS=-O2 -fno-warn-warnings-deprecations -rtsopts
FLAGS=${WAYFLAGS} ${OTHERFLAGS} $(EXTRA)
NOOUT= > /dev/null
RUNFLAGS=${TWOPE} ${EXTRARUN}

# can also set EXTRA flags for compilation, e.g. EXTRA="-dcore-lint"
# can also set EXTRARUN flags, e.g. EXTRARUN="+RTS -l -RTS"
# these flags go last in the command which compiles/runs

PROGRAMS=typePropagates testExceptions testTrySer
LIB=GHC/Packing

all	: compiler $(PROGRAMS)
.PHONY	: compiler $(PROGRAMS) clean help

help	:
	@echo Test programs for Serialisation 
	@echo 
	@echo Programs prepared to run using this Makefile are:
	@echo '$(PROGRAMS)'
	@echo 
	@echo You can set the following variables to influence the run:
	@echo 
	@echo 'EDEN:      compiler to use (default "./eden-exp")'
	@echo 'WAYFLAGS:  compilation way (default "-debug")'
	@echo 'EXTRA:     extra flags for compilation (come last on command line)'
	@echo 'EXTRARUN:  extra flags for running the program (come last)'
	@echo
	@echo 'Usage example:'
	@echo '#>make typePropagates EDEN=../ghc-7.7 WAYFLAGS="-parcp -eventlog" EXTRA=-dcore-lint EXTRARUN="+RTS -l -N3"'
	@echo
	@echo '(compile SimpleProcess with ../ghc-7.7, for copy way with event logging,'
	@echo 'run a lint check on core when compiling, run on 3 PEs using logging).'


clean	:
	@rm -f *.o *.hi
	@for PROG in ${PROGRAMS}; do rm -f $$PROG $$USER\=$$PROG; done
	@rm -f ${LIB}.hi ${LIB}.o
	@rm -rf doc/

compiler:
	$(EDEN) --version

doc:	${LIB}.hs
	${HADDOCK} --html -odoc ${LIB}.hs

%.o	: %.hs ${LIB}.o
	$(EDEN) $(FLAGS) $< --make

${LIB}.o	: ${LIB}.hs
	$(EDEN) $(FLAGS) $< --make

# testTrySer needs blackhole creation (or a hack...)
testTrySer: testTrySer.o ${LIB}.o
	$(EDEN) -parcp ${WAYFLAGS} ${OTHERFLAGS} ${EXTRA} -o $@ $@.hs --make

typePropagates: typePropagates.o ${LIB}.o
	$(EDEN) $(FLAGS) -o $@ $@.hs --make

testExceptions:	testExceptions.o ${LIB}.o
	$(EDEN) $(FLAGS) -o $@ $@.hs --make
