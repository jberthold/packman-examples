

# can be set e.g. $>gmake EDEN=/path/to/my/eden/ghc 
ifeq ($(strip $(EDEN)),) 
  EDEN=./eden-github
endif

# can be set e.g. $>gmake EDEN=/path/to/my/eden/ghc 
ifeq ($(strip $(HADDOCK)),) 
  HADDOCK=./eden-github-haddock
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

PROGRAMS=typePropagates testExceptions testTrySer Memoize-memocombinators CheckpointExample
LIB=GHC/Packing
CPLIB=Control/Monad/Checkpoint
OTHER=Data/IntTrie.hs Data/MemoCombinators.hs

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
	@echo
	@echo 'Usage example:'
	@echo '#>make testTrySer EDEN=../ghc-7.7 WAYFLAGS="-parcp -eventlog" EXTRA=-dcore-lint'
	@echo
	@echo '(compile testTrySer with ../ghc-7.7, for copy way with event logging,'
	@echo 'and activate checks on core when compiling'


clean	:
	@rm -f *.o *.hi
	@for PROG in ${PROGRAMS}; do rm -f $$PROG $$USER\=$$PROG; done
	@rm -f ${LIB}.hi ${LIB}.o ${CPLIB}.hi ${CPLIB}.o
	@rm -rf doc/
	@rm -f Data/*o Data/*hi
	@rm -f *_test2

compiler:
	$(EDEN) --version

doc:	${LIB}.hs
	${HADDOCK} --html -odoc ${LIB}.hs

%.o	: %.hs ${LIB}.o
	$(EDEN) $(FLAGS) $< --make

${LIB}.o	: ${LIB}.hs
	$(EDEN) $(FLAGS) $< -c

#${CPLIB}.o	: ${CPLIB}.hs
#	$(EDEN) $(FLAGS) $< -c

# testTrySer needs blackhole creation (or a hack...)
testTrySer: testTrySer.o ${LIB}.o
	$(EDEN) -parcp ${WAYFLAGS} ${OTHERFLAGS} ${EXTRA} -o $@ $@.hs --make

typePropagates: typePropagates.o ${LIB}.o
	$(EDEN) $(FLAGS) -o $@ $@.hs --make

testExceptions:	testExceptions.o ${LIB}.o
	$(EDEN) $(FLAGS) -o $@ $@.hs --make

Memoize-memocombinators: Memoize-memocombinators.o ${LIB}.o ${OTHER}
	$(EDEN) $(FLAGS) -o $@ $@.hs --make

# Checkpoint program in first version doznwok! Use only second version
CheckpointExample: CheckpointExample.o ${LIB}.o ${CPLIB}.o
	$(EDEN) $(FLAGS) -o $@ $@.hs --make

