AC_INIT([Haskell RTS-serialisation support module ], [0.0.0.0], [jb.diku@gmail.com], [rts-serialisation])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([rts-serialisation.cabal])

# figure out GHC to use
AC_ARG_WITH([hc],
            [AC_HELP_STRING([--with-hc=ARG],
	                    [Use path ARG for GHC (we only support GHC!)])],
	    [WithGhc="$withval"],
            [if test "$HC" = ""; then
	       AC_PATH_PROG([GHC], [ghc])
	     fi
	     WithGhc="$GHC"])

AC_SUBST([WithGhc])

# we require it to be really GHC
AS_IF([test "$WithGhc" == ""],
      AC_MSG_ERROR([GHC is required for this library.]))

# GHC should be callable
AS_IF([$WithGhc --version],[],
      AC_MSG_ERROR([Your GHC does not work!]))

AC_MSG_CHECKING([for serialisation support in GHC runtime system])
# create Haskell which imports primitives
rm -f conftest.*; 
cat > conftest.hs << EOF
{-#  OPTIONS_GHC -XMagicHash #-}
module Serialisationtest where
import GHC.Base(serialize#, deserialize#, trySerialize# )
test s = trySerialize# s
EOF
#try to compile this, using WithGhc:
AS_IF( $WithGhc -c conftest.hs 2>&AS_MESSAGE_LOG_FD ,
      [AC_MSG_RESULT([OK])],
      [AC_MSG_RESULT([not found]); 
       AC_MSG_ERROR([Your GHC does not support serialisation primitives required by this package.])]
)

