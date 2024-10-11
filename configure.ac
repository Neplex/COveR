AC_INIT([COveR],[0.2])

# Checks for common programs using default macros
AC_PROG_CC

## Use gsl-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: gsl-config(1)
AC_PATH_PROG([GSL_CONFIG], [gsl-config])
## If gsl-config was found, let's use it
if test "${GSL_CONFIG}" != ""; then
    # Use gsl-config for header and linker arguments
    GSL_CFLAGS=`${GSL_CONFIG} --cflags`
    GSL_LIBS=`${GSL_CONFIG} --libs`
else
    AC_MSG_ERROR([gsl-config not found, is GSL installed?])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
# TODO: GSL on CRAN
GSL_CFLAGS=""
GSL_LIBS=""

AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT