AC_INIT([icluster config], version-0.1)

AC_PATH_PROG([GSL_CONFIG], [gsl-config])

GSL_CFLAGS=`${GSL_CONFIG} --cflags`
GSL_LIBS=`${GSL_CONFIG} --libs`

AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)
AC_OUTPUT(src/Makevars)
