#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(1.0.0)
AC_INIT(eimp, 1.0.0, [], [])

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
# AC_ERLANG_SUBST_ROOT_DIR
# AC_ERLANG_SUBST_LIB_DIR

AC_CHECK_HEADERS([gd.h], [

  AC_SEARCH_LIBS([gdImageScale], [gd], [
    gd=-DHAVE_GD
    AC_CHECK_HEADERS([jpeglib.h], [
      AC_SEARCH_LIBS([gdImageCreateFromJpegPtr, gdImageJpegPtr], [gd], [
        AC_SEARCH_LIBS([jpeg_read_header], [jpeg], [jpeg=-DHAVE_JPEG], [
          AC_MSG_WARN([libjpeg library was not found])])],
      [AC_MSG_WARN([libgd library is not compiled with JPEG support])])],
    [AC_MSG_WARN([libjpeg header file jpeglib.h was not found])])

    AC_CHECK_HEADERS([png.h], [
      AC_SEARCH_LIBS([gdImageCreateFromPngPtr, gdImagePngPtr], [gd], [
        AC_SEARCH_LIBS([png_sig_cmp], [png], [png=-DHAVE_PNG], [
          AC_MSG_WARN([libpng library was not found])])],
      [AC_MSG_WARN([libgd library is not compiled with PNG support])])],
    [AC_MSG_WARN([libnpg header file png.h was not found])])

    AC_CHECK_HEADERS([webp/decode.h], [
      AC_SEARCH_LIBS([gdImageCreateFromWebpPtr, gdImageWebpPtr], [gd], [
        AC_SEARCH_LIBS([WebPGetInfo], [webp], [webp=-DHAVE_WEBP], [
          AC_MSG_WARN([libwebp library was not found])])],
      [AC_MSG_WARN([libgd library is not compiled with WebP support])])],
    [AC_MSG_WARN([libwebp header file webp/decode.h was not found])])
  ], [AC_MSG_WARN([libgd header file gd.h was not found])])

], [AC_MSG_WARN([libgd header file gd.h was not found])])

AC_ARG_ENABLE(gcov,
[AC_HELP_STRING([--enable-gcov], [compile with gcov enabled (default: no)])],
[case "${enableval}" in
  yes) gcov=true ;;
  no)  gcov=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-gcov) ;;
esac],[gcov=false])

AC_SUBST(gcov)
AC_SUBST(webp)
AC_SUBST(gd)
AC_SUBST(jpeg)
AC_SUBST(png)

AC_CONFIG_FILES([vars.config])

AC_OUTPUT
