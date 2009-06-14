# Extra autoconf macros for the Glasgow fptools
#
# To be a good autoconf citizen, names of local macros have prefixed with FP_ to
# ensure we don't clash with any pre-supplied autoconf ones.

# FP_ARG_OPENGL
# -------------
AC_DEFUN([FP_ARG_OPENGL],
[AC_ARG_ENABLE([opengl],
  [AC_HELP_STRING([--enable-opengl],
    [build a Haskell binding for OpenGL (GL/GLU). On Mac OS X, use
     --enable-opengl=x11 to use X11 instead of the "native" libraries.
     (default=autodetect)])],
  [enable_opengl=$enableval],
  [enable_opengl=yes])
])# FP_ARG_OPENGL


# FP_CHECK_QUARTZ_OPENGL
# ----------------------
AC_DEFUN([FP_CHECK_QUARTZ_OPENGL],
[AC_REQUIRE([FP_ARG_OPENGL])
AC_REQUIRE([AC_CANONICAL_TARGET])

use_quartz_opengl=no
if test x"$enable_opengl" = xyes; then
  case $target_os in
  darwin*)
    AC_DEFINE([USE_QUARTZ_OPENGL], [1],
              [Define to 1 if native OpenGL should be used on Mac OS X])
    use_quartz_opengl=yes
    ;;
  esac
fi

GLU_FRAMEWORKS=
GLUT_FRAMEWORKS=
if test x"$use_quartz_opengl" = xyes; then
  GLU_FRAMEWORKS=OpenGL
  GLUT_FRAMEWORKS=GLUT
fi
AC_SUBST([GLU_FRAMEWORKS])
AC_SUBST([GLUT_FRAMEWORKS])
])# FP_CHECK_QUARTZ_OPENGL


# FP_CHECK_WIN32
# --------------
# If Windows is the target platform (e.g. MinGW/MSYS or Cygwin with
# -mno-cygwin), the variable "fp_is_win32" is set to "yes", otherwise (e.g. *nix
# systems or plain Cygwin) it is set to "no".
AC_DEFUN([FP_CHECK_WIN32],
[AC_CACHE_CHECK([for Windows environment], [fp_cv_is_win32],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
#if !_WIN32
   syntax error;
#endif
])], [fp_cv_is_win32=yes], [fp_cv_is_win32=no])])
fp_is_win32="$fp_cv_is_win32"[]dnl
])# FP_CHECK_WIN32


# FP_PATH_XTRA
# ------------
# Same as AC_PATH_XTRA, but works even for broken Cygwins which try to include
# the non-existant <gl/mesa_wgl.h> header when -mno-cygwin is used.
AC_DEFUN([FP_PATH_XTRA],
[AC_REQUIRE([FP_CHECK_WIN32])
if test x"$fp_is_win32" = xyes; then
  no_x=yes
else
  AC_PATH_XTRA
fi
])# FP_PATH_XTRA


# FP_CHECK_GL_HELPER(LIBNAME, LIBS, INCLUDES, FUNCTION-BODY)
# ----------------------------------------------------------
# Try each library in LIBS to successfully link INCLUDES plus FUNCTION-BODY,
# setting LIBNAME_CFLAGS and LIBNAME_LIBS to the corresponding values. Sets
# no_LIBNAME to "yes" if no suitable library was found. (LIBNAME_CFLAGS0
# contains the value of LIBNAME_CFLAGS without CPPFLAGS, and LIBNAME_LIBS0
# contains the value of LIBNAME_LIBS without LDFLAGS, but these are only
# used internally.)
AC_DEFUN([FP_CHECK_GL_HELPER],
[AC_CACHE_CHECK([for $1 library], [fp_cv_check_$1_lib],
  [fp_cv_check_$1_lib="no"
  fp_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS ${$1_CFLAGS}"
  fp_save_LIBS="$LIBS"
  for fp_try_lib in $2; do
    # transform "-lfoo" to "foo.lib" when using cl
    if test x"$CC" = xcl; then
      fp_try_lib=`echo $fp_try_lib | sed -e 's/^-l//' -e 's/$/.lib/'`
    fi
    LIBS="$fp_try_lib ${$1_LIBS} $fp_save_LIBS"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([$3], [$4])], [fp_cv_check_$1_lib="$fp_try_lib ${$1_LIBS}"; break])
  done
  LIBS="$fp_save_LIBS"
  CPPFLAGS="$fp_save_CPPFLAGS"])

  if test x"$fp_cv_check_$1_lib" = xno; then
    no_$1=yes
    $1_CFLAGS=
    $1_LIBS=
  else
    $1_CFLAGS0="${$1_CFLAGS}"
    $1_CFLAGS="$CPPFLAGS ${$1_CFLAGS0}"
    $1_LIBS0="$fp_cv_check_$1_lib"
    $1_LIBS="$LDFLAGS ${$1_LIBS0}"
  fi
])# FP_CHECK_GL_HELPER


# FP_CHECK_GL
# -----------
AC_DEFUN([FP_CHECK_GL],
[AC_REQUIRE([FP_PATH_XTRA])
AC_REQUIRE([FP_CHECK_QUARTZ_OPENGL])
AC_REQUIRE([FP_CHECK_WIN32])

if test x"$use_quartz_opengl" = xno; then
  AC_CHECK_FUNC(atan,[fp_libm_not_needed=yes],[fp_libm_not_needed=dunno])
  if test x"$fp_libm_not_needed" = xdunno; then
     AC_CHECK_LIB([m], [atan], [GL_LIBS="-lm $GL_LIBS"])
  fi

  if test x"$no_x" != xyes; then
    test -n "$x_includes" && GL_CFLAGS="-I$x_includes $GL_CFLAGS"
    test -n "$x_libraries" && GL_LIBS="-L$x_libraries -lX11 $GL_LIBS"
  fi

  FP_CHECK_GL_HELPER([GL], [-lGL -lopengl32], [@%:@include <GL/gl.h>], [glEnd()])

  if test x"$fp_is_win32" = xyes; then
    # Ugly: To get wglGetProcAddress on Windows, we have to link with
    # opengl32.dll, too, even when we are using Cygwin with X11.
    case "$GL_LIBS" in
      *-lopengl32*|*opengl32.lib*) ;;
      *) fp_save_LIBS="$LIBS"
         LIBS="$LIBS -lopengl32"
         AC_LINK_IFELSE([AC_LANG_PROGRAM([[@%:@include <GL/gl.h>]], [[glEnd()]])],
           [GL_LIBS="$GL_LIBS -lopengl32"; GL_LIBS0="$GL_LIBS0 -lopengl32"])
         LIBS="$fp_save_LIBS"
         ;;
    esac
  fi
fi
AC_SUBST([GL_CFLAGS])
AC_SUBST([GL_LIBS])
])# FP_CHECK_GL


# FP_CHECK_GLU
# ------------
AC_DEFUN([FP_CHECK_GLU],
[AC_REQUIRE([FP_CHECK_GL])dnl
GLU_CFLAGS="$GL_CFLAGS0"
GLU_LIBS="$GL_LIBS0"

if test x"$use_quartz_opengl" = xno; then
  FP_CHECK_GL_HELPER([GLU], [-lglu32 -lGLU], [@%:@include <GL/glu.h>], [gluNewQuadric()])
fi

AC_SUBST([GLU_CFLAGS])
AC_SUBST([GLU_LIBS])
])# FP_CHECK_GLU


# FP_ARG_COMPILER
# -------------
AC_DEFUN([FP_ARG_COMPILER],
[AC_ARG_WITH([compiler],
  [AC_HELP_STRING([--with-compiler@<:@=HC@:>@],
    [use the given Haskell compiler (default=ghc)])],
  [with_compiler=$withval],
  [with_compiler=ghc])
])# FP_ARG_COMPILER
