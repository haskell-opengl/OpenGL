# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2002/07/08 17:44:26 panne Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = specs

ALL_DIRS = \
	Graphics/Rendering \
	Graphics/Rendering/OpenGL \
	Graphics/Rendering/OpenGL/GL \
	Graphics/Rendering/OpenGL/GLU

PACKAGE = OpenGL
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts '-\#include <GL/glu.h>'

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries (OpenGL package)"

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to OpenGL.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------
# Per-module flags

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
