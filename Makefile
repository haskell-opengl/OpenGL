# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2002/02/28 17:42:27 panne Exp $

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

SRC_HC_OPTS += -fglasgow-exts '-\#include <GL/glu.h>'

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to OpenGL.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------
# Per-module flags

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
