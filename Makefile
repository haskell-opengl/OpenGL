# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2003/01/24 18:54:33 panne Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = include specs

ALL_DIRS = \
	Graphics/Rendering \
	Graphics/Rendering/OpenGL \
	Graphics/Rendering/OpenGL/GL \
	Graphics/Rendering/OpenGL/GLU

PACKAGE = OpenGL
PACKAGE_DEPS = base

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsOpenGL.h"' -cpp

# WinDoze DLL hell
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -DCALLCONV=stdcall
else
SRC_HC_OPTS += -DCALLCONV=ccall
endif

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries (OpenGL package)"

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to OpenGL.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
