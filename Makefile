# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.9 2003/02/10 22:04:54 panne Exp $

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
# Hmmm, the following stuff for generating the Haskell files containing the
# GL/GLU enumeration constants is more than ugly, but the build system descends
# into SUBDIRS *after* the target in the current directory is updated.
#
# TODO: Is there a better solution for all these hacks?
# -----------------------------------------------------------------------------

# Make sure our enumeration .spec converter is up-to-date

ENUM_CONVERTER=specs/enumerant/ConvertEnumSpec$(exeext)

$(ENUM_CONVERTER): $(basename $(ENUM_CONVERTER)).hs
	  $(MAKE) -C $(dir $(ENUM_CONVERTER)) $(MFLAGS) $(notdir $(ENUM_CONVERTER))

# Generate the enumeration constants from the .spec files.

Graphics/Rendering/OpenGL/GL/Constants.incl: \
    $(dir $(ENUM_CONVERTER))enum.spec $(ENUM_CONVERTER)
	$(RM) $@
	./$(ENUM_CONVERTER) $< > $@

Graphics/Rendering/OpenGL/GLU/Constants.incl: \
    $(dir $(ENUM_CONVERTER))enumglu.spec $(ENUM_CONVERTER)
	$(RM) $@
	./$(ENUM_CONVERTER) $< > $@

boot:: Graphics/Rendering/OpenGL/GL/Constants.incl \
       Graphics/Rendering/OpenGL/GLU/Constants.incl

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
