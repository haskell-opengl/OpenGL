# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Graphics/Rendering \
	Graphics/Rendering/OpenGL \
	Graphics/Rendering/OpenGL/GL \
	Graphics/Rendering/OpenGL/GLU

PACKAGE = OpenGL
PACKAGE_DEPS = base

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsOpenGL.h"' -cpp $(GL_CFLAGS)
SRC_CC_OPTS += $(GL_CFLAGS)

# WinDoze DLL hell
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -DCALLCONV=stdcall
SRC_HC_OPTS := $(subst -mno-cygwin,,$(SRC_HC_OPTS))
else
SRC_HC_OPTS += -DCALLCONV=ccall
endif

# Needed for at least Graphics/Rendering/OpenGL/GL/QueryUtils.p_o
ifeq "$(hppa_TARGET_ARCH)" "1"
SRC_HC_OPTS += -optc-mbig-switch
endif

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries ($(PACKAGE) package)"

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to package.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------

STUBOBJS += \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.$(way_)o \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.[ch] \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.[ch]

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
