# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(GL_BUILD_PACKAGE)" "no"

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Graphics/Rendering \
	Graphics/Rendering/OpenGL \
	Graphics/Rendering/OpenGL/GL \
	Graphics/Rendering/OpenGL/GL/PixelRectangles \
	Graphics/Rendering/OpenGL/GL/Texturing \
	Graphics/Rendering/OpenGL/GLU

PACKAGE_DEPS = base

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsOpenGL.h"' -cpp \
	       -funbox-strict-fields $(GLU_CFLAGS)
SRC_CC_OPTS += $(GLU_CFLAGS)

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

PACKAGE_CPP_OPTS += -DMAINTAINER=$(MAINTAINER)

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries ($(PACKAGE) package)"

# -----------------------------------------------------------------------------

STUBOBJS += \
   Graphics/Rendering/OpenGL/GLU/NURBS_stub.$(way_)o \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.$(way_)o \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Graphics/Rendering/OpenGL/GLU/NURBS_stub.[ch] \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.[ch] \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.[ch]

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

endif

include $(TOP)/mk/target.mk
