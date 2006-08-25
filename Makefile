# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(findstring clean, $(MAKECMDGOALS))" ""
# if we're cleaning, then config.mk might have been cleaned already
GL_BUILD_PACKAGE=yes
PACKAGE=OpenGL
endif

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

package.conf.inplace \
package.conf.installed \
OpenGL/Graphics/Rendering/OpenGL/GL/BasicTypes.$(way_)o : include/HsOpenGLConfig.h

Graphics/Rendering/OpenGL/GL/CoordTrans.$(way_)o \
Graphics/Rendering/OpenGL/GL/Evaluators.$(way_)o \
Graphics/Rendering/OpenGL/GL/PixelRectangles/PixelMap.$(way_)o \
Graphics/Rendering/OpenGL/GL/RasterPos.$(way_)o \
Graphics/Rendering/OpenGL/GL/Rectangles.$(way_)o \
Graphics/Rendering/OpenGL/GL/VertexSpec.$(way_)o : include/HsOpenGLConfig.h include/HsOpenGLTypes.h

Graphics/Rendering/OpenGL/GL/BeginEnd.$(way_)o \
Graphics/Rendering/OpenGL/GL/BufferObjects.$(way_)o \
Graphics/Rendering/OpenGL/GL/CoordTrans.$(way_)o \
Graphics/Rendering/OpenGL/GL/PerFragment.$(way_)o \
Graphics/Rendering/OpenGL/GL/PixelRectangles/ColorTable.$(way_)o \
Graphics/Rendering/OpenGL/GL/PixelRectangles/Convolution.$(way_)o \
Graphics/Rendering/OpenGL/GL/PixelRectangles/Histogram.$(way_)o \
Graphics/Rendering/OpenGL/GL/PixelRectangles/Minmax.$(way_)o \
Graphics/Rendering/OpenGL/GL/PointParameter.$(way_)o \
Graphics/Rendering/OpenGL/GL/RasterPos.$(way_)o \
Graphics/Rendering/OpenGL/GL/Rectangles.$(way_)o \
Graphics/Rendering/OpenGL/GL/Texturing/Specification.$(way_)o \
Graphics/Rendering/OpenGL/GL/VertexArrays.$(way_)o \
Graphics/Rendering/OpenGL/GL/VertexSpec.$(way_)o \
Graphics/Rendering/OpenGL/GLU/Matrix.$(way_)o \ : include/HsOpenGLExt.h

# -----------------------------------------------------------------------------

STUBOBJS += \
   Graphics/Rendering/OpenGL/GLU/NURBS_stub.$(way_)o \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.$(way_)o \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Graphics/Rendering/OpenGL/GLU/NURBS_stub.[ch] \
   Graphics/Rendering/OpenGL/GLU/Quadrics_stub.[ch] \
   Graphics/Rendering/OpenGL/GLU/Tessellation_stub.[ch]

endif

EXCLUDED_SRCS += Setup.hs

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += OpenGL.buildinfo config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
