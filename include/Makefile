# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(filter-out HsOpenGLConfig.h,$(wildcard *.h))

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

DIST_CLEAN_FILES += HsOpenGL.h HsOpenGLConfig.h

include $(TOP)/mk/target.mk
