# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2002/05/19 11:02:18 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertSpec
CLEAN_FILES += Main.hi
SRC_HC_OPTS += -package text

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
