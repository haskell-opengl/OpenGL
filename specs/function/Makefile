# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertSpec
CLEAN_FILES += Main.hi
SRC_HC_OPTS += -package parsec

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
