# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2002/05/31 12:28:19 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertEnumSpec
CLEAN_FILES += Main.hi test.stdout.tmp

test: ConvertEnumSpec
	$(RM) test.stdout.tmp
	ConvertEnumSpec -v test.spec > test.stdout.tmp
	diff test.stdout test.stdout.tmp && echo "test passed"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
