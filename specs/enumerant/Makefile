# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2002/02/28 20:52:01 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

HS_PROG      = ConvertEnumSpec
CLEAN_FILES += Main.hi test.stdout.tmp
SRC_HC_OPTS += -package text

test: ConvertEnumSpec
	$(RM) test.stdout.tmp
	ConvertEnumSpec -v test.spec > test.stdout.tmp
	diff test.stdout test.stdout.tmp && echo "test passed"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
