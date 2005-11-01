# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertEnumSpec
CLEAN_FILES += Main.hi test.stdout.tmp

SRC_HC_OPTS += -cpp -package parsec -package mtl

test: ConvertEnumSpec
	$(RM) test.stdout.tmp
	./ConvertEnumSpec -v test.spec > test.stdout.tmp
	diff test.stdout test.stdout.tmp && echo "test passed"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
