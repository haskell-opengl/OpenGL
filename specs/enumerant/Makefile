# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertEnumSpec
CLEAN_FILES += Main.hi test.stdout.tmp

SRC_HC_OPTS += -cpp

ghc_ge_504 = $(shell if (test $(GhcCanonVersion) -ge 504); then echo YES; else echo NO; fi)

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS += -package data -package text
endif

test: ConvertEnumSpec
	$(RM) test.stdout.tmp
	ConvertEnumSpec -v test.spec > test.stdout.tmp
	diff test.stdout test.stdout.tmp && echo "test passed"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
