# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

MKDEPENDHS   = $(GHC_INPLACE)
HS_PROG      = ConvertEnumSpec
CLEAN_FILES += Main.hi test.stdout.tmp

SRC_HC_OPTS += -cpp -package parsec -package mtl

TMP_FILE     = test.stdout.tmp

test: ConvertEnumSpec
	@$(RM) $(TMP_FILE)
	@./ConvertEnumSpec -v test.spec > $(TMP_FILE)
	@if diff test.stdout $(TMP_FILE) ; then \
	  echo "test passed" ; \
	  $(RM) $(TMP_FILE) ; \
	else \
	  echo "test failed" ; \
	fi

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
