SCHEME := /usr/bin/mzscheme
TESTFILE := tests.ss

.PHONY: check

check:
	@${SCHEME} ${TESTFILE} | sed -e 's/: */:/'
