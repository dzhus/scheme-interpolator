SCHEME := /usr/bin/racket
TESTFILE := tests.ss

.PHONY: check

check:
	@${SCHEME} ${TESTFILE} | sed -e 's/: */:/'
