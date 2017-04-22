
test:	test.mlb test.deps
	mlton $<
	./test.sh > test.log 2>&1
	tail -3 test.log

SCRIPTS	:= ../sml-buildscripts

test.deps:	test.mlb
	${SCRIPTS}/mlb-dependencies $< > $@

-include *.deps

