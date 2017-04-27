
test:	test.mlb json.sml test.sml
	mlton $<
	./test.sh > test.log 2>&1
	tail -3 test.log

clean:
	rm -f test test.log


