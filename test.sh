#!/bin/bash

set -eu

pass=0
fail=0

for testfile in testfiles/test_parsing/*.json ; do

    output=$(./test "$testfile")
    base=$(basename $testfile)
    
    case $base in
	y_*) if [ -n "$output" ]
	     then pass=$(($pass + 1)); echo "--- PASS: $base"
	     else fail=$(($fail + 1)); echo "*** FAIL: $base"
	     fi ;;
	n_*) if [ -z "$output" ]
	     then pass=$(($pass + 1)); echo "--- PASS: $base"
	     else fail=$(($fail + 1)); echo "*** FAIL: $base"
	     fi ;;
	i_*) ;;
    esac
    
done

echo
echo "Passed: $pass"
echo "Failed: $fail"
