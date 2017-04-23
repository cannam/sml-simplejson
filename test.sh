#!/bin/bash

set -eu

pass=0
fail=0

# Check that each of the parser test "pass" files (y_*.json) is
# successfully parsed, and that each of the "reject" files (n_*.json)
# is rejected. We also have a file merged-in.json which has a single
# array containing all of the contents of all the "pass" files; this
# should also be successfully parsed.

for testfile in testfiles/test_parsing/*.json testfiles/merged/merged-in.json ; do

    output=$(./test "$testfile")
    base=$(basename $testfile)
    
    case $base in
	y_*|merged*) if [ -n "$output" ]
	     then pass=$(($pass + 1))
		  echo "--- pass: $base"
	     else fail=$(($fail + 1))
		  echo "*** FAIL: $base"
	     fi ;;
	n_*) if [ -z "$output" ]
	     then pass=$(($pass + 1))
		  echo "--- pass: $base"
	     else fail=$(($fail + 1))
		  echo "Output was: $output"
		  echo "*** FAIL: $base"
	     fi ;;
	i_*) echo "- ignore: $base" ;;
    esac
    
done

# Process the merged file that has all of the "pass" file contents,
# and check that the output is something that can also be parsed

m_in=testfiles/merged/merged-in.json
m_out=testfiles/merged/merged-out.json
m_expected=testfiles/merged/merged-expected.json
m_collapsed=testfiles/merged/expected-collapsed.json

./test $m_in > $m_out

reread=$(./test $m_out)
if [ -n "$reread" ]
then pass=$(($pass + 1))
     echo "--- pass: merged file conversion can be re-read"
else fail=$(($fail + 1))
     echo "*** FAIL: merged file conversion cannot be re-read"
fi

# Now check that the output from the above matches an expected file
# that we generated and hand-checked earlier

cat $m_expected | perl -p -e 's/\n//gs' > $m_collapsed
echo >> $m_collapsed # to match actual output

if cmp -s $m_out $m_collapsed ; then
    pass=$(($pass + 1))
    echo "--- pass: merged file contents match"
else
    fail=$(($fail + 1))
    echo "*** FAIL: merged file contents differ"
    echo
    od -c $m_out > $m_out.od
    od -c $m_collapsed > $m_collapsed.od
    echo "Diff of char/octal dumps (output on left, expected on right):"
    sdiff -w156 $m_out.od $m_collapsed.od || true
    rm $m_out.od $m_collapsed.od
fi

echo
echo "Passed: $pass"
echo "Failed: $fail"
