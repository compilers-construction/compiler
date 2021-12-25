#! /bin/bash
testfile=$1
echo testfile = $testfile

make -f ./Makefile $testfile
expected_file=$([ -f $testfile.expected ] && echo $testfile.expected || echo $testfile.scm)
echo expected_file = $expected_file

o1=`scheme -q < $expected_file`
o2=`./$testfile`
echo "(equal? '($o1) '($o2))" > test.scm

scheme -q < test.scm