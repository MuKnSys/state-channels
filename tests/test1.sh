#!/bin/bash

SC_HOME=`dirname $0`/..
cd "$SC_HOME" 2> /dev/null ; SC_HOME=`pwd` ; cd - > /dev/null

BIN=$SC_HOME/bin
SRC=$SC_HOME/src
TESTS=$SC_HOME/tests
#SNIPPETS=$SC_HOME/snippets
#SCORIES=$SC_HOME/scories
EXAMPLES=$SC_HOME/examples

cd $TESTS

#$BIN/scm $SNIPPETS/tab1.ss > OUT 2> /dev/null
#diff -q -U2 OUTTAB1 OUT

#$BIN/scm $SCORIES/persons0.ss > OUT 2> /dev/null
#diff -q -U2 OUTPERSONS0 OUT

#$BIN/scm $SCORIES/eg1.ss > OUT 2> /dev/null
#diff -q -U2 OUTEG1 OUT

$BIN/scm $EXAMPLES/mp1_1.ss > OUT 2> /dev/null
diff -q -U2 OUT1_1.txt OUT

$SRC/clish.ss mp0 mp0_1 > OUT 2> /dev/null
diff -q -U2 OUT0 OUT

#$SRC/clish.ss mp1 mp1_3 > OUT 2> /dev/null
#diff -q -U2 OUT1 OUT

#$SRC/clish.ss mp2 mp2_3 > OUT 2> /dev/null
#diff -q -U2 OUT2 OUT

rm -f $TESTS/OUT
