#!/usr/bin/env bash

SC_HOME=`dirname $0`/..
cd "$SC_HOME" 2> /dev/null ; SC_HOME=`pwd` ; cd - > /dev/null

BIN=$SC_HOME/bin
SRC=$SC_HOME/src
TESTS=$SC_HOME/tests
#SNIPPETS=$SC_HOME/snippets
#SCORIES=$SC_HOME/scories
EXAMPLES=$SC_HOME/examples

#$BIN/scm $SNIPPETS/tab1.ss > OUT 2> /dev/null
#diff -q -U2 OUTTAB1 OUT

#$BIN/scm $SCORIES/persons0.ss > OUT 2> /dev/null
#diff -q -U2 OUTPERSONS0 OUT

#$BIN/scm $SCORIES/eg1.ss > OUT 2> /dev/null ==> TODO: fix this one
#diff -q -U2 OUTEG1 OUT

cd $EXAMPLES

$BIN/scm mp1_1.ss > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT1_1.txt $TESTS/OUT

$BIN/cli mp0 mp0_1 > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT0 $TESTS/OUT

$BIN/cli mp1 mp1_3 > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT1 $TESTS/OUT

$BIN/cli mp0 mp0_1_scheduled > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT0_scheduled $TESTS/OUT

# Following tests done with Guile
#   due to the behaviour of htables, not exactly the same as Gerbil's output
#
$BIN/cli mp1 mp1_3_scheduled > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT1_scheduled $TESTS/OUT

$BIN/cli mp2 mp2_3_scheduled > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT2_scheduled $TESTS/OUT

$BIN/cli mp2 mp3_1_scheduled > $TESTS/OUT 2> /dev/null
diff -q -U2 $TESTS/OUT3_scheduled $TESTS/OUT

rm -f $TESTS/OUT
