#!/bin/bash

KERNAN='mono --debug /Users/kjx/mwh-gre/kernan/Grace/bin/Debug/Grace.exe'

$KERNAN main.grace test/test.grace | sed 's/\[[A-Z0-9]*\]/[-]/g' &> test/OUT-test.txt
diff test/EXP-test.txt test/OUT-test.txt
