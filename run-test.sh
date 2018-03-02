kernan3 test.grace | sed 's/\[[A-Z0-9]*\]/[-]/g' > OUT-test.txt
diff EXP-test.txt OUT-test.txt
