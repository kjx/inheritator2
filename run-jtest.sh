kernan jtest.grace | sed 's/\[[A-Z0-9]*\]/[-]/g' > OUT-jtest.txt
diff EXP-jtest.txt OUT-jtest.txt
