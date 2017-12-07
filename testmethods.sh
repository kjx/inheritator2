mono --debug /Users/kjx/mwh-gre/kernan/Grace/bin/Debug/Grace.exe jcollections.grace | grep method  > M 
sort -b M > MS
egrep '(method|class|type|trait)' jcollections.grace | sed -e 's/class/method/' -e 's/trait/method/' -e 's/type/method/' > N
sort -b N > NS
wc -l M N 
