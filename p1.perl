# do this
# grep "^import " *.grace | grep as | tr -d /- | perl p1.perl > test.gv
# then this
# dot -Tpdf test.gv  > test.pdf

print "strict digraph {\n";
while (<>) {  
      s/([\w-\/]*)\.grace\:import.*\"([\w-\/]*)\".*as.*$/ $1 -> $2/;
      print $_;
      }
print "}\n";
