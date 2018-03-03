class super {
  method a { print "A" }
  method b { print "B" }
}

class sub {
  inherit super 
     exclude a
     exclude b
  method c { print "C" } 
}


super.a
super.b
sub.c
sub.a
sub.b
