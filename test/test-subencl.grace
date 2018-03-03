def encl = object {
   method x { print "encl.x" }
   class incl {
       method test { x } 
  }
}

def encl2 = object { 
  method x { print "encl2.x" }
  class sub {
     inherit encl.incl 
     method x { print "sub.x" }
  }
}

encl2.sub.test
