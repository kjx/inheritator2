def out = object {
   method asString { "out" }
   method m { print "M {self}"} 
   method n { m }
   
   class sup {
     method asString { "sup" }
     method t {n}
   }
}

def in = object { 
   method asString { "out" }
   class sub {
     inherit out.sup
     method asString { "sub" }
     t 
     }
}

in.sub
