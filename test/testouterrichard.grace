class out {
   method asString { "out" }
   method m { print "M {self}"} 
   method n { m }
   
   class sup {
     method asString { "sup" }
     method t {n}
   }
}

class in { 
   inherit out
   method asString { "in" }
   class sub {
     inherit sup
     method asString { "sub" }
     t 
     }
}

in.sub
