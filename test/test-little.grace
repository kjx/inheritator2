print "start"
print 1

def a : String = "A"

def b : Number = "A"

type A = interface { 
  x 
  y
}

def o : A = object {
   method x { } 
}

print "x"


def p : A = object {
   method x { }
   method y { } 
}

print "x"

def q : A = o 

print "x"


def r : A = p

def s : A = object {
    method x { }
    method y { }
    method z { } 
}


type B = interface {
  x -> String 
}

def w : B = object {
  method x -> String { "x" }
}

def x : B = object {
  method x -> Number { 1 }
}



print 2
print "done"
