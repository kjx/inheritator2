type String = interface {
  ++ (_ : String ) -> String
  size -> Number
  asString -> String
}

type Number = interface {
  + (_:Number) -> Number
  - (_:Number) -> Number
  asString -> String
}

def stringLiteral = object {
  method ++ (_ : String ) -> String { stringLiteral }
  method size -> Number { numberLiteral }
  method asString -> String { "stringLiteral" }
}


def numberLiteral = object {
  method + (_:Number) -> Number { numberLiteral }
  method - (_:Number) -> Number { numberLiteral }
  method asString -> String { "numberLiteral" }
}


print "start"
print 1

def a : String = "A"

def b : Number = "A"  //ERROR

type A = interface { 
  x 
  y
}

def o : A = object {   //ERROR
   method x { } 
}

print "x"


def p : A = object {
   method x { }
   method y { } 
}

print "x"

def q : A = o       //ERROR

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

def x : B = object {  //SHOUDL ERROR, DOESNT
  method x -> Number { 1 }
}

print(B)
print(w)
print(x)

print 2
print "done"
