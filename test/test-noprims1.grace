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



def a : String = "A"

def b : Number = "A"  //ERROR

type A = interface { 
  x 
  y
}

def o : A = object {   //ERROR
   method x { } 
}



def p : A = object {
   method x { }
   method y { } 
}



def q : A = o       //statically correct,  incorrect dynamically/inferred




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

def x : B = object {  //ERROR
  method x -> Number { 1 }
}

