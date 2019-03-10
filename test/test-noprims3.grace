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

def stringLiteral : String = object {
  method ++ (_ : String ) -> String { stringLiteral }
  method size -> Number { numberLiteral }
  method asString -> String { "stringLiteral" }
}


def numberLiteral : Number = object {
  method + (_:Number) -> Number { numberLiteral }
  method - (_:Number) -> Number { numberLiteral }
  method asString -> String { "numberLiteral" }
}



type StringBlock = interface {
  apply -> String
}

type NumberBlock = interface {
  apply -> Number
}

type GenericBlock[[T]] = interface {
  apply -> T
}

def a : StringBlock = object { method apply -> String { "hello" } }

def b : StringBlock = a

def c : GenericBlock[[String]] = a

def y : NumberBlock = a //ERROR

def z : GenericBlock[[Number]] = a //ERROR


method ss( in : String ) -> String { "42" } 

ss("42") 
ss(42) //ERROR

def l1 : Number = 3
def l2 : Number = 3 + 4 

def w1 : Number = a.apply //ERROR

