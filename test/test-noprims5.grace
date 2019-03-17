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


def s : String = 42             //ERROR
def n : Number = "42"           //ERROR

def a : String = "a"
def b : Number = 42

def c : String = "a" ++ "a"
def d : Number = 42 + 42


method foo {
  def s : String = 42   //ERROR
  def n : Number = "42" //ERRORx

  def a : String = "a"
  def b : Number = 42

  def c : String = "a" ++ "a"
  def d : Number = 42 + 42
}

