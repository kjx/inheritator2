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


