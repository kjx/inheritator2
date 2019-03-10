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



method foo -> String { 4 }   //SHOULD ERROR DOESN'T

//foo //DYNAMIC ERROR

