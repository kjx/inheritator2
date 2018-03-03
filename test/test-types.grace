class test {
     method m(_:String) { "M" }
     method n -> String { "N" }
}

type TestString = interface {
     m(_:String) 
     n -> String
}

type TestNumber = interface {
     m(_:Number)
     n -> Number
}

type TestFailure = interface {
     x   
}
print( TestString.match(test) )
print( TestNumber.match(test) )
print( TestFailure.match(test) )

