type Number = interface { +(_) }
type String = interface { ++(_) }

method wantsNumber(x : Number) { print "wantsNumber got {x}" }
method wantsString(x : String) { print "wantsString got {x}" }
method wantsGeneric[[T]](x : T) { print "wants{T} got {x}" }

wantsNumber(3)
wantsString("Hello")
wantsGeneric[[Number]](30)
wantsGeneric[[String]]("Hello")

//wantsString(3)
//wantsNumber("Hello")
//wantsGeneric[[String]](30)
//wantsGeneric[[Number]]("Hello")

method returnsNumber(n) -> Number { n }
method returnsString(s) -> String { s }
method returnsGeneric[[T]](g) -> T { g }

returnsNumber(3)
returnsString("Hello")
//returnsString(3)
//returnsNumber("Hello")

returnsGeneric[[Number]](3)
returnsGeneric[[String]]("Hello")
//returnsGeneric[[String]](3)
//returnsGeneric[[Number]]("Hello")

//don't have blame, so dunno what to do with this
type WantsReturns[[W,R]] = interface {
  apply(_ : W) -> R 
}

