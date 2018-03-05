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

def d1 : Number = 3
//def d2 : String = 3
def d3 : String = "Hello"
//def d4 : Number = "Hello"


var v1 : Number := 3
// var v2 : String := 3
var v3 : String := "Hello"
// var v4 : Number := "Hello"

v1 := 33
// v1 := "three"
v3 := "one"
// v3 := 1

class box[[T]] {  
  var v : T
  method <-(v' : T) { v := v' }
  method prefix^ -> T { v }
}
print "box"

def b = box
b <- "Hello"
^b
print "Box" 

def bs = box[[String]]
print "Gello"
bs <- "Hello"
print "dot"
^bs
print "BOXBOX"

type Box[[U]] = interface {
  <-(_ : U)
  prefix ^ -> U
}

print "b1b2"
def b1 : Box[[Number]] = box[[Number]]
def b2 : Box[[String]] = box[[String]]

print "b1b1b2b2"
b1 <- 3
//b1 <- "Hello"
b2 <- "Hello"
//b2 <- 3

print "v1v3v1v3"
v1 := ^ b1
// v3 := ^ b1
// v1 := ^ b2
v3 := ^ b2


//test done! 

print "done" 



