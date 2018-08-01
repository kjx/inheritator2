type Number = Unknown

type A = interface {
    m(n:Number) -> Number
    B -> Number
}

def y:A = object {
    method m(n:Number) -> Number {2}
    type B = Number
}



print (y.B)
print (A)
print (A.B)

print "one"
def x: y.B = 3
print "two"
def z: A.B = 3            // this causes run-time error
print "three"
print(x)
print "four"
print(z)

