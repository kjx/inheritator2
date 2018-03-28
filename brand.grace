class square is aSquare {  } 

class rectangle is aRectangle { } 

class brand {
  print "WHOOPEE"
  method iAmBrand { }
  method Type { brandType(self) }
}

class brandType(underlyingBrand) {
  method match(other) {primitiveBrandMatch(underlyingBrand,other)}
}

class And(l,r) {
  method match(other) {
    if (l.match(other))
       then {r.match(other)} 
       else {false}
  }
}


def aSquare = brand 

def aRectangle = brand 

def s = square
def r = rectangle

def SquareType = aSquare.Type
def RectangleType = aRectangle.Type
print "GOING"
print( primitiveBrandMatch(23, 12) )
print( primitiveBrandMatch(23, s) )
print( primitiveBrandMatch(aSquare, 12) )
print( primitiveBrandMatch(aSquare, s) )

print (SquareType.match(12))
print (SquareType.match(s))

print (RectangleType.match(s))
print (RectangleType.match(r))

print "done"



