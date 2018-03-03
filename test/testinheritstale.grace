def x = object { 
   print "making x"
   method foo { print "x foo" } 
   method asString { print "x" } 
   def block = { print (self) }
}


def y = object { 
  inherit x 
  method asString { print "y" }
}

y.asString
y.block.apply

