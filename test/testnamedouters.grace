class out {
   def myOuter = self
   method x { print "outer x" } 
   class in {
     method x { print "inner x" }
     x
     myOuter.x
   }
}
out.in
