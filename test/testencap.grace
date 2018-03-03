class foo {
   def x = 4
   def y is public = 3
   method z is confidential { } 

   print "1"
   x
   print "2"
   self.x
   print "3"
   def xx = self
   xx.x
   print "4"
}


foo
