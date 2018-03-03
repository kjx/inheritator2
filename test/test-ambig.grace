
def o = object { 
   //method foo { "outer foo" } 
   method test {
      print(foo)
      def foo = "inner foo"
      print(foo)
   }

}

o.test
