class one { 
  method asString { "one" }
  method Z { "Z" }
  class two {
    method asString { "two" }
    class three { 
      method asString { "three" } 
      class four {
        method asString { "four"  }
        method test { 
          print (self)
          print (outer.Z)
        } 
}}}}


one.two.three.four.test
