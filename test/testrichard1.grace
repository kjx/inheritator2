class A {
  
  class Inner {
    method foo {
      bar
    }
  }
  
  method bar {
    return "via A"
  }
}
        

class B {
  inherit A.Inner
}


class C {
  class B2 { inherit B }

  method bar {
    return "via C"
  }
}


print(C.B2.foo)
