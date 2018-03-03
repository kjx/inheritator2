class Outer {
  method iAmOuter { } 
  class Sup { 
    method iAmSup { } 
  }
}

class Sub {
  inherit Outer.Sup
  method iAmSub { } 
}

Sub.iAmSup
