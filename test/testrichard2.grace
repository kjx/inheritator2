class Outer {
  method iamOuter { }
  class Inner {
    method iamInner { } 
  }
}



class Sub {
  inherit Outer.Inner
  method iAmSub { } 
}

Sub
