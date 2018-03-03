class S { 
  method foo { } 
}
class T { 
  method foo { } 
} 
class Z { 
  inherit S 
  use T
}
Z.foo
