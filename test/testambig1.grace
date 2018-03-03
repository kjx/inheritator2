trait one { 
  method foo { } 
}
trait two {
  method foo { }
}
trait three {
  use one
  use two
}
trait four {
  method foo { } 
}

class dismissed {
  use three
  use four
}


dismissed.foo
